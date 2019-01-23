! Some interesting/fun links:
!     - https://stackoverflow.com/questions/10305689/sockets-programming-gfortran/10306821
!     - https://stackoverflow.com/questions/24726446/how-to-call-a-c-function-in-fortran-and-properly-pass-uint32-t-arguments
!     - http://www.fortran.bcs.org/2015/suggestion_string_handling.pdf
!     - https://github.com/lukeasrodgers/fortran-server

module netorcai_socket
    use, intrinsic :: iso_c_binding
    use stdc

    implicit none
    private

    type, public :: Socket
        integer(c_int) :: sock
    contains
        procedure :: init => socket_init
        procedure :: connect => socket_connect
        procedure :: send => socket_send
        procedure :: recv => socket_recv
        procedure :: send_all => socket_send_all
        procedure :: recv_all => socket_recv_all
        procedure :: close => socket_close
    end type Socket
contains
    ! Used for internal checks
    subroutine check_ssize_t(result, funcName)
        character(len=*), intent(in) :: funcName
        integer(c_size_t), intent(in) :: result
        character(len=:), allocatable, target :: errStr

        if(result < 0) then
            errStr = c_char_"Critical error occured in function " // funcName // c_null_char
            call stdc_perror(c_loc(errStr))
            stop 1 ! standard way to exit a program
        end if
    end subroutine check_ssize_t

    ! Used for internal checks
    subroutine check_int(result, funcName)
        character(len=*), intent(in) :: funcName
        integer(c_int), intent(in) :: result

        ! A ssize_t is always bigger than an int
        ! note the beautiful way to do casts
        call check_ssize_t(int(result, kind=c_size_t), funcName)
    end subroutine check_int

    ! Initialize a Socket object
    subroutine socket_init(this)
        class(Socket), intent(out) :: this

        this%sock = stdc_socket(AF_INET, SOCK_STREAM, IPPROTO_TCP)
        call check_int(this%sock, "socket")
    end subroutine socket_init

    ! Initiate an TCP/IPv4 based connection on a socket
    subroutine socket_connect(this, ip, port)
        class(Socket), intent(inout) :: this
        character(len=*), intent(in) :: ip
        integer, intent(in) :: port
        character(len=len(ip)+1), target :: cstring
        type(c_sockaddr_in), target :: addr
        integer(c_int) :: tmpRes

        if(port < 0 .or. port > 65536) then
            print *, "Invalid port"
            stop 1
        end if

        cstring = ip // c_null_char
        addr%sin_family = int(AF_INET, kind=c_int16_t)
        addr%sin_port = stdc_htons(int(port, kind=c_int16_t))
        tmpRes = stdc_inet_aton(c_loc(cstring), c_loc(addr%sin_addr))

        if(tmpRes == 0) then
            print *, "Invalid IPv4 address"
            stop 1
        end if

        ! Assume addrLen = 16
        call check_int(stdc_connect(this%sock, c_loc(addr), 16), "connect")
    end subroutine socket_connect

    ! Send a maximum of len(buffer) bytes of data from buffer to the network
    ! Return the number of character actually sent
    ! Note: buffer is a string and not an array, but it works better in practice internally...
    function socket_send(this, buffer) result(res)
        class(Socket), intent(inout) :: this
        character(len=*), intent(in), target :: buffer ! TODO: check this
        integer :: imin, imax
        integer(c_size_t) :: tmpLen
        integer(c_size_t) :: tmpRes
        integer :: res

        imin = 1!lbound(buffer, 1)
        imax = len(buffer)!ubound(buffer, 1)

        ! WARNING: FORTRAN array start at 1...
        tmpLen = imax - imin + 1
        tmpRes = stdc_send(this%sock, c_loc(buffer(imin:imax)), tmpLen, 0)
        call check_ssize_t(tmpRes, "send")
        res = int(tmpRes)
    end function socket_send

    ! Receive a maximum of len(buffer) bytes of data from the network into buffer
    ! Return the number of character actually received
    ! Note: buffer is a string and not an array, but it works better in practice internally...
    function socket_recv(this, buffer) result(res)
        class(Socket), intent(inout) :: this
        character(len=*), intent(in), target :: buffer ! TODO: check this
        integer :: imin, imax
        integer(c_size_t) :: tmpLen
        integer(c_size_t) :: tmpRes
        integer :: res

        imin = 1!lbound(buffer, 1)
        imax = len(buffer)!ubound(buffer, 1)

        ! WARNING: FORTRAN array start at 1...
        tmpLen = imax - imin + 1
        tmpRes = stdc_recv(this%sock, c_loc(buffer(imin:imax)), tmpLen, 0)
        call check_ssize_t(tmpRes, "recv")
        res = int(tmpRes)
    end function socket_recv

    ! Send exactly len(buffer) bytes of data from buffer to the network
    ! Crash if data cannot be sent (eg. connection failure)
    ! Note: buffer is a string and not an array, but it works better in practice internally...
    subroutine socket_send_all(this, buffer)
        class(Socket), intent(inout) :: this
        character(len=*), intent(in) :: buffer ! TODO: check this
        integer :: imin, imax
        integer :: sent
        integer :: localSent

        sent = 0
        imin = 1!lbound(buffer, 1)
        imax = len(buffer)!ubound(buffer, 1)

        if(len(buffer) == 0) then
            return
        end if

        do while(imin+sent <= imax)
            localSent = this%send(buffer(imin+sent:imax))

            if(localSent == 0) then
                print *, "Broken connection during transfer (sending data)"
                stop 1
            end if

            sent = sent + localSent
        end do
    end subroutine socket_send_all

    ! Receive exactly len(buffer) bytes of data from the network into buffer
    ! Crash if data cannot be received (eg. connection failure)
    ! Note: buffer is a string and not an array, but it works better in practice internally...
    subroutine socket_recv_all(this, buffer)
        class(Socket), intent(inout) :: this
        character(len=*), intent(in), target :: buffer ! TODO: check this
        integer :: imin, imax
        integer :: received
        integer :: localReceived

        received = 0
        imin = 1!lbound(buffer, 1)
        imax = len(buffer)!ubound(buffer, 1)

        if(len(buffer) == 0) then
            return
        end if

        do while(imin+received <= imax)
            localReceived = this%recv(buffer(imin+received:imax))

            if(localReceived == 0) then
                print *, "Broken connection during transfer (receiving data)"
                stop 1
            end if

            received = received + localReceived
        end do
    end subroutine socket_recv_all

    ! Close the socket descriptor
    subroutine socket_close(this)
        class(Socket), intent(inout) :: this

        call check_int(stdc_close(this%sock), "close")
    end subroutine socket_close
end module netorcai_socket

