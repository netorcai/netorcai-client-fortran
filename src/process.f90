! Some interesting/fun links:
!     - https://stackoverflow.com/questions/10305689/sockets-programming-gfortran/10306821
!     - https://stackoverflow.com/questions/24726446/how-to-call-a-c-function-in-fortran-and-properly-pass-uint32-t-arguments
!     - http://www.fortran.bcs.org/2015/suggestion_string_handling.pdf
!     - https://stackoverflow.com/questions/50149805/do-i-need-to-flush-named-pipes
!     - https://github.com/lukeasrodgers/fortran-server

module netorcai_process
    use, intrinsic :: iso_c_binding
    use stdc

    implicit none
    private

    type, public :: Process
        integer(c_int) :: pid
        integer(c_int) :: readPipe, writePipe
    contains
        procedure :: start => process_start
        procedure :: readLine => process_readLine
        procedure :: writeLine => process_writeLine
        procedure :: readData => process_readData
        procedure :: writeData => process_writeData
        procedure :: wait => process_wait
        procedure :: kill => process_kill
    end type Process
contains
    ! Start a process and return its pid. Crash on error.
    subroutine process_start(this, command)
        class(Process), intent(out) :: this
        character(len=*), intent(in) :: command
        character(len=10), target :: path
        character(len=3), target :: argv1
        character(len=len(command)+1), target :: argv2
        type(c_ptr), dimension(1:4), target :: argv
        integer(c_int), dimension(1:2), target :: inPipe
        integer(c_int), dimension(1:2), target :: outPipe
        integer(c_int) :: pid, useless
        integer(c_long) :: nullLong

        ! Explanation here:
        ! https://stackoverflow.com/questions/6171552/popen-simultaneous-read-and-write

        useless = stdc_pipe(c_loc(inPipe), 0)
        useless = stdc_pipe(c_loc(outPipe), 0)

        pid = stdc_fork()

        if(pid == 0) then
            ! Child process
            useless = stdc_dup2(inPipe(1), STDIN_FILENO)
            useless = stdc_dup2(outPipe(2), STDOUT_FILENO)
            useless = stdc_dup2(outPipe(2), STDERR_FILENO)

            ! Ask kernel to deliver SIGTERM in case the parent dies
            ! Seriously FORTRAN?! It cannot cast an int to a long...
            nullLong = int(0, kind=c_long)
            useless = stdc_prctl(PR_SET_PDEATHSIG, int(SIGTERM, kind=c_long), nullLong, nullLong, nullLong)

            ! Assume bash is installed and is in /bin/bash
            path = C_CHAR_"/bin/bash" // c_null_char
            argv1 = C_CHAR_"-c" // c_null_char
            argv2 = command // c_null_char
            argv(1) = c_loc(path)
            argv(2) = c_loc(argv1)
            argv(3) = c_loc(argv2)
            argv(4) = c_null_ptr

            ! Execute the shell command
            useless = stdc_execv(c_loc(path), c_loc(argv))

            ! Should not be here
            print *, "Error: execl failure"
            stop 1
        elseif(pid > 0) then
            ! Parent process
            useless = stdc_close(inPipe(1))
            useless = stdc_close(outPipe(2))

            this%pid = pid
            this%writePipe = inPipe(2)
            this%readPipe = outPipe(1)
        else
            ! Failure
            print *, "Error: fork() failure"
            stop 1
        end if
    end subroutine process_start

    ! Read a line from the process stdout/stderr.
    ! Return the allocated line read (should be deallocated by the user).
    function process_readLine(this) result(line)
        class(Process), intent(inout) :: this
        character(len=1) :: buffer
        character(len=:), allocatable :: line
        integer :: read

        line = ""

        ! Very inneficient but work...
        do while(.true.)
            read = this%readData(buffer)
            if(read < 0) then
                print *, 'Error: Broken pipe (during read)'
                stop 1
            elseif(read == 0 .or. buffer == achar(10)) then
                return
            else
                line = line // buffer
            end if
        end do
    end function process_readLine

    ! Write a line in the process stdin.
    subroutine process_writeLine(this, line)
        class(Process), intent(inout) :: this
        character(len=*) :: line

        if(this%writeData(line // achar(10)) /= len(line)+1) then
            print *, "Error: Broken pipe (during write)"
            stop 1
        end if
    end subroutine process_writeLine

    ! Read raw data from the process stdout/stderr.
    ! Return the number of bytes read.
    function process_readData(this, buffer) result(res)
        class(Process), intent(inout) :: this
        character(len=*), target :: buffer
        integer :: res

        res = int(stdc_read(this%readPipe, c_loc(buffer), int(len(buffer), kind=c_size_t)))
    end function process_readData

    ! Write raw data to the process stdin.
    ! Return the number of bytes written.
    function process_writeData(this, buffer) result(res)
        class(Process), intent(inout) :: this
        character(len=*), target :: buffer
        integer :: res

        res = int(stdc_write(this%writePipe, c_loc(buffer), int(len(buffer), kind=c_size_t)))
    end function process_writeData

    ! Wait the process pid. Crash on error.
    ! Return the process' status.
    function process_wait(this) result(status)
        class(Process), intent(inout) :: this
        integer(c_int), target :: cStatus
        integer :: status

        if(stdc_waitpid(this%pid, c_loc(cStatus), int(0, kind=c_int)) /= this%pid) then
            print *, "Error: waitpid() failure"
            stop 1
        end if

        status = int(cStatus)
    end function process_wait

    ! Wait the process pid. Crash on error.
    subroutine process_kill(this)
        class(Process), intent(inout) :: this

        if(stdc_kill(this%pid, SIGKILL) < 0) then
            print *, "Error: kill() failure"
            stop 1
        end if
    end subroutine process_kill
end module netorcai_process

