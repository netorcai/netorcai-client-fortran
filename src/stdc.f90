! Assumtions:
!     - FORTRAN 2003 compatible
!     - Linux ONLY data structures and constants
!     - types size are OK (eg. socklen_t and s_addr are strict uint32_t)

! Notes:
!     - Do not care about unsigned: C unsigned integers have no equivalent in Fortran
!     - IPv6 not supported

! C-FORTRAN Interoperability:
!     - https://gcc.gnu.org/onlinedocs/gfortran/Interoperable-Subroutines-and-Functions.html
!     - https://gcc.gnu.org/onlinedocs/gcc-6.3.0/gfortran/Working-with-Pointers.html

module stdc
    use, intrinsic :: ISO_C_BINDING

    implicit none

    integer(c_int), parameter :: AF_UNSPEC = 0
    integer(c_int), parameter :: AF_UNIX = 1
    integer(c_int), parameter :: AF_INET = 2
    integer(c_int), parameter :: AF_INET6 = 10

    integer(c_int), parameter :: SOCK_STREAM = 1
    integer(c_int), parameter :: SOCK_DGRAM = 2
    integer(c_int), parameter :: SOCK_RAW = 3
    integer(c_int), parameter :: SOCK_SEQPACKET = 5

    integer(c_int), parameter :: IPPROTO_IP = 0
    integer(c_int), parameter :: IPPROTO_TCP = 6

    integer(c_int), parameter :: SOL_SOCKET = 65535
    integer(c_int), parameter :: SO_REUSEADDR = 4

    !type, bind(c) :: c_sockaddr
    !    integer(c_short) :: sa_family
    !    character(kind=c_char,len=1) :: sa_data ! the compiler say the length MUST be 1...
    !end type c_sockaddr

    type, bind(c) :: c_in_addr
        integer(c_int32_t) :: s_addr
    end type c_in_addr

    ! Should have the size of sockaddr, which is always 16 bytes in practice
    ! There is no standard sizeof operator in FORTRAN 2003...
    ! There is a compiler-dependent sizeof in gfortran (GNU extention)
    ! Since FORTRAN 2008, there is c_sizeof
    ! But in practice c_sizeof cannot be evaluated at compile time...
    type, bind(c) :: c_sockaddr_in
        integer(c_int16_t) :: sin_family
        integer(c_int16_t) :: sin_port
        type(c_in_addr) :: sin_addr
        character(c_char) :: padding(8)
    end type c_sockaddr_in


    ! Missing c_sockaddr_in6 for IPv6 support
    interface
        ! int socket(int domain, int type, int protocol)
        integer(c_int) function stdc_socket(domain, type, protocol) bind(c, name="socket")
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: domain, type, protocol
        end function stdc_socket

        ! int setsockopt(int sockfd, int level, int optname, const void* optval, socklen_t optlen);
        integer(c_int) function stdc_setsockopt(sockfd, level, optname, optval, optlen) bind(c, name='setsockopt')
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: sockfd, level, optname
            type(c_ptr), value :: optval
            integer(c_int32_t), value :: optlen
        end function stdc_setsockopt

        ! int connect(int sockfd, const struct sockaddr* addr, socklen_t addrlen)
        integer(c_int) function stdc_connect(sockfd, addr, addrlen) bind(c, name="connect")
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: sockfd
            type(c_ptr), value :: addr
            integer(c_int32_t), value :: addrlen
        end function stdc_connect

        ! int bind(int sockfd, const struct sockaddr* addr, socklen_t addrlen)
        integer(c_int) function stdc_bind(sockfd, addr, addrlen) bind(c, name='bind')
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: sockfd
            type(c_ptr), value :: addr
            integer(c_int32_t), value :: addrlen
        end function stdc_bind

        ! int listen(int sockfd, int backlog)
        integer(c_int) function stdc_listen(sockfd, backlog) bind(c, name='listen')
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: sockfd, backlog
        end function stdc_listen

        ! int accept(int sockfd, struct sockaddr* addr, socklen_t* addrlen)
        integer(c_int) function stdc_accept(sockfd, addr, addrlen) bind(c, name='accept')
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: sockfd
            type(c_ptr), value :: addr, addrlen
        end function stdc_accept

        ! ssize_t send(int sockfd, const void* buf, size_t len, int flags)
        integer(c_size_t) function stdc_send(sockfd, buf, len, flags) bind(c, name='send')
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: sockfd
            type(c_ptr), value :: buf
            integer(c_size_t), value :: len
            integer(c_int), value :: flags
        end function stdc_send

        ! ssize_t recv(int sockfd, void* buf, size_t len, int flags)
        integer(c_size_t) function stdc_recv(sockfd, buf, len, flags) bind(c, name='recv')
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: sockfd
            type(c_ptr), value :: buf
            integer(c_size_t), value :: len
            integer(c_int), value :: flags
        end function stdc_recv

        ! int close(int filedes)
        integer(c_int) function stdc_close(filedes) bind(c, name="close")
            use, intrinsic :: iso_c_binding
            integer(c_int), value :: filedes
        end function stdc_close

        ! uint16_t htons(uint16_t hostshort)
        integer(c_short) function stdc_htons(hostshort) bind(c, name='htons')
            use, intrinsic :: iso_c_binding
            integer(c_short), value :: hostshort
        end function stdc_htons

        ! uint16_t ntohs(uint16_t hostshort)
        integer(c_short) function stdc_ntohs(netshort) bind(c, name='ntohs')
            use, intrinsic :: iso_c_binding
            integer(c_short), value :: netshort
        end function stdc_ntohs

        ! int inet_aton(const char* cp, struct in_addr* inp)
        integer(c_int) function stdc_inet_aton(cp, inp) bind(c, name='inet_aton')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: cp
            type(c_ptr), value :: inp
        end function stdc_inet_aton

        ! Missing inet_pton for IPv6 support (too complex to bind in FORTRAN)

        ! void perror(const char* s)
        subroutine stdc_perror(s) bind(c, name='perror')
            use, intrinsic :: iso_c_binding
            type(c_ptr), value :: s
        end subroutine stdc_perror
    end interface
end module stdc
