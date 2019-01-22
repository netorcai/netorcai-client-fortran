! Notes:
!     - UTF-8 is not supported
!     - Symbolic hostnames and IPv6 are not supported (due to the underlying socket layer)

module netorcai_client
    use netorcai_socket
    use fson

    implicit none
    private

    ! Netorcai metaprotocol client class (FORTRAN version)
    type, public :: Client
        type(Socket) :: sock
    contains
        procedure :: init => client_init
        procedure :: close => client_close
        procedure :: connect => client_connect
        procedure :: recvString => client_recvString
        procedure :: sendString => client_sendString
        procedure :: recvJson => client_recvJson
        procedure :: sendJson => client_sendJson
        procedure :: sendLogin => client_sendLogin
        procedure :: sendTurnAck => client_sendTurnAck
        procedure :: sendDoInitAck => client_sendDoInitAck
        procedure :: sendDoTurnAck => client_sendDoTurnAck
    end type Client
contains
    ! Constructor. Initializes a TCP socket.
    subroutine client_init(this)
        class(Client), intent(out) :: this

        call this%sock%init()
    end subroutine client_init

    ! Destructor. Closes the socket.
    subroutine client_close(this)
        class(Client), intent(inout) :: this

        !call sock%shutdown(BOTH) ! TODO
        call this%sock%close()
    end subroutine client_close

    ! Connect to a remote endpoint. Crash on error.
    subroutine client_connect(this, hostname, port)
        class(Client), intent(inout) :: this
        character(len=*), intent(in) :: hostname
        integer, intent(in) :: port

        call this%sock%connect(hostname, port)
    end subroutine client_connect

    ! Reads and return a string message on the client socket. Crash on error.
    ! Return an allocated string that should be deallocated by the user.
    function client_recvString(this) result(contentBuf)
        class(Client), intent(inout) :: this
        character(len=:), allocatable :: contentBuf
        character(len=2) :: contentSizeBuf
        integer :: contentSize

        ! Read content size
        call this%sock%recv_all(contentSizeBuf)

        ! TODO: check endianness
        contentSize = int(ichar(contentSizeBuf(1:1))) + int(ichar(contentSizeBuf(2:2))) * 256

        allocate(character(contentSize) :: contentBuf)
        call this%sock%recv_all(contentBuf)
    end function client_recvString

    ! Reads a JSON message on the client socket. Crash on error.
    function client_recvJson(this) result(jsonValue)
        class(Client), intent(inout) :: this
        character(len=:), allocatable :: jsonStr
        type(fson_value), pointer :: jsonValue

        jsonStr = this%recvString()
        jsonValue => fson_parse(str=jsonStr)
        deallocate(jsonStr)
    end function client_recvJson

!    ! Reads a LOGIN_ACK message on the client socket. Crash on error.
!    LoginAckMessage readLoginAck()
!    {
!        auto msg = recvJson();
!        switch (msg["message_type"].str)
!        {
!        case "LOGIN_ACK":
!            return LoginAckMessage();
!        case "KICK":
!            throw new Exception(format!"Kicked from netorai. Reason: %s"(msg["kick_reason"].str));
!        default:
!            throw new Exception(format!"Unexpected message received: %s"(msg["message_type"].str));
!        }
!    }
!
!    ! Reads a GAME_STARTS message on the client socket. Crash on error.
!    GameStartsMessage readGameStarts()
!    {
!        auto msg = recvJson;
!        switch (msg["message_type"].str)
!        {
!        case "GAME_STARTS":
!            return parseGameStartsMessage(msg);
!        case "KICK":
!            throw new Exception(format!"Kicked from netorai. Reason: %s"(msg["kick_reason"]));
!        default:
!            throw new Exception(format!"Unexpected message received: %s"(msg["message_type"]));
!        }
!    }
!
!    ! Reads a TURN message on the client socket. Crash on error.
!    TurnMessage readTurn()
!    {
!        auto msg = recvJson;
!        switch (msg["message_type"].str)
!        {
!        case "TURN":
!            return parseTurnMessage(msg);
!        case "GAME_ENDS":
!            throw new Exception("Game over!");
!        case "KICK":
!            throw new Exception(format!"Kicked from netorai. Reason: %s"(msg["kick_reason"]));
!        default:
!            throw new Exception(format!"Unexpected message received: %s"(msg["message_type"]));
!        }
!    }
!
!    ! Reads a GAME_ENDS message on the client socket. Crash on error.
!    GameEndsMessage readGameEnds()
!    {
!        auto msg = recvJson;
!        switch (msg["message_type"].str)
!        {
!        case "GAME_ENDS":
!            return parseGameEndsMessage(msg);
!        case "KICK":
!            throw new Exception(format!"Kicked from netorai. Reason: %s"(msg["kick_reason"]));
!        default:
!            throw new Exception(format!"Unexpected message received: %s"(msg["message_type"]));
!        }
!    }
!
!    ! Reads a DO_INIT message on the client socket. Crash on error.
!    DoInitMessage readDoInit()
!    {
!        auto msg = recvJson;
!        switch (msg["message_type"].str)
!        {
!        case "DO_INIT":
!            return parseDoInitMessage(msg);
!        case "KICK":
!            throw new Exception(format!"Kicked from netorai. Reason: %s"(msg["kick_reason"]));
!        default:
!            throw new Exception(format!"Unexpected message received: %s"(msg["message_type"]));
!        }
!    }
!
!    ! Reads a DO_TURN message on the client socket. Crash on error.
!    DoTurnMessage readDoTurn()
!    {
!        auto msg = recvJson;
!        switch (msg["message_type"].str)
!        {
!        case "DO_TURN":
!            return parseDoTurnMessage(msg);
!        case "KICK":
!            throw new Exception(format!"Kicked from netorai. Reason: %s"(msg["kick_reason"]));
!        default:
!            throw new Exception(format!"Unexpected message received: %s"(msg["message_type"]));
!        }
!    }

    ! Send a string message on the client socket. Crash on error.
    ! TODO: FIXME: buggy function
    subroutine client_sendString(this, message)
        class(Client), intent(inout) :: this
        character(len=*), intent(in) :: message
        character(len=2) :: contentSizeBuf
        integer :: contentSize

        contentSize = len(message) + 1
        if(contentSize >= 65536) then
            write(*,*) "Content size ", contentSize, " does not fit in 16 bits"
            stop 1
        end if

        ! TODO: check endianess
        contentSizeBuf(1:1) = char(mod(contentSize, 256))
        contentSizeBuf(2:2) = char(contentSize / 256)

        call this%sock%send_all(contentSizeBuf)
        call this%sock%send_all(message)
        call this%sock%send_all(achar(10))
    end subroutine client_sendString

    ! Replace all occurences of seekStr by replaceStr in str and return the result
    ! Does not exists in FORTRAN...
    ! Return an allocated string that should be deallocated by the user
    function strReplace(str, seekStr, replaceStr) result(outStr)
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: seekStr
        character(len=*), intent(in) :: replaceStr
        character(len=:), allocatable :: outStr
        integer :: i

        outStr = ""
        i = 1

        ! Naive algorithm (very inefficient)
        do while(i <= len(str))
            if(str(i:min(i+len(seekStr)-1,len(str))) == seekStr) then
                outStr = outStr // replaceStr
                i = i + len(seekStr)
            else
                outStr = outStr // str(i:i)
                i = i + 1
            end if
        end do
    end function strReplace

    ! WARNING: escape string required
    ! Convert a json to a string
    ! This function is missing in fson...
    ! Return an allocated string.
    ! Funny notes:
    !     - trim(adjustl(s)) is needed for really trim a string (trim is only for the right)
    !     - Static strings have to be trim like in MATLAB which also cause issue with withspaces...
    recursive function fson_value_toString(this) result(jsonStr)
        use fson_value_m
        use fson_string_m
        type(fson_value), pointer :: this, element
        character(len=:), allocatable :: jsonStr
        character(len=1024) :: tmpStr1 ! Name and string values should not be too big... 
        character(len=:), allocatable :: tmpStr2
        integer :: i, count

        select case(this % value_type)
            case(TYPE_OBJECT)
                jsonStr = jsonStr // "{"
                count = fson_value_count(this)
                element => this%children
                do i = 1, count
                    call fson_string_copy(element % name, tmpStr1)
                    tmpStr2 = strReplace(tmpStr1, '"', '\"')
                    jsonStr = jsonStr // '"' // trim(tmpStr2) // '": '
                    deallocate(tmpStr2)
                    tmpStr2 = fson_value_toString(element)
                    jsonStr = jsonStr // tmpStr2
                    deallocate(tmpStr2)
                    if (i < count) then
                        jsonStr = jsonStr // ", "
                    end if
                    element => element%next
                end do

                jsonStr = jsonStr // "}"
            case(TYPE_ARRAY)
                jsonStr = jsonStr // "["
                count = fson_value_count(this)
                element => this%children
                do i = 1, count
                    tmpStr2 = fson_value_toString(element)
                    jsonStr = jsonStr // tmpStr2
                    deallocate(tmpStr2)
                    if (i < count) then
                        jsonStr = jsonStr // ", "
                    end if
                    element => element%next
                end do
                jsonStr = jsonStr // "]"
            case(TYPE_NULL)
                jsonStr = jsonStr // "null"
            case (TYPE_STRING)
                call fson_string_copy(this % value_string, tmpStr1)
                tmpStr2 = strReplace(tmpStr1, '"', '\"')
                jsonStr = jsonStr // '"' // trim(tmpStr2) // '"'
                deallocate(tmpStr2)
            case(TYPE_LOGICAL)
                if(this % value_logical) then
                    jsonStr = jsonStr // "true"
                else
                    jsonStr = jsonStr // "false"
                end if
            case(TYPE_INTEGER)
                write(tmpStr1, *) this % value_long_integer
                jsonStr = jsonStr // trim(adjustl(tmpStr1))
            case(TYPE_REAL)
                write(tmpStr1, *) this % value_double
                jsonStr = jsonStr // trim(adjustl(tmpStr1))
        end select
    end function fson_value_toString

    ! Send a JSON message on the client socket. Crash on error.
    subroutine client_sendJson(this, message)
        class(Client), intent(inout) :: this
        character(len=:), allocatable :: jsonStr
        type(fson_value), pointer :: message

        jsonStr = fson_value_toString(message)
        call this%sendString(jsonStr)
        deallocate(jsonStr)
    end subroutine client_sendJson

    ! Missing fson helper function...
    function fson_value_create_struct() result(jsonValue)
        use fson_value_m
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_OBJECT
    end function fson_value_create_struct

    ! Missing fson helper function...
    subroutine fson_value_add_pair(jsonStruct, nodeName, jsonNode)
        use fson_value_m
        use fson_string_m
        type(fson_value), pointer, intent(in) :: jsonStruct
        character(len=*), intent(in) :: nodeName
        type(fson_value), pointer, intent(in) :: jsonNode

        jsonNode%name => fson_string_create()
        call fson_string_append(jsonNode%name, nodeName)
        call fson_value_add(jsonStruct, jsonNode)
    end subroutine fson_value_add_pair

    ! Missing fson helper function...
    function fson_value_create_array() result(jsonValue)
        use fson_value_m
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_ARRAY
    end function fson_value_create_array

    ! Missing fson helper function...
    function fson_value_create_string(value) result(jsonValue)
        use fson_value_m
        use fson_string_m
        character(len=*), intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_STRING
        jsonValue%value_string => fson_string_create()
        call fson_string_append(jsonValue%value_string, value)
    end function fson_value_create_string

    ! Missing fson helper function...
    function fson_value_create_integer(value) result(jsonValue)
        use fson_value_m
        integer, intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_INTEGER
        jsonValue%value_integer = value
    end function fson_value_create_integer

    ! Missing fson helper function...
    function fson_value_create_real(value) result(jsonValue)
        use fson_value_m
        real, intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_INTEGER
        jsonValue%value_real = value
    end function fson_value_create_real

    ! Send a LOGIN message on the client socket. Crash on error.
    subroutine client_sendLogin(this, nickname, role)
        use fson_value_m
        class(Client), intent(inout) :: this
        character(len=*), intent(in) :: nickname
        character(len=*), intent(in) :: role
        type(fson_value), pointer :: msg

        msg => fson_value_create_struct()
        call fson_value_add_pair(msg, "message_type", fson_value_create_string("LOGIN"))
        call fson_value_add_pair(msg, "nickname", fson_value_create_string(nickname))
        call fson_value_add_pair(msg, "role", fson_value_create_string(role))

        call this%sendJson(msg)
        call fson_value_destroy(msg)
    end subroutine client_sendLogin

    ! Send a TURN_ACK message on the client socket. Crash on error.
    subroutine client_sendTurnAck(this, turnNumber, actions)
        use fson_value_m
        class(Client), intent(inout) :: this
        integer, intent(in) :: turnNumber
        type(fson_value), pointer, intent(in) :: actions
        type(fson_value), pointer :: msg

        msg => fson_value_create_struct()
        call fson_value_add_pair(msg, "message_type", fson_value_create_string("TURN_ACK"))
        call fson_value_add_pair(msg, "turn_number", fson_value_create_integer(turnNumber))
        call fson_value_add_pair(msg, "actions", actions)

        call this%sendJson(msg)
        call fson_value_destroy(msg)
    end subroutine client_sendTurnAck

    ! Send a DO_INIT_ACK message on the client socket. Crash on error.
    subroutine client_sendDoInitAck(this, initialGameState)
        use fson_value_m
        class(Client), intent(inout) :: this
        type(fson_value), pointer, intent(in) :: initialGameState
        type(fson_value), pointer :: msg

        msg => fson_value_create_struct()
        call fson_value_add_pair(msg, "message_type", fson_value_create_string("DO_INIT_ACK"))
        call fson_value_add_pair(msg, "initial_game_state", initialGameState)

        call this%sendJson(msg)
        call fson_value_destroy(msg)
    end subroutine client_sendDoInitAck

    ! Send a DO_TURN_ACK message on the client socket. Crash on error.
    subroutine client_sendDoTurnAck(this, gameState, winnerPlayerID)
        use fson_value_m
        class(Client), intent(inout) :: this
        type(fson_value), pointer, intent(in) :: gameState
        integer, intent(in) :: winnerPlayerID
        type(fson_value), pointer :: msg

        msg => fson_value_create_struct()
        call fson_value_add_pair(msg, "message_type", fson_value_create_string("DO_TURN_ACK"))
        call fson_value_add_pair(msg, "winner_player_id", fson_value_create_integer(winnerPlayerID))
        call fson_value_add_pair(msg, "game_state", gameState)

        call this%sendJson(msg)
        call fson_value_destroy(msg)
    end subroutine client_sendDoTurnAck
end module netorcai_client

