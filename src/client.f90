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

    ! Convert a json to a string
    ! This function is missing in fson...
    ! Return an allocated string.
    ! Funny note: trim(adjustl(s)) is needed for really trim a string (trim is only for the right)
    recursive function fson_value_toString(this) result(jsonStr)
        use fson_value_m
        use fson_string_m
        type(fson_value), pointer :: this, element
        character(len=:), allocatable :: jsonStr
        character(len=:), allocatable :: tmpJsonStr
        character (len = 1024) :: tmpChars
        integer :: i, count

        select case(this % value_type)
            case(TYPE_OBJECT)
                jsonStr = jsonStr // "{"
                count = fson_value_count(this)
                element => this%children
                do i = 1, count
                    ! get the name
                    call fson_string_copy(element % name, tmpChars)
                    ! write the name
                    jsonStr = jsonStr // '"' // trim(tmpChars) // '": '
                    ! recursive write of the element
                    tmpJsonStr = fson_value_toString(element)
                    jsonStr = jsonStr // tmpJsonStr
                    deallocate(tmpJsonStr)
                    ! write the separator if required
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
                    ! recursive write of the element
                    tmpJsonStr = fson_value_toString(element)
                    jsonStr = jsonStr // tmpJsonStr
                    deallocate(tmpJsonStr)
                    ! write the separator if required
                    if (i < count) then
                        jsonStr = jsonStr // ", "
                    end if
                    element => element%next
                end do
                jsonStr = jsonStr // "]"
            case(TYPE_NULL)
                jsonStr = jsonStr // "null"
            case (TYPE_STRING)
                call fson_string_copy(this % value_string, tmpChars)
                jsonStr = jsonStr // '"' // trim(tmpChars) // '"'
            case(TYPE_LOGICAL)
                if(this % value_logical) then
                    jsonStr = jsonStr // "true"
                else
                    jsonStr = jsonStr // "false"
                end if
            case(TYPE_INTEGER)
                write(tmpChars, *) this % value_long_integer
                jsonStr = jsonStr // trim(adjustl(tmpChars))
            case(TYPE_REAL)
                write(tmpChars, *) this % value_double
                jsonStr = jsonStr // trim(adjustl(tmpChars))
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

!    ! Send a LOGIN message on the client socket. Crash on error.
!    void sendLogin(in string nickname, in string role)
!    {
!        JSONValue msg = ["message_type" : "LOGIN", "nickname" : nickname, "role" : role];
!
!        sendJson(msg);
!    }
!
!    ! Send a TURN_ACK message on the client socket. Crash on error.
!    void sendTurnAck(in int turnNumber, in JSONValue actions)
!    {
!        JSONValue msg = ["message_type" : "TURN_ACK"];
!        msg.object["turn_number"] = turnNumber;
!        msg.object["actions"] = actions;
!
!        sendJson(msg);
!    }
!
!    ! Send a DO_INIT_ACK message on the client socket. Crash on error.
!    void sendDoInitAck(in JSONValue initialGameState)
!    {
!        JSONValue msg = ["message_type" : "DO_INIT_ACK"];
!        msg.object["initial_game_state"] = initialGameState;
!
!        sendJson(msg);
!    }
!
!    ! Send a DO_TURN_ACK message on the client socket. Crash on error.
!    void sendDoTurnAck(in JSONValue gameState, in int winnerPlayerID)
!    {
!        JSONValue msg = ["message_type" : "DO_TURN_ACK"];
!        msg.object["winner_player_id"] = winnerPlayerID;
!        msg.object["game_state"] = gameState;
!
!        sendJson(msg);
!    }
end module netorcai_client

