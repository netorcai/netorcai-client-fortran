! Notes:
!     - UTF-8 is not supported
!     - Symbolic hostnames and IPv6 are not supported (due to the underlying socket layer)
!     - Assume string static bounds are big enough

module netorcai_client
    use netorcai_socket
    use netorcai_message
    use netorcai_json

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
        procedure :: recvJson => client_recvJson
        procedure :: readLoginAck => client_readLoginAck
        procedure :: readGameStarts => client_readGameStarts
        procedure :: readTurn => client_readTurn
        procedure :: readGameEnds => client_readGameEnds
        procedure :: readDoInit => client_readDoInit
        procedure :: readDoTurn => client_readDoTurn
        procedure :: sendString => client_sendString
        procedure :: sendJson => client_sendJson
        procedure :: sendLogin => client_sendLogin
        procedure :: sendTurnAck => client_sendTurnAck
        procedure :: sendDoInitAck => client_sendDoInitAck
        procedure :: sendDoTurnAck => client_sendDoTurnAck
    end type Client

    ! Fun note: array of strings must contain strings of the same size in FORTRAN, like in MATLAB
    ! See https://software.intel.com/en-us/forums/intel-visual-fortran-compiler-for-windows/topic/345003
    type String
        character(len=:), allocatable :: str
    end type String
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

    subroutine client_checkMessageType(jsonMsg, targetMessageTypes)
        type(fson_value), pointer :: jsonMsg
        type(String), dimension(:) :: targetMessageTypes
        character(len=256) :: messageType
        character(len=256) :: kickReason
        integer :: i
        logical :: found

        call fson_get(jsonMsg, "message_type", messageType)

        if(trim(messageType) == "KICK") then
            call fson_get(jsonMsg, "kick_reason", kickReason)
            print *, "Kicked from netorai. Reason: ", trim(kickReason)
            STOP 1
        else
            found = .false.

            do i = 1, size(targetMessageTypes)
                if(trim(messageType) == targetMessageTypes(i)%str) then
                    found = .true.
                end if
            end do

            if(.not. found) then
                print *, "Unexpected message received: ", trim(messageType)
                STOP 1
            endif
        end if
    end subroutine client_checkMessageType

    ! Reads a LOGIN_ACK message on the client socket. Crash on error.
    function client_readLoginAck(this) result(res)
        class(Client), intent(inout) :: this
        type(LoginAckMessage) :: res
        type(fson_value), pointer :: jsonMsg

        jsonMsg => this%recvJson()
        call client_checkMessageType(jsonMsg, (/String("LOGIN_ACK")/))
        res = LoginAckMessage()
        call fson_destroy(jsonMsg)
    end function client_readLoginAck

    ! Reads a GAME_STARTS message on the client socket. Crash on error.
    function client_readGameStarts(this) result(res)
        class(Client), intent(inout) :: this
        type(GameStartsMessage) :: res
        type(fson_value), pointer :: jsonMsg

        jsonMsg => this%recvJson()
        call client_checkMessageType(jsonMsg, (/String("GAME_STARTS")/))
        res = message_parseGameStarts(jsonMsg)
        call fson_destroy(jsonMsg)
    end function client_readGameStarts

    ! Reads a TURN message on the client socket. Crash on error.
    ! Return true if the game continue and false else.
    ! Also return 
    function client_readTurn(this, turn) result(continueGame)
        class(Client), intent(inout) :: this
        type(TurnMessage), intent(out) :: turn
        logical :: continueGame
        type(fson_value), pointer :: jsonMsg
        character(len=256) :: messageType

        jsonMsg => this%recvJson()
        call client_checkMessageType(jsonMsg, (/String("TURN"), String("GAME_ENDS")/))
        call fson_get(jsonMsg, "message_type", messageType)

        continueGame = trim(messageType) == "TURN"

        if(continueGame) then
            turn = message_parseTurn(jsonMsg)
        end if

        call fson_destroy(jsonMsg)
    end function client_readTurn

    ! Reads a GAME_ENDS message on the client socket. Crash on error.
    function client_readGameEnds(this) result(res)
        class(Client), intent(inout) :: this
        type(GameEndsMessage) :: res
        type(fson_value), pointer :: jsonMsg

        jsonMsg => this%recvJson()
        call client_checkMessageType(jsonMsg, (/String("GAME_ENDS")/))
        res = message_parseGameEnds(jsonMsg)
        call fson_destroy(jsonMsg)
    end function client_readGameEnds

    ! Reads a DO_INIT message on the client socket. Crash on error.
    function client_readDoInit(this) result(res)
        class(Client), intent(inout) :: this
        type(DoInitMessage) :: res
        type(fson_value), pointer :: jsonMsg

        jsonMsg => this%recvJson()
        call client_checkMessageType(jsonMsg, (/String("DO_INIT")/))
        res = message_parseDoInit(jsonMsg)
        call fson_destroy(jsonMsg)
    end function client_readDoInit

    ! Reads a DO_TURN message on the client socket. Crash on error.
    function client_readDoTurn(this) result(res)
        class(Client), intent(inout) :: this
        type(DoTurnMessage) :: res
        type(fson_value), pointer :: jsonMsg

        jsonMsg => this%recvJson()
        call client_checkMessageType(jsonMsg, (/String("DO_TURN")/))
        res = message_parseDoTurn(jsonMsg)
        call fson_destroy(jsonMsg)
    end function client_readDoTurn

    ! Send a string message on the client socket. Crash on error.
    ! TODO: FIXME: buggy function
    subroutine client_sendString(this, message)
        class(Client), intent(inout) :: this
        character(len=*), intent(in) :: message
        character(len=2) :: contentSizeBuf
        integer :: contentSize

        contentSize = len(message) + 1
        if(contentSize >= 65536) then
            print *, "Error: content size ", contentSize, " does not fit in 16 bits"
            stop 1
        end if

        ! TODO: check endianess
        contentSizeBuf(1:1) = char(mod(contentSize, 256))
        contentSizeBuf(2:2) = char(contentSize / 256)

        call this%sock%send_all(contentSizeBuf)
        call this%sock%send_all(message)
        call this%sock%send_all(achar(10))
    end subroutine client_sendString

    ! Send a JSON message on the client socket. Crash on error.
    subroutine client_sendJson(this, message)
        class(Client), intent(inout) :: this
        character(len=:), allocatable :: jsonStr
        type(fson_value), pointer :: message

        jsonStr = fson_value_toString(message)
        call this%sendString(jsonStr)
        deallocate(jsonStr)
    end subroutine client_sendJson

    ! Send a LOGIN message on the client socket. Crash on error.
    subroutine client_sendLogin(this, nickname, role)
        class(Client), intent(inout) :: this
        character(len=*), intent(in) :: nickname
        character(len=*), intent(in) :: role
        type(fson_value), pointer :: msg

        msg => fson_value_create_struct()
        call fson_value_add_pair(msg, "message_type", fson_value_create_string("LOGIN"))
        call fson_value_add_pair(msg, "nickname", fson_value_create_string(nickname))
        call fson_value_add_pair(msg, "role", fson_value_create_string(role))

        call this%sendJson(msg)
        call fson_destroy(msg)
    end subroutine client_sendLogin

    ! Send a TURN_ACK message on the client socket. Crash on error.
    subroutine client_sendTurnAck(this, turnNumber, actions)
        class(Client), intent(inout) :: this
        integer, intent(in) :: turnNumber
        type(fson_value), pointer, intent(in) :: actions
        type(fson_value), pointer :: msg

        msg => fson_value_create_struct()
        call fson_value_add_pair(msg, "message_type", fson_value_create_string("TURN_ACK"))
        call fson_value_add_pair(msg, "turn_number", fson_value_create_integer(turnNumber))
        call fson_value_add_pair(msg, "actions", fson_value_copy(actions))

        call this%sendJson(msg)
        call fson_destroy(msg)
    end subroutine client_sendTurnAck

    ! Send a DO_INIT_ACK message on the client socket. Crash on error.
    subroutine client_sendDoInitAck(this, initialGameState)
        class(Client), intent(inout) :: this
        type(fson_value), pointer, intent(in) :: initialGameState
        type(fson_value), pointer :: msg

        msg => fson_value_create_struct()
        call fson_value_add_pair(msg, "message_type", fson_value_create_string("DO_INIT_ACK"))
        call fson_value_add_pair(msg, "initial_game_state", fson_value_copy(initialGameState))

        call this%sendJson(msg)
        call fson_destroy(msg)
    end subroutine client_sendDoInitAck

    ! Send a DO_TURN_ACK message on the client socket. Crash on error.
    subroutine client_sendDoTurnAck(this, gameState, winnerPlayerID)
        class(Client), intent(inout) :: this
        type(fson_value), pointer, intent(in) :: gameState
        integer, intent(in) :: winnerPlayerID
        type(fson_value), pointer :: msg

        msg => fson_value_create_struct()
        call fson_value_add_pair(msg, "message_type", fson_value_create_string("DO_TURN_ACK"))
        call fson_value_add_pair(msg, "winner_player_id", fson_value_create_integer(winnerPlayerID))
        call fson_value_add_pair(msg, "game_state", fson_value_copy(gameState))

        call this%sendJson(msg)
        call fson_destroy(msg)
    end subroutine client_sendDoTurnAck
end module netorcai_client

