! Notes:
!     - UTF-8 is not supported
!     - Symbolic hostnames and IPv6 are not supported (due to the underlying socket layer)
!     - Assume string static bounds are big enough

module netorcai_client
    use netorcai_socket
    use netorcai_message
    use netorcai_json
    use netorcai_proto_version

    implicit none
    private

    ! Netorcai metaprotocol client class (FORTRAN version)
    type, public :: Client
        type(Socket) :: sock
    contains
        procedure :: init => client_init
        procedure :: close => client_close
        procedure :: connect => client_connect
        procedure :: recvUtf8 => client_recvUtf8
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
        character(len=4) :: contentSizeBuf
        integer :: contentSize, p1, p2, p3, p4

        ! Read content size
        call this%sock%recv_all(contentSizeBuf)

        ! TODO: check endianness
        p1 = int(ichar(contentSizeBuf(1:1)))
        p2 = int(ichar(contentSizeBuf(2:2)))
        p3 = int(ichar(contentSizeBuf(3:3)))
        p4 = int(ichar(contentSizeBuf(4:4)))

        if(p4 /= 0) then
            print *, "Error: protocol parsing error (too big json)"
            stop 1
        end if

        contentSize = p1 + p2 * 256 + p3 * 65536

        allocate(character(contentSize) :: contentBuf)
        call this%sock%recv_all(contentBuf)
    end function client_recvString

    ! Reads and return a string message on the client socket. Crash on error.
    ! Unicode code points that are not ascii are replaced with ?
    ! Return an allocated string that should be deallocated by the user.
    function client_recvUtf8(this) result(res)
        class(Client), intent(inout) :: this
        character(:), allocatable :: res
        character(:), allocatable :: tmpStr
        integer :: i, j, codePoint, codePoints

        i = 1
        codePoints = 0
        tmpStr = this%recvString()

        ! Count code points
        do while(i <= len(tmpStr))
            codePoint = ichar(tmpStr(i:i))

            if(codePoint >= 128) then
                if(codePoint >= 192) then
                    if(codePoint < 224) then
                        i = i + 2
                    else if(codePoint < 240) then
                        i = i + 3
                    else if(codePoint < 248) then
                        i = i + 4
                    else if(codePoint < 256) then
                        print *, 'Error: invalid utf-8 code point found (too long)'
                    else
                        print *, 'Error: internal logic error (bad compiler string encoding)'
                        stop 1
                    end if
                else
                    print *, 'Error: invalid utf-8 code point found (no header)'
                    stop 1
                end if
            else
                i = i + 1
            end if

            codePoints = codePoints + 1
        end do

        allocate(character(len=codePoints) :: res)
        i = 1

        ! Produce an ASCII string containing '?' for each non-ASCII characters
        do j = 1, codePoints
            codePoint = ichar(tmpStr(i:i))

            if(codePoint >= 128) then
                i = i + 2
                if(codePoint >= 224) i = i + 1
                if(codePoint >= 240) i = i + 1
                res(j:j) = '?'
            else
                i = i + 1
                res(j:j) = achar(codePoint)
            end if
        end do
    end function client_recvUtf8

    ! Reads a JSON message on the client socket. Crash on error.
    function client_recvJson(this) result(jsonDoc)
        class(Client), intent(inout) :: this
        type(JsonDocument), allocatable :: jsonDoc

        jsonDoc = json_parse(this%recvUtf8())
    end function client_recvJson

    subroutine client_checkMessageType(jsonMsg, targetMessageTypes)
        class(JsonValue), intent(in) :: jsonMsg
        type(String), dimension(:), intent(in) :: targetMessageTypes
        character(:), allocatable :: messageType
        character(:), allocatable :: kickReason
        integer :: i
        logical :: found

        call jsonMsg%lookup("message_type", messageType)

        if(messageType == "KICK") then
            call jsonMsg%lookup("kick_reason", kickReason)
            print *, "Kicked from netorai. Reason: ", kickReason
            stop 1
        else
            found = .false.

            do i = 1, size(targetMessageTypes)
                found = found .or. messageType == targetMessageTypes(i)%str
            end do

            if(.not. found) then
                print *, "Unexpected message received: ", messageType
                stop 1
            end if
        end if
    end subroutine client_checkMessageType

    ! Reads a LOGIN_ACK message on the client socket. Crash on error.
    function client_readLoginAck(this) result(res)
        class(Client), intent(inout) :: this
        type(LoginAckMessage) :: res
        class(JsonDocument), allocatable :: jsonMsg

        jsonMsg = this%recvJson()
        call client_checkMessageType(jsonMsg%getRoot(), [String("LOGIN_ACK")])
        res = message_parseLoginAckMessage(jsonMsg%getRoot())
    end function client_readLoginAck

    ! Reads a GAME_STARTS message on the client socket. Crash on error.
    function client_readGameStarts(this) result(res)
        class(Client), intent(inout) :: this
        type(GameStartsMessage) :: res
        class(JsonDocument), allocatable :: jsonMsg

        jsonMsg = this%recvJson()
        call client_checkMessageType(jsonMsg%getRoot(), [String("GAME_STARTS")])
        res = message_parseGameStarts(jsonMsg%getRoot())
    end function client_readGameStarts

    ! Reads a TURN message on the client socket. Crash on error.
    ! Return true if the game continue and false else.
    ! Also return if the game should be continued (or game-over otherwise)
    function client_readTurn(this, turn) result(continueGame)
        class(Client), intent(inout) :: this
        type(TurnMessage), intent(out) :: turn
        logical :: continueGame
        class(JsonDocument), allocatable :: jsonMsg
        class(JsonValue), pointer :: jsonRoot
        character(:), allocatable :: messageType

        jsonMsg = this%recvJson()
        jsonRoot => jsonMsg%getRoot()
        call client_checkMessageType(jsonRoot, [String("TURN"), String("GAME_ENDS")])
        call jsonRoot%lookup("message_type", messageType)

        continueGame = messageType == "TURN"

        if(continueGame) then
            turn = message_parseTurn(jsonRoot)
        end if
    end function client_readTurn

    ! Reads a GAME_ENDS message on the client socket. Crash on error.
    function client_readGameEnds(this) result(res)
        class(Client), intent(inout) :: this
        type(GameEndsMessage) :: res
        type(JsonDocument), allocatable :: jsonMsg

        jsonMsg = this%recvJson()
        call client_checkMessageType(jsonMsg%getRoot(), [String("GAME_ENDS")])
        res = message_parseGameEnds(jsonMsg%getRoot())
    end function client_readGameEnds

    ! Reads a DO_INIT message on the client socket. Crash on error.
    function client_readDoInit(this) result(res)
        class(Client), intent(inout) :: this
        type(DoInitMessage) :: res
        class(JsonDocument), allocatable :: jsonMsg

        jsonMsg = this%recvJson()
        call client_checkMessageType(jsonMsg%getRoot(), [String("DO_INIT")])
        res = message_parseDoInit(jsonMsg%getRoot())
    end function client_readDoInit

    ! Reads a DO_TURN message on the client socket. Crash on error.
    function client_readDoTurn(this) result(res)
        class(Client), intent(inout) :: this
        type(DoTurnMessage) :: res
        class(JsonDocument), allocatable :: jsonMsg

        jsonMsg = this%recvJson()
        call client_checkMessageType(jsonMsg%getRoot(), [String("DO_TURN")])
        res = message_parseDoTurn(jsonMsg%getRoot())
    end function client_readDoTurn

    ! Send a string message on the client socket. Crash on error.
    ! TODO: FIXME: buggy function
    subroutine client_sendString(this, message)
        class(Client), intent(inout) :: this
        character(len=*), intent(in) :: message
        character(len=4) :: contentSizeBuf
        integer :: contentSize

        contentSize = len(message) + 1
        if(contentSize >= 16777216) then
            print *, "Error: content size ", contentSize, " does not fit in 24 bits"
            stop 1
        end if

        ! TODO: check endianess
        contentSizeBuf(1:1) = char(mod(contentSize, 256))
        contentSizeBuf(2:2) = char(mod(contentSize / 256, 256))
        contentSizeBuf(3:3) = char(mod(contentSize / 65536, 256))
        contentSizeBuf(4:4) = char(0)

        call this%sock%send_all(contentSizeBuf)
        call this%sock%send_all(message)
        call this%sock%send_all(achar(10))
    end subroutine client_sendString

    ! Send a JSON message on the client socket. Crash on error.
    subroutine client_sendJson(this, message)
        class(Client), intent(inout) :: this
        class(JsonValue), intent(in) :: message
        character(len=:), allocatable :: jsonStr

        jsonStr = message%toString()
        call this%sendString(jsonStr)
    end subroutine client_sendJson

    ! Send a LOGIN message on the client socket. Crash on error.
    subroutine client_sendLogin(this, nickname, role)
        class(Client), intent(inout) :: this
        character(len=*), intent(in) :: nickname
        character(len=*), intent(in) :: role
        class(JsonObject), pointer :: msg

        msg => json_makeObject()
        call msg%add("message_type", json_makeString("LOGIN"))
        call msg%add("nickname", json_makeString(nickname))
        call msg%add("role", json_makeString(role))
        call msg%add("metaprotocol_version", json_makeString(metaprotocolVersion))

        call this%sendJson(msg)
        call msg%destroy()
    end subroutine client_sendLogin

    ! Send a TURN_ACK message on the client socket. Crash on error.
    subroutine client_sendTurnAck(this, turnNumber, actions)
        class(Client), intent(inout) :: this
        integer, intent(in) :: turnNumber
        class(JsonValue), intent(in) :: actions
        class(JsonObject), pointer :: msg

        msg => json_makeObject()
        call msg%add("message_type", json_makeString("TURN_ACK"))
        call msg%add("turn_number", json_makeInt(turnNumber))
        call msg%add("actions", actions%clone())

        call this%sendJson(msg)
        call msg%destroy()
    end subroutine client_sendTurnAck

    ! Send a DO_INIT_ACK message on the client socket. Crash on error.
    subroutine client_sendDoInitAck(this, initialGameState)
        class(Client), intent(inout) :: this
        class(JsonValue), intent(in) :: initialGameState
        class(JsonObject), pointer :: msg

        msg => json_makeObject()
        call msg%add("message_type", json_makeString("DO_INIT_ACK"))
        call msg%add("initial_game_state", initialGameState%clone())

        call this%sendJson(msg)
        call msg%destroy()
    end subroutine client_sendDoInitAck

    ! Send a DO_TURN_ACK message on the client socket. Crash on error.
    subroutine client_sendDoTurnAck(this, gameState, winnerPlayerID)
        class(Client), intent(inout) :: this
        class(JsonValue), intent(in) :: gameState
        integer, intent(in) :: winnerPlayerID
        class(JsonObject), pointer :: msg

        msg => json_makeObject()
        call msg%add("message_type", json_makeString("DO_TURN_ACK"))
        call msg%add("winner_player_id", json_makeInt(winnerPlayerID))
        call msg%add("game_state", gameState%clone())

        call this%sendJson(msg)
        call msg%destroy()
    end subroutine client_sendDoTurnAck
end module netorcai_client

