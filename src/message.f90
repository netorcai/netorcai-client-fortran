module netorcai_message
    use netorcai_json

    implicit none
    private

    ! Stores information about one player
    type, public :: PlayerInfo
        integer :: playerID ! The player unique identifier (in [0..nbPlayers[)
        character(len=:), allocatable :: nickname ! The player nickname
        character(len=:), allocatable :: remoteAddress ! The player socket remote address
        logical :: isConnected ! Whether the player is currently connected or not
    end type PlayerInfo

    ! Content of a LOGIN_ACK metaprotocol message
    type, public :: LoginAckMessage
        ! ¯\_(ツ)_/¯
    end type LoginAckMessage

    ! Content of a GAME_STARTS metaprotocol message
    type, public :: GameStartsMessage
        integer :: playerID ! Caller's player identifier. players: [0..nbPlayers[. visu: -1
        integer :: nbPlayers ! Number of players in the game
        integer :: nbTurnsMax ! Maximum number of turns. Game can finish before it
        real :: msBeforeFirstTurn ! Time before the first TURN is sent (in ms)
        real :: msBetweenTurns ! Time between two consecutive TURNs (in ms)
        type(PlayerInfo), dimension(:), allocatable :: playersInfo ! (only for visus) Information about the players
        class(JsonValue), pointer :: initialGameState ! Game-dependent object.
    end type GameStartsMessage

    ! Content of a GAME_ENDS metaprotocol message
    type, public :: GameEndsMessage
        integer :: winnerPlayerID ! Unique identifier of the player that won the game. Or -1.
        class(JsonValue), pointer :: gameState ! Game-dependent object.
    end type GameEndsMessage

    ! Content of a TURN metaprotocol message
    type, public :: TurnMessage
        integer :: turnNumber ! In [0..nbTurnsMax[
        type(PlayerInfo), dimension(:), allocatable :: playersInfo ! (only for visus) Information about the players
        class(JsonValue), pointer :: gameState ! Game-dependent object.
    end type TurnMessage

    ! Content of a DO_INIT metaprotocol message
    type, public :: DoInitMessage
        integer :: nbPlayers ! The number of players of the game
        integer :: nbTurnsMax ! The maximum number of turns of the game
    end type DoInitMessage

    ! Convenient struct for the player actions of a DO_TURN metaprotocol message
    type, public :: PlayerActions
        integer :: playerID ! The identifier of the player that issued the actions
        integer :: turnNumber ! The turn number the actions come from
        class(JsonValue), pointer :: actions ! The actions themselves
    end type PlayerActions

    ! Content of a DO_TURN metaprotocol message
    type, public :: DoTurnMessage
        type(PlayerActions), dimension(:), allocatable :: playerActions ! The ordered list of player actions
    end type DoTurnMessage

    public :: message_parseGameStarts
    public :: message_parseGameEnds
    public :: message_parseTurn
    public :: message_parseDoInit
    public :: message_parseDoTurn
contains
    ! Parses a player information (in GAME_STARTS and GAME_ENDS messages)
    function message_parsePlayerInfo(jsonVal) result(res)
        class(JsonValue), pointer, intent(in) :: jsonVal
        character(:), allocatable :: tmpStr
        type(PlayerInfo) :: res

        ! Fun note: old FORTRAN string works like MATLAB:
        ! fix-sized strings are allocated on the stack and then we have to 
        ! trim them to remove remaining spaces
        call jsonVal%lookup("player_id", res%playerID)
        call jsonVal%lookup("nickname", tmpStr)
        res%nickname = trim(tmpStr)
        call jsonVal%lookup("remote_address", tmpStr)
        res%remoteAddress = trim(tmpStr)
        call jsonVal%lookup("is_connected", res%isConnected)
    end function message_parsePlayerInfo

    ! Parses several player information (in GAME_STARTS and GAME_ENDS messages)
    function message_parsePlayersInfo(jsonVal) result(res)
        class(JsonValue), pointer, intent(in) :: jsonVal
        type(JsonArray), pointer :: jsonArr
        type(PlayerInfo), dimension(:), allocatable :: res
        type(JsonItem) :: item
        integer :: i

        call jsonVal%get(jsonArr)
        allocate(res(jsonArr%size()))

        do i = 1, jsonArr%size()
            item = jsonArr%getItem(i)

            ! WARNING: are pointers copied or their items (causing allocations or a crash)?
            res(i) = message_parsePlayerInfo(item%value)
        end do
    end function message_parsePlayersInfo

    ! Parses a GAME_STARTS metaprotocol message
    function message_parseGameStarts(jsonVal) result(res)
        class(JsonValue), pointer, intent(in) :: jsonVal
        type(GameStartsMessage) :: res
        class(JsonValue), pointer :: jsonPlayersInfos

        call jsonVal%lookup("player_id", res%playerID)
        call jsonVal%lookup("nb_players", res%nbPlayers)
        call jsonVal%lookup("nb_turns_max", res%nbTurnsMax)
        call jsonVal%lookup("milliseconds_before_first_turn", res%msBeforeFirstTurn)
        call jsonVal%lookup("milliseconds_between_turns", res%msBetweenTurns)
        call jsonVal%lookupValue("initial_game_state", res%initialGameState)
        res%initialGameState => res%initialGameState%clone()
        call jsonVal%lookupValue("players_info", jsonPlayersInfos)
        res%playersInfo = message_parsePlayersInfo(jsonPlayersInfos)
    end function message_parseGameStarts

    ! Parses a GAME_ENDS metaprotocol message
    function message_parseGameEnds(jsonVal) result(res)
        class(JsonValue), pointer, intent(in) :: jsonVal
        type(GameEndsMessage) :: res

        call jsonVal%lookup("winner_player_id", res%winnerPlayerID)
        call jsonVal%lookupValue("game_state", res%gameState)
        res%gameState => res%gameState%clone()
    end function message_parseGameEnds

    ! Parses a TURN metaprotocol message
    function message_parseTurn(jsonVal) result(res)
        class(JsonValue), pointer, intent(in) :: jsonVal
        type(TurnMessage) :: res
        class(JsonValue), pointer :: jsonPlayersInfos

        call jsonVal%lookup("turn_number", res%turnNumber)
        call jsonVal%lookupValue("players_info", jsonPlayersInfos)
        res%playersInfo = message_parsePlayersInfo(jsonPlayersInfos)
        call jsonVal%lookupValue("game_state", res%gameState)
        res%gameState => res%gameState%clone()
    end function message_parseTurn

    ! Parses a DO_INIT metaprotocol message
    function message_parseDoInit(jsonVal) result(res)
        class(JsonValue), pointer, intent(in) :: jsonVal
        type(DoInitMessage) :: res

        call jsonVal%lookup("nb_players", res%nbPlayers)
        call jsonVal%lookup("nb_turns_max", res%nbTurnsMax)
    end function message_parseDoInit

    ! Parses the playerActions field of a DO_TURN metaprotocol message
    function message_parsePlayerActions(jsonVal) result(res)
        class(JsonValue), pointer, intent(in) :: jsonVal
        type(PlayerActions) :: res

        call jsonVal%lookup("player_id", res%playerID)
        call jsonVal%lookup("turn_number", res%turnNumber)
        call jsonVal%lookupValue("actions", res%actions)
        res%actions => res%actions%clone()
    end function message_parsePlayerActions

    ! Parses a DO_TURN metaprotocol message
    function message_parseDoTurn(jsonVal) result(res)
        class(JsonValue), pointer, intent(in) :: jsonVal
        type(DoTurnMessage) :: res
        type(JsonArray), pointer :: jsonActions
        type(JsonItem) :: item
        integer :: i

        ! Since FORTRAN pointers are not really first class citizens as in C/C++,
        ! arrays of json value are handled in internal fson types
        call jsonVal%lookup("player_actions", jsonActions)

        allocate(res%playerActions(jsonActions%size()))

        do i = 1, jsonActions%size()
            item = jsonActions%getItem(i)

            ! WARNING: are pointers copied or their items (causing allocations or a crash)?
            res%playerActions(i) = message_parsePlayerActions(item%value)
        end do
    end function message_parseDoTurn
end module netorcai_message

