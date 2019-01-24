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
        type(fson_value), pointer :: initialGameState ! Game-dependent object.
    end type GameStartsMessage

    ! Content of a GAME_ENDS metaprotocol message
    type, public :: GameEndsMessage
        integer :: winnerPlayerID ! Unique identifier of the player that won the game. Or -1.
        type(fson_value), pointer :: gameState ! Game-dependent object.
    end type GameEndsMessage

    ! Content of a TURN metaprotocol message
    type, public :: TurnMessage
        integer :: turnNumber ! In [0..nbTurnsMax[
        type(PlayerInfo), dimension(:), allocatable :: playersInfo ! (only for visus) Information about the players
        type(fson_value), pointer :: gameState ! Game-dependent object.
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
        type(fson_value), pointer :: actions ! The actions themselves
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
    function message_parsePlayerInfo(jsonValue) result(res)
        type(fson_value), pointer, intent(in) :: jsonValue
        character(len=256) :: tmpStr
        type(PlayerInfo) :: res

        ! Fun note: old FORTRAN string works like MATLAB:
        ! fix-sized strings are allocated on the stack and then we have to 
        ! trim them to remove remaining spaces
        call fson_get(jsonValue, "player_id", res%playerID)
        call fson_get(jsonValue, "nickname", tmpStr)
        res%nickname = trim(tmpStr)
        call fson_get(jsonValue, "remote_address", tmpStr)
        res%remoteAddress = trim(tmpStr)
        call fson_get(jsonValue, "is_connected", res%isConnected)
    end function message_parsePlayerInfo

    ! Parses several player information (in GAME_STARTS and GAME_ENDS messages)
    function message_parsePlayersInfo(jsonValue) result(res)
        type(fson_value), pointer, intent(in) :: jsonValue
        type(PlayerInfo), dimension(:), allocatable :: res
        type(fson_value), pointer :: item
        integer :: i

        allocate(res(fson_value_count(jsonValue)))

        do i = 1, fson_value_count(jsonValue)
            item => fson_value_get(jsonValue, i)

            ! WARNING: are pointers copied or their items (causing allocations or a crash)?
            res(i) = message_parsePlayerInfo(item)
        end do
    end function message_parsePlayersInfo

    ! Parses a GAME_STARTS metaprotocol message
    function message_parseGameStarts(jsonValue) result(res)
        type(fson_value), pointer, intent(in) :: jsonValue
        type(GameStartsMessage) :: res
        type(fson_value), pointer :: jsonPlayersInfos

        call fson_get(jsonValue, "player_id", res%playerID)
        call fson_get(jsonValue, "nb_players", res%nbPlayers)
        call fson_get(jsonValue, "nb_turns_max", res%nbTurnsMax)
        call fson_get(jsonValue, "milliseconds_before_first_turn", res%msBeforeFirstTurn)
        call fson_get(jsonValue, "milliseconds_between_turns", res%msBetweenTurns)
        call fson_get(jsonValue, "initial_game_state", res%initialGameState)
        res%initialGameState => fson_value_copy(res%initialGameState)
        call fson_get(jsonValue, "players_info", jsonPlayersInfos)
        res%playersInfo = message_parsePlayersInfo(jsonPlayersInfos)
    end function message_parseGameStarts

    ! Parses a GAME_ENDS metaprotocol message
    function message_parseGameEnds(jsonValue) result(res)
        type(fson_value), pointer, intent(in) :: jsonValue
        type(GameEndsMessage) :: res

        call fson_get(jsonValue, "winner_player_id", res%winnerPlayerID)
        call fson_get(jsonValue, "game_state", res%gameState)
        res%gameState => fson_value_copy(res%gameState)
    end function message_parseGameEnds

    ! Parses a TURN metaprotocol message
    function message_parseTurn(jsonValue) result(res)
        type(fson_value), pointer, intent(in) :: jsonValue
        type(TurnMessage) :: res
        type(fson_value), pointer :: jsonPlayersInfos

        call fson_get(jsonValue, "turn_number", res%turnNumber)
        call fson_get(jsonValue, "players_info", jsonPlayersInfos)
        res%playersInfo = message_parsePlayersInfo(jsonPlayersInfos)
        call fson_get(jsonValue, "game_state", res%gameState)
        res%gameState => fson_value_copy(res%gameState)
    end function message_parseTurn

    ! Parses a DO_INIT metaprotocol message
    function message_parseDoInit(jsonValue) result(res)
        type(fson_value), pointer, intent(in) :: jsonValue
        type(DoInitMessage) :: res

        call fson_get(jsonValue, "nb_players", res%nbPlayers)
        call fson_get(jsonValue, "nb_turns_max", res%nbTurnsMax)
    end function message_parseDoInit

    ! Parses the playerActions field of a DO_TURN metaprotocol message
    function message_parsePlayerActions(jsonValue) result(res)
        type(fson_value), pointer, intent(in) :: jsonValue
        type(PlayerActions) :: res

        call fson_get(jsonValue, "player_id", res%playerID)
        call fson_get(jsonValue, "turn_number", res%turnNumber)
        call fson_get(jsonValue, "actions", res%actions)
        res%actions => fson_value_copy(res%actions)
    end function message_parsePlayerActions

    ! Parses a DO_TURN metaprotocol message
    function message_parseDoTurn(jsonValue) result(res)
        type(fson_value), pointer, intent(in) :: jsonValue
        type(DoTurnMessage) :: res
        type(fson_value), pointer :: jsonActions
        type(fson_value), pointer :: item
        integer :: i

        ! Since FORTRAN pointers are not really first class citizens as in C/C++,
        ! arrays of json value are handled in internal fson types
        call fson_get(jsonValue, "player_actions", jsonActions)

        allocate(res%playerActions(fson_value_count(jsonActions)))

        do i = 1, fson_value_count(jsonActions)
            item => fson_value_get(jsonActions, i)

            ! WARNING: are pointers copied or their items (causing allocations or a crash)?
            res%playerActions(i) = message_parsePlayerActions(item)
        end do
    end function message_parseDoTurn
end module netorcai_message

