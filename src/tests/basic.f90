module netorcai_test_basic
    use netorcai_client
    use netorcai_message
    use netorcai_json
    use netorcai_process
    use, intrinsic :: iso_c_binding
    use stdc
    use zofu

    implicit none
    public
contains
    subroutine setup()
        ! Nothing to do
    end subroutine setup

    function launchNetorcaiWaitListening() result(netorcaiProcess)
        type(Process) :: netorcaiProcess
        character(len=:), allocatable :: line

        ! Note: use --fast or else --delay-turns=50
        call netorcaiProcess%start("netorcai &
            &--simple-prompt &
            &--delay-first-turn=50 &
            &--fast &
            &--nb-turns-max=200 &
            &--nb-players-max=16")

        ! Wait for connection to be possible
        line = netorcaiProcess%readLine()

        ! If cannot be found
        if(index(line, "Listening incoming connections") == 0) then
            stop "First message is not the one expected: " // line
        end if

        deallocate(line)
    end function launchNetorcaiWaitListening

    ! Funny note: there is apparently no way to discard a function result in FORTRAN!
    ! Why would anyone try to do that?
    subroutine test_game(test)
        ! Default connection parameters
        character(len=*), parameter :: ip = "127.0.0.1"
        integer, parameter :: port = 4242

        class(unit_test_type), intent(inout) :: test
        type(Process) :: netorcaiProcess
        type(Client) :: gameLogic
        type(Client) :: player
        type(TurnMessage) :: turn
        type(DoInitMessage) :: doInit
        type(LoginAckMessage) :: loginAck
        type(GameStartsMessage) :: gameStarts
        type(DoTurnMessage) :: doTurn
        type(GameEndsMessage) :: gameEnds
        type(fson_value), pointer :: jsonValue
        logical :: endOfGame
        integer :: i

        ! Run netorcai
        netorcaiProcess = launchNetorcaiWaitListening()

        ! Run game logic
        call gameLogic%init()
        call gameLogic%connect(ip, port)
        call gameLogic%sendLogin("gl", "game logic")
        loginAck = gameLogic%readLoginAck()

        ! Run player
        call player%init()
        call player%connect(ip, port)
        call player%sendLogin("player", "player")
        loginAck = player%readLoginAck()

        ! Run game
        call netorcaiProcess%writeLine("start")

        doInit = gameLogic%readDoInit()
        jsonValue => fson_parse(str='{"all_clients": {"gl": "D"}}')
        call gameLogic%sendDoInitAck(jsonValue)
        call fson_destroy(jsonValue)
        gameStarts = player%readGameStarts()

        do i = 1, doInit%nbTurnsMax
            doTurn = gameLogic%readDoTurn()
            jsonValue => fson_parse(str='{"all_clients": {"gl": "D"}}')
            call gameLogic%sendDoTurnAck(jsonValue, -1)
            call fson_destroy(jsonValue)

            turn = player%readTurn(endOfGame)

            if(endOfGame) then
                return
            end if

            jsonValue => fson_parse(str='[{"player": "D"}]')
            call player%sendTurnAck(turn%turnNumber, jsonValue)
            call fson_destroy(jsonValue)
        end do

        doTurn = gameLogic%readDoTurn()
        jsonValue => fson_parse(str='{"all_clients": {"gl": "D"}}')
        call gameLogic%sendDoTurnAck(jsonValue, -1)
        call fson_destroy(jsonValue)

        gameEnds = player%readGameEnds()

        call gameLogic%close()
        call player%close()

        print *, "Waiting for the netorcai process to end..."
        call test%assert(netorcaiProcess%wait() == 0)
    end subroutine test_game
end module netorcai_test_basic

