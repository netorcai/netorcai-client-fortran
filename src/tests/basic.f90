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
            &--nb-turns-max=100 &
            &--nb-players-max=16")

        ! Wait for connection to be possible
        line = netorcaiProcess%readLine()

        ! If cannot be found
        if(index(line, "Listening incoming connections") == 0) then
            print *, "Error: first message of netorcai is not the one expected: " // line
            stop 1
        end if
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
        class(JsonDocument), allocatable :: jsonDoc
        integer :: i, j

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
        jsonDoc = json_parse('{"all_clients": {"gl": "FORTRAN"}}')
        call gameLogic%sendDoInitAck(jsonDoc%getRoot())
        deallocate(jsonDoc)
        gameStarts = player%readGameStarts()
        call gameStarts%initialGameState%destroy() ! Free struct internal json data

        do i = 1, doInit%nbTurnsMax-1
            doTurn = gameLogic%readDoTurn()
            do j = 1, size(doTurn%playerActions)
                call doTurn%playerActions(j)%actions%destroy() ! Free struct internal json data
            end do

            jsonDoc = json_parse('{"all_clients": {"gl": "FORTRAN"}}')
            call gameLogic%sendDoTurnAck(jsonDoc%getRoot(), -1)
            deallocate(jsonDoc)

            if(.not. player%readTurn(turn)) exit ! Break the game loop if needed
            call turn%gameState%destroy() ! Free struct internal json data

            jsonDoc = json_parse('[{"player": "FORTRAN"}]')
            call player%sendTurnAck(turn%turnNumber, jsonDoc%getRoot())
            deallocate(jsonDoc)
        end do

        doTurn = gameLogic%readDoTurn()
        jsonDoc = json_parse('{"all_clients": {"gl": "FORTRAN"}}')
        call gameLogic%sendDoTurnAck(jsonDoc%getRoot(), -1)
        deallocate(jsonDoc)

        gameEnds = player%readGameEnds()
        call gameEnds%gameState%destroy() ! Free struct internal json data

        call gameLogic%close()
        call player%close()

        print *, "Waiting for the netorcai process to end..."
        call test%assert(netorcaiProcess%wait(), 0, "bad process return value")
    end subroutine test_game
end module netorcai_test_basic

