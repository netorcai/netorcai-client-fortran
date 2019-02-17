program hello_gl
    use netorcai_message
    use netorcai_client
    use netorcai_json

    implicit none


    ! Default connection parameters
    character(len=*), parameter :: ip = "127.0.0.1"
    integer, parameter :: port = 4242

    type(Client) :: c
    type(LoginAckMessage) :: loginAck
    type(GameStartsMessage) :: gameStarts
    type(TurnMessage) :: turn
    class(JsonDocument), allocatable :: jsonDoc
    integer :: i

    call c%init()

    print *, "Connecting to netorcai..."
    call c%connect(ip, port)
    print *, "done"

    print *, "Logging in as a player..."
    call c%sendLogin("FORTRAN-pl", "player")
    loginAck = c%readLoginAck()
    print *, "done"

    print *, "Waiting for GAME_STARTS..."
    gameStarts = c%readGameStarts()
    call gameStarts%initialGameState%destroy() ! Free struct internal json data
    print *, "done"

    do i = 1, gameStarts%nbTurnsMax-1
        print *, "Waiting for TURN..."
        if(.not. c%readTurn(turn)) exit ! Break the game loop if needed
        call turn%gameState%destroy() ! Free struct internal json data

        jsonDoc = json_parse('[{"player": "FORTRAN"}]')
        call c%sendTurnAck(turn%turnNumber, jsonDoc%getRoot())
        deallocate(jsonDoc)
        print *, "done"
    end do

    call c%close()
end program hello_gl
