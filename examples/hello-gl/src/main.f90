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
    type(DoInitMessage) :: doInit
    type(DoTurnMessage) :: doTurn
    class(JsonDocument), allocatable :: jsonDoc
    integer :: turn

    call c%init()

    print *, "Connecting to netorcai..."
    call c%connect(ip, port)
    print *, "done"

    print *, "Logging in as a game logic..."
    call c%sendLogin("FORTRAN-gl", "game logic")
    loginAck = c%readLoginAck()
    print *, "done"

    print *, "Waiting for DO_INIT..."
    doInit = c%readDoInit()
    jsonDoc = json_parse('{"all_clients": {"gl": "FORTRAN"}}')
    call c%sendDoInitAck(jsonDoc%getRoot())
    deallocate(jsonDoc)
    print *, "done"

    do turn = 1, doInit%nbTurnsMax
        print *, "Waiting for DO_TURN ", turn, "... "
        doTurn = c%readDoTurn()
        do j = 1, size(doTurn%playerActions)
            call doTurn%playerActions(j)%actions%destroy() ! Free struct internal json data
        end do

        jsonDoc = json_parse('{"all_clients": {"gl": "FORTRAN"}}')
        call c%sendDoTurnAck(jsonDoc%getRoot(), -1)
        deallocate(jsonDoc)
        print *, "done"
    end do

    call c%close()
end program hello_gl
