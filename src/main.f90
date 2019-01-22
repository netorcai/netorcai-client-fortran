program test
    use netorcai_client
    use fson

    implicit none

    type(Client) :: cli
    type(fson_value), pointer :: jsonValue

    call cli%init()
    call cli%connect("127.0.0.1", 12345)

    jsonValue => cli%recvJson()
    print *, "received !"
    call cli%sendJson(jsonValue)
    call cli%close()
end program test
