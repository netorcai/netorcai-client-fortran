! Interface of the fson library
! Add extensions to create and serialize json documents (missing in fson)
module netorcai_json
    use fson, only: fson_value, fson_parse, fson_get, fson_destroy
    use fson_value_m
    use fson_string_m
    use netorcai_utils, only: utils_strReplace

    implicit none
    private

    ! publicly export useful functions of fson
    public :: fson_value
    public :: fson_parse
    public :: fson_get
    public :: fson_destroy
    public :: fson_value_count
    public :: fson_value_get

    ! Fun note: if lines are too long, FORTRAN fail at compile time...
    public :: fson_value_toString
    public :: fson_value_copy
    public :: fson_value_create_struct
    public :: fson_value_add
    public :: fson_value_add_pair
    public :: fson_value_create_null
    public :: fson_value_create_array
    public :: fson_value_create_string
    public :: fson_value_create_int
    public :: fson_value_create_long
    public :: fson_value_create_float
    public :: fson_value_create_double
    public :: fson_value_create_logical
contains
    ! Serialize a json document: convert it to a string
    ! Return an allocated string that should be deallocated by the user.
    ! Funny notes:
    !     - trim(adjustl(s)) is needed for really trim a string (trim is only for the right)
    !     - Static strings have to be trim like in MATLAB which also cause issue with withspaces...
    recursive function fson_value_toString(this) result(jsonStr)
        type(fson_value), pointer :: this, element
        character(len=:), allocatable :: jsonStr
        character(len=1024) :: tmpStr1 ! Name and string values should not be too big... 
        character(len=:), allocatable :: tmpStr2
        integer :: i, count

        jsonStr = ""

        select case(this%value_type)
            case(TYPE_OBJECT)
                jsonStr = jsonStr // "{"
                count = fson_value_count(this)
                element => this%children
                do i = 1, count
                    call fson_string_copy(element%name, tmpStr1)
                    tmpStr2 = utils_strReplace(tmpStr1, '"', '\"')
                    jsonStr = jsonStr // '"' // trim(tmpStr2) // '": '
                    deallocate(tmpStr2)
                    tmpStr2 = fson_value_toString(element)
                    jsonStr = jsonStr // tmpStr2
                    deallocate(tmpStr2)
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
                    tmpStr2 = fson_value_toString(element)
                    jsonStr = jsonStr // tmpStr2
                    deallocate(tmpStr2)
                    if (i < count) then
                        jsonStr = jsonStr // ", "
                    end if
                    element => element%next
                end do
                jsonStr = jsonStr // "]"
            case(TYPE_NULL)
                jsonStr = jsonStr // "null"
            case (TYPE_STRING)
                call fson_string_copy(this%value_string, tmpStr1)
                tmpStr2 = utils_strReplace(tmpStr1, '"', '\"')
                jsonStr = jsonStr // '"' // trim(tmpStr2) // '"'
                deallocate(tmpStr2)
            case(TYPE_LOGICAL)
                if(this%value_logical) then
                    jsonStr = jsonStr // "true"
                else
                    jsonStr = jsonStr // "false"
                end if
            case(TYPE_INTEGER)
                write(tmpStr1, *) this%value_long_integer
                jsonStr = jsonStr // trim(adjustl(tmpStr1))
            case(TYPE_REAL)
                write(tmpStr1, *) this%value_double
print *, "VALUE=<", trim(adjustl(tmpStr1)), ">"
                jsonStr = jsonStr // trim(adjustl(tmpStr1))
        end select
    end function fson_value_toString

    ! Copy recursively the json structure.
    ! Useful to create new json document and better manage their destruction. 
    recursive function fson_value_copy(this) result(jsonNodeCopy)
        type(fson_value), pointer :: this, element
        type(fson_value), pointer :: jsonNodeCopy
        character(len=1024) :: tmpStr ! Name and string values should not be too big... 
        integer :: i, count

        select case(this%value_type)
            case(TYPE_OBJECT)
                jsonNodeCopy => fson_value_create_struct()
                count = fson_value_count(this)
                element => this%children
                do i = 1, count
                    call fson_string_copy(element%name, tmpStr)
                    call fson_value_add_pair(jsonNodeCopy, tmpStr, fson_value_copy(element))
                    element => element%next
                end do
            case(TYPE_ARRAY)
                jsonNodeCopy => fson_value_create_array()
                count = fson_value_count(this)
                element => this%children
                do i = 1, count
                    call fson_value_add(jsonNodeCopy, fson_value_copy(element))
                    element => element%next
                end do
            case(TYPE_NULL)
                jsonNodeCopy => fson_value_create_null()
            case (TYPE_STRING)
                call fson_string_copy(this%value_string, tmpStr)
                jsonNodeCopy => fson_value_create_string(tmpStr)
            case(TYPE_LOGICAL)
                jsonNodeCopy => fson_value_create_logical(this%value_logical)
            case(TYPE_INTEGER)
                jsonNodeCopy => fson_value_create_long(this%value_long_integer)
            case(TYPE_REAL)
                jsonNodeCopy => fson_value_create_double(this%value_double)
        end select
    end function fson_value_copy

    ! Create and return a new json null node.
    function fson_value_create_null() result(jsonValue)
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_NULL
    end function fson_value_create_null

    ! Create and return a new json struct node.
    ! Struct nodes contain pairs added with fson_value_add_pair.
    function fson_value_create_struct() result(jsonValue)
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_OBJECT
    end function fson_value_create_struct

    ! Add a json pair to an existing struct node
    subroutine fson_value_add_pair(jsonStruct, nodeName, jsonNode)
        type(fson_value), pointer, intent(in) :: jsonStruct
        character(len=*), intent(in) :: nodeName
        type(fson_value), pointer, intent(in) :: jsonNode

        jsonNode%name => fson_string_create()
        call fson_string_append(jsonNode%name, nodeName)
        call fson_value_add(jsonStruct, jsonNode)
    end subroutine fson_value_add_pair

    ! Create and return a new json array node.
    ! Elements can be added with the native fson_value_add
    function fson_value_create_array() result(jsonValue)
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_ARRAY
    end function fson_value_create_array

    ! Create and return a new json string node from value.
    function fson_value_create_string(value) result(jsonValue)
        character(len=*), intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_STRING
        jsonValue%value_string => fson_string_create()
        call fson_string_append(jsonValue%value_string, value)
    end function fson_value_create_string

    ! Create and return a new json integer node from value (32 bits).
    function fson_value_create_int(value) result(jsonValue)
        integer, intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_INTEGER
        ! WTF: why it should be stored in 2 vars ?
        jsonValue%value_integer = value
        jsonValue%value_long_integer = int(value, kind=4)
    end function fson_value_create_int

    ! Create and return a new json integer node from value (64 bits).
    function fson_value_create_long(value) result(jsonValue)
        integer(8), intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_INTEGER
        ! WTF: why it should be stored in 2 vars ?
        jsonValue%value_integer = int(value, kind=4)
        jsonValue%value_long_integer = value
    end function fson_value_create_long

    ! Create and return a new json real node from value (simple precision).
    function fson_value_create_float(value) result(jsonValue)
        real, intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_REAL
        ! WTF: why it should be stored in 2 vars ?
        jsonValue%value_real = value
        jsonValue%value_double = real(value, kind=4)
    end function fson_value_create_float

    ! Create and return a new json real node from value (double precision).
    function fson_value_create_double(value) result(jsonValue)
        double precision, intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_REAL
        ! WTF: why it should be stored in 2 vars ?
        jsonValue%value_real = real(value, kind=4)
        jsonValue%value_double = value
    end function fson_value_create_double

    ! Create and return a new json null node.
    function fson_value_create_logical(value) result(jsonValue)
        logical, intent(in) :: value
        type(fson_value), pointer :: jsonValue

        jsonValue => fson_value_create()
        jsonValue%value_type = TYPE_LOGICAL
        jsonValue%value_logical = value
    end function fson_value_create_logical
end module netorcai_json

