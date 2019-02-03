! Fun notes:
!     - Allocatable behave like unique_ptr in C++ and seems cool but its a trap!
!     - Allocatable classes are totally buggy (apparently both in gfortran and ifort)
!     - Allocatable array are just buggy with ifort < 2019
!     - Returned allocatable variables MUST be filled (crash otherwise) even when the function fail
!     - DO NOT use constructor with structure that contain pointers (init the values, not the pointers)
!     - Returned pointer variables cannot be initialized (otherwise it would be too safe) 
!     - WTF: recursive functions reset its local variables to their init value if set after each call
!         => DO NOT SET VARIABLE IN DECLARATIONS
!     - Pure virtual methods are called "deferred" in FORTRAN and are both uggly and cumbersome
!     - Import is a magic keyword to solve issues with the symbol definition order
!     - class should be used for polymorphic types (YES! in FORTRAN!), type otherwise
!     - class(xxx)-based affectation is not checked by gfortran...

! Json local library
module netorcai_json
    use netorcai_utils
    use netorcai_vector

    implicit none
    private

    ! Load a Json document from a file.
    ! If fail is not set, the function crashes on error.
    public :: json_load

    ! Load a Json document from a string.
    ! If fail is not set, the function crashes on error.
    public :: json_parse

    public :: json_makeNull
    public :: json_makeBool
    public :: json_makeInt
    public :: json_makeLong
    public :: json_makeFloat
    public :: json_makeDouble
    public :: json_makeString
    public :: json_makeArray
    public :: json_makeObject

    ! Helper class to control the memory thanks to scoping
    ! (useful in FORTRAN 2008 mainly)
    type, public :: JsonDocument
        class(JsonValue), pointer, private :: value => null()
    contains
        ! TODO: add a isValid method to replace the fail out argument

        ! Retrieve the root node of the document that enable then further get/set.
        procedure, public :: getRoot => JsonDocument_getRoot

        ! Serialize the whole json document into a string.
        ! Proxy to the JsonValue class. See it for more information.
        procedure, public :: toString => JsonDocument_toString

        ! Clone the whole json document.
        ! Proxy to the JsonValue class. See it for more information.
        procedure, public :: clone => JsonDocument_clone

        ! Save the whole document into a file.
        ! Proxy to the JsonValue class. See it for more information.
        procedure, public :: saveTo => JsonDocument_saveTo

        ! Manual destruction: should not be called unless the destructor is not
        ! automatically called (which should not be the case, but in practice it is...).
        ! Note that this method deallocate its content but not itself, 
        ! which is needed if this is an allocated pointer.
        procedure, public :: destroy => JsonDocument_destroy

        ! Clean all child nodes when destroyed
        final :: JsonDocument_destructor
    end type JsonDocument

    ! Main class of this module: represent an abstract json value.
    type, abstract, public :: JsonValue
    contains
        ! Retrieve the value into a native type
        generic, public :: get => JsonValue_getBool, JsonValue_getInt, &
                                    JsonValue_getLong, JsonValue_getFloat, &
                                    JsonValue_getDouble, JsonValue_getString, &
                                    JsonValue_getArray, JsonValue_getObject

        procedure, private :: JsonValue_getBool
        procedure, private :: JsonValue_getInt
        procedure, private :: JsonValue_getLong
        procedure, private :: JsonValue_getFloat
        procedure, private :: JsonValue_getDouble
        procedure, private :: JsonValue_getString
        procedure, private :: JsonValue_getArray
        procedure, private :: JsonValue_getObject

        ! Lookup the value through the current object and put it into a native type
        generic, public :: lookup => JsonValue_lookupBool, JsonValue_lookupInt, &
                                        JsonValue_lookupLong, JsonValue_lookupFloat, &
                                        JsonValue_lookupDouble, JsonValue_lookupString, &
                                        JsonValue_lookupArray, JsonValue_lookupObject

        procedure, private :: JsonValue_lookupBool
        procedure, private :: JsonValue_lookupInt
        procedure, private :: JsonValue_lookupLong
        procedure, private :: JsonValue_lookupFloat
        procedure, private :: JsonValue_lookupDouble
        procedure, private :: JsonValue_lookupString
        procedure, private :: JsonValue_lookupArray
        procedure, private :: JsonValue_lookupObject

        ! Serialize the value into a string (with its children).
        ! Return an allocated string that should be deallocated by the user.
        procedure(JsonValue_toString), deferred, public :: toString

        ! Clone the value (and its children).
        ! Useful for creating a new json document since no value should be used 
        ! in multiple other value (due to automatic recursive deletion).
        procedure(JsonValue_clone), deferred, public :: clone

        ! Save the value into a file (with its children).
        ! If fail is not set, the function crashes on error.
        procedure, public :: saveTo => JsonValue_saveTo

        ! Deallocate all children and internal structures.
        ! Note that this method deallocate its content but not itself, 
        ! which is needed if this is an allocated pointer.
        procedure, public :: destroy => JsonValue_destroy
    end type JsonValue

    ! For deferred procedures
    abstract interface
        recursive function JsonValue_toString(this) result(res)
            import JsonValue
            class(JsonValue), intent(in) :: this
            character(:), allocatable :: res
        end function JsonValue_toString

        recursive function JsonValue_clone(this) result(res)
            import JsonValue
            class(JsonValue), intent(in) :: this
            class(JsonValue), pointer :: res
        end function JsonValue_clone
    end interface

    type, extends(JsonValue), public :: JsonNull
    contains
        procedure, public :: toString => JsonNull_toString
        procedure, public :: clone => JsonNull_clone
    end type JsonNull

    type, extends(JsonValue), public :: JsonBool
        logical, private :: value
    contains
        procedure, public :: toString => JsonBool_toString
        procedure, public :: clone => JsonBool_clone
    end type JsonBool

    type, extends(JsonValue), public :: JsonInteger
        integer(8), private :: value
    contains
        procedure, public :: toString => JsonInteger_toString
        procedure, public :: clone => JsonInteger_clone
    end type JsonInteger

    type, extends(JsonValue), public :: JsonNumber
        real(8), private :: value
    contains
        procedure, public :: toString => JsonNumber_toString
        procedure, public :: clone => JsonNumber_clone
    end type JsonNumber

    type, extends(JsonValue), public :: JsonString
        character(:), pointer, private :: value
    contains
        procedure, public :: toString => JsonString_toString
        procedure, public :: clone => JsonString_clone
        procedure, public :: destroy => JsonString_destroy
    end type JsonString

    ! Funny note: you cannot declare array of pointer in FORTRAN so you need this...
    type, public :: JsonItem
        class(JsonValue), pointer, public :: value => null()
    end type JsonItem

    type, extends(JsonValue), public :: JsonArray
        type(Vector), private :: value
    contains
        procedure, public :: toString => JsonArray_toString
        procedure, public :: clone => JsonArray_clone
        procedure, public :: destroy => JsonArray_destroy
        procedure, public :: add => JsonArray_add
        procedure, public :: getItem => JsonArray_getItem
        procedure, private :: setItem => JsonArray_setItem
        procedure, public :: size => JsonArray_size
    end type JsonArray

    type, public :: JsonPair
        character(:), pointer, public :: name => null()
        class(JsonValue), pointer, public :: value => null()
    end type JsonPair

    type, extends(JsonValue), public :: JsonObject
        type(Vector), private :: value
    contains
        procedure, public :: toString => JsonObject_toString
        procedure, public :: clone => JsonObject_clone
        procedure, public :: destroy => JsonObject_destroy
        procedure, public :: add => JsonObject_add
        procedure, public :: getItem => JsonObject_getItem
        procedure, private :: setItem => JsonObject_setItem
        procedure, public :: size => JsonObject_size
    end type JsonObject
contains
    function json_load(filename, fail) result(res)
        character(*), intent(in) :: filename
        logical, optional, intent(out) :: fail
        type(JsonDocument), allocatable :: res
        character(:), allocatable :: fileContent

        fileContent = utils_getFileContent(filename, fail)
        if(present(fail) .and. fail) return
        res = json_parse(fileContent, fail)
    end function json_load

    ! Internal function
    function json_expect(jsonStr, prefix, offset) result(found)
        character(*), intent(in) :: jsonStr
        character(*), intent(in) :: prefix
        integer, intent(inout) :: offset
        logical :: found

        found = utils_startsWith(jsonStr, prefix, offset)

        if(found) then
            offset = offset + len(prefix)
        end if
    end function json_expect

    ! Internal function
    subroutine json_skipSpaces(jsonStr, offset)
        character(*), intent(in) :: jsonStr
        integer, intent(inout) :: offset
        integer :: c

        do while(offset <= len(jsonStr))
            c = ichar(jsonStr(offset:offset))

            if(c == 9 .or. c == 10 .or. c == 13 .or. c == 32) then
                offset = offset + 1
            else
                exit
            end if
        end do
    end subroutine json_skipSpaces

    ! Internal function
    ! Note: allow integers that begin with 0
    function json_parseInteger(jsonStr, offset, withPlus, fail) result(res)
        character(*), intent(in) :: jsonStr
        integer, intent(inout) :: offset
        logical, intent(in) :: withPlus
        logical, intent(out) :: fail
        integer(8) :: res, sign
        integer :: tmp

        res = 0
        sign = 1
        fail = .false.

        ! If it is an empty string
        if(offset > len(jsonStr)) then
            fail = .true.
            return
        end if

        if(jsonStr(offset:offset) == '-') then
            sign = -1
            offset = offset + 1
        elseif(withPlus .and. jsonStr(offset:offset) == '+') then
            offset = offset + 1
        end if

        ! If it is an unterminated integer
        if(offset > len(jsonStr)) then
            fail = .true.
            return
        end if

        tmp = ichar(jsonStr(offset:offset))

        ! If it does not start with a digit
        if(tmp < ichar('0') .or. tmp > ichar('9')) then
            fail = .true.
            return
        end if

        do while(offset <= len(jsonStr))
            tmp = ichar(jsonStr(offset:offset))

            ! Overflow
            if(ishft(res, -59) > 0) then
                fail = .true.
                return
            end if

            if(tmp >= ichar('0') .and. tmp <= ichar('9')) then
                res = res * 10 + (tmp - ichar('0'))
                offset = offset + 1
            else
                exit
            end if
        end do

        res = sign * res
    end function json_parseInteger

    ! Internal function
    function json_parseMantissa(jsonStr, offset) result(res)
        character(*), intent(in) :: jsonStr
        integer, intent(inout) :: offset
        real(8) :: res, factor
        integer :: tmp

        res = 0.0_8
        factor = 0.1_8

        do while(offset <= len(jsonStr))
            tmp = ichar(jsonStr(offset:offset))

            if(tmp >= ichar('0') .and. tmp <= ichar('9')) then
                res = res + (tmp - ichar('0')) * factor
                factor = factor * 0.1_8
                offset = offset + 1
            else
                exit
            end if
        end do
    end function json_parseMantissa

    ! Internal function
    function json_parseString(jsonStr, offset, fail) result(res)
        character(*), intent(in) :: jsonStr
        integer, intent(inout) :: offset
        logical, intent(out) :: fail
        character(:), allocatable :: res
        character :: c
        logical :: escape
        integer :: unicodeChar

        if(offset > len(jsonStr) .or. jsonStr(offset:offset) /= '"') then
            fail = .true.
            return
        end if

        res = ""
        escape = .false.
        offset = offset + 1
        fail = .false.

        do
            if(offset > len(jsonStr)) then
                fail = .true.
                return
            end if

            c = jsonStr(offset:offset)
            offset = offset + 1

            if(escape) then
                escape = .false.

                if(c == '"' .or. c == '\' .or. c == '/') then
                    res = res // c
                elseif(c == 'b') then
                    ! WTF, why anyone whould put this in a Json doc?
                    res = res // achar(8)
                elseif(c == 'n') then
                    res = res // achar(10)
                elseif(c == 'r') then
                    res = res // achar(13)
                elseif(c == 't') then
                    res = res // achar(9)
                elseif(c == 'u') then
                    if(offset+3 > len(jsonStr)) then
                        fail = .true.
                        return
                    end if

                    unicodeChar = utils_hexToInt(jsonStr(offset:offset+3), fail)
                    if(fail) return

                    ! Since unicode is not supported by FORTRAN, 
                    ! we just put a '?' character in the string for 
                    ! non-ASCII unicode characters
                    if(unicodeChar >= 0 .and. unicodeChar <= 128) then
                        res = res // achar(unicodeChar)
                    else
                        res = res // '?'
                    end if

                    offset = offset + 3 ! incremented later
                else
                    fail = .true.
                    offset = offset - 1 ! for the error recovery
                    return
                end if
            else
                if(c == '"') then
                    return
                elseif(c == '\') then
                    escape = .true.
                else
                    res = res // c
                end if
            end if
        end do
    end function json_parseString

    ! Internal function
    ! Funny note: slices can be used in FORTRAN, but the copy ellision is an optional optimisation.
    ! To avoid a O(n**2) parsing algorithm, string slices are replaced by the tuple (str, offset)
    recursive function json_llParse(jsonStr, offset, fail) result(res)
        character(*), intent(in) :: jsonStr
        integer, intent(inout) :: offset
        logical, intent(out) :: fail
        class(JsonValue), pointer :: res
        class(JsonArray), pointer :: resArray
        class(JsonObject), pointer :: resObject
        character(:), allocatable :: tmpStr
        class(JsonValue), pointer :: tmpValue
        integer(8) :: tmpInt
        real(8) :: tmpReal

        ! TODO: free memory when error occurs with a GOTO-based error recovery 

        fail = .false.
        res => null()
        resArray => null() ! For debugging purposes
        resObject => null() ! For debugging purposes

        call json_skipSpaces(jsonStr, offset)

        if(offset > len(jsonStr)) then
            fail = .true.
            return
        end if

        select case(jsonStr(offset:offset))
            case('n')
                fail = .not. json_expect(jsonStr, "null", offset)
                if(fail) return
                res => json_makeNull()

            case('t')
                fail = .not. json_expect(jsonStr, "true", offset)
                if(fail) return
                res => json_makeBool(.true.)

            case('f')
                fail = .not. json_expect(jsonStr, "false", offset)
                if(fail) return
                res => json_makeBool(.false.)

            case('"')
                tmpStr = json_parseString(jsonStr, offset, fail)
                if(fail) return
                res => json_makeString(tmpStr)

            case('[')
                offset = offset + 1
                resArray => json_makeArray()
                res => resArray
                call json_skipSpaces(jsonStr, offset)
                if(.not. json_expect(jsonStr, ']', offset)) then
                    do
                        tmpValue => json_llParse(jsonStr, offset, fail)
                        if(fail) return
                        call resArray%add(tmpValue)
                        call json_skipSpaces(jsonStr, offset)

                        if(json_expect(jsonStr, ',', offset)) then
                            cycle
                        elseif(json_expect(jsonStr, ']', offset)) then
                            exit
                        else
                            fail = .true.
                            return
                        end if
                    end do
                end if
                res => resArray

            case('{')
                offset = offset + 1
                resObject => json_makeObject()
                call json_skipSpaces(jsonStr, offset)
                if(.not. json_expect(jsonStr, '}', offset)) then
                    do
                        call json_skipSpaces(jsonStr, offset)
                        tmpStr = json_parseString(jsonStr, offset, fail)
                        if(fail) return
                        call json_skipSpaces(jsonStr, offset)
                        fail = .not. json_expect(jsonStr, ':', offset)
                        if(fail) return
                        tmpValue => json_llParse(jsonStr, offset, fail)
                        if(fail) return
                        call resObject%add(tmpStr, tmpValue)
                        call json_skipSpaces(jsonStr, offset)

                        if(json_expect(jsonStr, ',', offset)) then
                            cycle
                        elseif(json_expect(jsonStr, '}', offset)) then
                            exit
                        else
                            fail = .true.
                            return
                        end if
                    end do
                end if
                res => resObject

            case("-", "0": "9")
                ! TODO fix: allow numbers >= 10 starting with 0 (should not be)
                tmpInt = json_parseInteger(jsonStr, offset, .false., fail)
                if(fail) return
                if(json_expect(jsonStr, '.', offset)) then
                    tmpReal = tmpInt
                    if(tmpInt >= 0) then
                        tmpReal = tmpReal + json_parseMantissa(jsonStr, offset)
                    else
                        tmpReal = tmpReal - json_parseMantissa(jsonStr, offset)
                    end if
                    if(json_expect(jsonStr, 'E', offset) &
                            & .or. json_expect(jsonStr, 'e', offset)) then
                        tmpInt = json_parseInteger(jsonStr, offset, .true., fail)
                        if(fail) return
                        tmpReal = tmpReal * (10.0_8 ** tmpInt)
                    end if
                    res => json_makeDouble(tmpReal)
                else
                    res => json_makeLong(tmpInt)
                end if

            case default
                fail = .true.
                return
        end select
    end function json_llParse

    function json_parse(jsonStr, fail) result(res)
        character(*), intent(in) :: jsonStr
        logical, optional, intent(out) :: fail
        type(JsonDocument), allocatable :: res
        class(JsonValue), pointer :: resPtr
        integer :: offset
        logical :: internalFail

        offset = 1
        resPtr => json_llParse(jsonStr, offset, internalFail)

        ! Fail if there is non-space trailing characters
        call json_skipSpaces(jsonStr, offset)

        ! Internal clean in the case of trailing characters
        if(.not. internalFail .and. offset <= len(jsonStr)) then
            call resPtr%destroy()
            internalFail = .true.
        end if

        if(present(fail)) then
            fail = internalFail
        elseif(internalFail) then
            print *, "Parse error"
            stop 1
        end if

        allocate(res)

        if(.not. internalFail) then
            res%value => resPtr
        else
            res%value => null() ! For debugging purposes
        end if
    end function json_parse

    function json_makeNull() result(res)
        type(JsonNull), pointer :: res

        allocate(res)
    end function json_makeNull

    function json_makeBool(value) result(res)
        logical, intent(in) :: value
        type(JsonBool), pointer :: res

        allocate(res)
        res%value = value
    end function json_makeBool

    function json_makeInt(value) result(res)
        integer(4), intent(in) :: value
        type(JsonInteger), pointer :: res

        allocate(res)
        res%value = value
    end function json_makeInt

    function json_makeLong(value) result(res)
        integer(8), intent(in) :: value
        type(JsonInteger), pointer :: res

        allocate(res)
        res%value = value
    end function json_makeLong

    function json_makeFloat(value) result(res)
        real(4), intent(in) :: value
        type(JsonNumber), pointer :: res

        allocate(res)
        res%value = value
    end function json_makeFloat

    function json_makeDouble(value) result(res)
        real(8), intent(in) :: value
        type(JsonNumber), pointer :: res

        allocate(res)
        res%value = value
    end function json_makeDouble

    function json_makeString(value) result(res)
        character(*), intent(in) :: value
        type(JsonString), pointer :: res

        allocate(res)
        allocate(character(len(value)) :: res%value)
        res%value = value
    end function json_makeString

    function json_makeArray() result(res)
        type(JsonArray), pointer :: res

        allocate(res)
        res%value = Vector()
    end function json_makeArray

    function json_makeObject() result(res)
        type(JsonObject), pointer :: res

        allocate(res)
        res%value = Vector()
    end function json_makeObject

    function JsonDocument_getRoot(this) result(res)
        class(JsonDocument), intent(in) :: this
        class(JsonValue), pointer :: res

        res => this%value
    end function JsonDocument_getRoot

    function JsonDocument_toString(this) result(res)
        class(JsonDocument), intent(in) :: this
        character(:), allocatable :: res

        res = this%value%toString()
    end function JsonDocument_toString

    function JsonDocument_clone(this) result(res)
        class(JsonDocument), intent(in) :: this
        type(JsonDocument), allocatable :: res

        res = JsonDocument(this%value)
    end function JsonDocument_clone

    subroutine JsonDocument_saveTo(this, filename, fail)
        class(JsonDocument), intent(in) :: this
        character(*), intent(in) :: filename
        logical, optional, intent(inout) :: fail

        call this%value%saveTo(filename, fail)
    end subroutine JsonDocument_saveTo

    subroutine JsonDocument_destroy(this)
        class(JsonDocument), intent(inout) :: this

        if(associated(this%value)) then
            call this%value%destroy()
            deallocate(this%value)
            nullify(this%value) ! To disable the destructor
        end if
    end subroutine JsonDocument_destroy

    subroutine JsonDocument_destructor(this)
        type(JsonDocument), intent(inout) :: this

        if(associated(this%value)) then
            call this%value%destroy()
            deallocate(this%value)
            nullify(this%value) ! For debugging purpose
        end if
    end subroutine JsonDocument_destructor

    ! Internal method use for error handling
    subroutine json_type_mismatch(this, paramType, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: paramType
        logical, optional, intent(out) :: fail
        character(:), allocatable :: dynType

        if(present(fail)) then
            fail = .true.
        else
            select type(this)
                type is (JsonNull)
                    dynType = "null type"
                type is (JsonBool)
                    dynType = "logical"
                type is (JsonInteger)
                    dynType = "integer"
                type is (JsonNumber)
                    dynType = "real"
                type is (JsonString)
                    dynType = "character(:), allocatable"
                type is (JsonArray)
                    dynType = "type(JsonItem), dimension(:), allocatable"
                type is (JsonObject)
                    dynType = "type(JsonPair), dimension(:), allocatable"
                class default
                    dynType = "unknown"
            end select

            print *, "Invalid get call: type mismatch between the dynamic json value (", &
                        dynType, ") and procedure parameter (", paramType, ")"
            stop 1
        end if
    end subroutine json_type_mismatch

    ! Internal method use for error handling
    subroutine json_type_ensureObject(this, concreteThis, fail)
        class(JsonValue), target, intent(in) :: this
        class(JsonObject), optional, pointer, intent(out) :: concreteThis
        logical, optional, intent(out) :: fail

        select type(this)
            type is (JsonObject)
                concreteThis => this
                if(present(fail)) then
                    fail = .false.
                end if
            class default
                concreteThis => null() ! For debugging purpose
                if(present(fail)) then
                    fail = .true.
                else
                    print *, "Invalid lookup call: this is not a JsonObject"
                    stop 1
                end if
        end select
    end subroutine json_type_ensureObject

    subroutine JsonValue_getBool(this, value, fail)
        class(JsonValue), target, intent(in) :: this
        logical, intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonBool), pointer :: concreteThis

        select type(this)
            type is (JsonBool)
                if(present(fail)) fail = .false.
                concreteThis => this
                value = concreteThis%value
            class default
                call json_type_mismatch(this, "logical", fail)
        end select
    end subroutine JsonValue_getBool

    subroutine JsonValue_getInt(this, value, fail)
        class(JsonValue), intent(in) :: this
        integer(4), intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonInteger), pointer :: concreteThis

        select type(this)
            type is (JsonInteger)
                if(present(fail)) fail = .false.
                concreteThis => this
                value = int(concreteThis%value, kind=4)
            class default
                call json_type_mismatch(this, "integer(4)", fail)
        end select
    end subroutine JsonValue_getInt

    subroutine JsonValue_getLong(this, value, fail)
        class(JsonValue), intent(in) :: this
        integer(8), intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonInteger), pointer :: concreteThis

        select type(this)
            type is (JsonInteger)
                if(present(fail)) fail = .false.
                concreteThis => this
                value = int(concreteThis%value, kind=8)
            class default
                call json_type_mismatch(this, "integer(8)", fail)
        end select
    end subroutine JsonValue_getLong

    subroutine JsonValue_getFloat(this, value, fail)
        class(JsonValue), intent(in) :: this
        real(4), intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonNumber), pointer :: concreteThis

        select type(this)
            type is (JsonNumber)
                if(present(fail)) fail = .false.
                concreteThis => this
                value = real(concreteThis%value, kind=4)
            class default
                call json_type_mismatch(this, "real(4)", fail)
        end select
    end subroutine JsonValue_getFloat

    subroutine JsonValue_getDouble(this, value, fail)
        class(JsonValue), intent(in) :: this
        real(8), intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonNumber), pointer :: concreteThis

        select type(this)
            type is (JsonNumber)
                if(present(fail)) fail = .false.
                concreteThis => this
                value = real(concreteThis%value, kind=8)
            class default
                call json_type_mismatch(this, "real(8)", fail)
        end select
    end subroutine JsonValue_getDouble

    subroutine JsonValue_getString(this, value, fail)
        class(JsonValue), intent(in) :: this
        character(:), allocatable, intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonString), pointer :: concreteThis

        select type(this)
            type is (JsonString)
                if(present(fail)) fail = .false.
                concreteThis => this
                value = concreteThis%value
            class default
                call json_type_mismatch(this, "character(:), allocatable", fail)
        end select
    end subroutine JsonValue_getString

    subroutine JsonValue_getArray(this, value, fail)
        class(JsonValue), intent(in) :: this
        type(JsonItem), dimension(:), allocatable, intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonArray), pointer :: concreteThis
        integer :: i

        select type(this)
            type is (JsonArray)
                if(present(fail)) fail = .false.
                concreteThis => this
                allocate(value(concreteThis%value%size()))
                do i = 1, concreteThis%value%size()
                    value(i) = concreteThis%getItem(i)
                end do
            class default
                call json_type_mismatch(this, "type(JsonItem), dimension(:), allocatable", fail)
        end select
    end subroutine JsonValue_getArray

    subroutine JsonValue_getObject(this, value, fail)
        class(JsonValue), intent(in) :: this
        type(JsonPair), dimension(:), allocatable, intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        integer :: i

        select type(this)
            type is (JsonObject)
                if(present(fail)) fail = .false.
                concreteThis => this
                allocate(value(concreteThis%value%size()))
                do i = 1, concreteThis%value%size()
                    value(i) = concreteThis%getItem(i)
                end do
            class default
                call json_type_mismatch(this, "type(JsonPair), dimension(:), allocatable", fail)
        end select
    end subroutine JsonValue_getObject

    ! Internal function to find a pair by its key
    ! Return the position of the pair if found, 0 otherwise
    ! Note: it is funny to rewrite such basic functions everytime!
    function json_find_index(object, key) result(res)
        class(JsonObject), intent(in) :: object
        character(*), intent(in) :: key
        type(JsonPair) :: item
        integer :: res
        integer :: i

        res = -1

        do i = 1, object%value%size()
            item = object%getItem(i)
            if(item%name == key) then
                res = i
            end if
        end do
    end function json_find_index

    subroutine json_check_pos_found(pos, key, fail)
        integer, intent(in) :: pos
        character(*), intent(in) :: key
        logical, optional, intent(out) :: fail

        if(pos <= 0) then
            if(present(fail)) then
                fail = .true.
            else
                print *, "Invalid key: """, key, """ not found in this"
                stop 1
            end if
        end if
    end subroutine json_check_pos_found

    subroutine JsonValue_lookupBool(this, key, value, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: key
        logical, intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        type(JsonPair) :: item
        integer :: pos

        call json_type_ensureObject(this, concreteThis, fail)
        if(present(fail) .and. fail) return
        pos = json_find_index(concreteThis, key)
        call json_check_pos_found(pos, key, fail)
        if(present(fail) .and. fail) return
        item = concreteThis%getItem(pos)
        call item%value%get(value, fail)
    end subroutine JsonValue_lookupBool

    subroutine JsonValue_lookupInt(this, key, value, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: key
        integer(4), intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        type(JsonPair) :: item
        integer :: pos

        call json_type_ensureObject(this, concreteThis, fail)
        if(present(fail) .and. fail) return
        pos = json_find_index(concreteThis, key)
        call json_check_pos_found(pos, key, fail)
        if(present(fail) .and. fail) return
        item = concreteThis%getItem(pos)
        call item%value%get(value, fail)
    end subroutine JsonValue_lookupInt

    subroutine JsonValue_lookupLong(this, key, value, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: key
        integer(8), intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        type(JsonPair) :: item
        integer :: pos

        call json_type_ensureObject(this, concreteThis, fail)
        if(present(fail) .and. fail) return
        pos = json_find_index(concreteThis, key)
        call json_check_pos_found(pos, key, fail)
        if(present(fail) .and. fail) return
        item = concreteThis%getItem(pos)
        call item%value%get(value, fail)
    end subroutine JsonValue_lookupLong

    subroutine JsonValue_lookupFloat(this, key, value, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: key
        real(4), intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        type(JsonPair) :: item
        integer :: pos

        call json_type_ensureObject(this, concreteThis, fail)
        if(present(fail) .and. fail) return
        pos = json_find_index(concreteThis, key)
        call json_check_pos_found(pos, key, fail)
        if(present(fail) .and. fail) return
        item = concreteThis%getItem(pos)
        call item%value%get(value, fail)
    end subroutine JsonValue_lookupFloat

    subroutine JsonValue_lookupDouble(this, key, value, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: key
        real(8), intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        type(JsonPair) :: item
        integer :: pos

        call json_type_ensureObject(this, concreteThis, fail)
        if(present(fail) .and. fail) return
        pos = json_find_index(concreteThis, key)
        call json_check_pos_found(pos, key, fail)
        if(present(fail) .and. fail) return
        item = concreteThis%getItem(pos)
        call item%value%get(value, fail)
    end subroutine JsonValue_lookupDouble

    subroutine JsonValue_lookupString(this, key, value, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: key
        character(:), allocatable, intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        type(JsonPair) :: item
        integer :: pos

        call json_type_ensureObject(this, concreteThis, fail)
        if(present(fail) .and. fail) return
        pos = json_find_index(concreteThis, key)
        call json_check_pos_found(pos, key, fail)
        if(present(fail) .and. fail) return
        item = concreteThis%getItem(pos)
        call item%value%get(value, fail)
    end subroutine JsonValue_lookupString

    subroutine JsonValue_lookupArray(this, key, value, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: key
        type(JsonItem), dimension(:), allocatable, intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        type(JsonPair) :: item
        integer :: pos

        call json_type_ensureObject(this, concreteThis, fail)
        if(present(fail) .and. fail) return
        pos = json_find_index(concreteThis, key)
        call json_check_pos_found(pos, key, fail)
        if(present(fail) .and. fail) return
        item = concreteThis%getItem(pos)
        call item%value%get(value, fail)
    end subroutine JsonValue_lookupArray

    subroutine JsonValue_lookupObject(this, key, value, fail)
        class(JsonValue), target, intent(in) :: this
        character(*), intent(in) :: key
        type(JsonPair), dimension(:), allocatable, intent(out) :: value
        logical, optional, intent(out) :: fail
        class(JsonObject), pointer :: concreteThis
        type(JsonPair) :: item
        integer :: pos

        call json_type_ensureObject(this, concreteThis, fail)
        if(present(fail) .and. fail) return
        pos = json_find_index(concreteThis, key)
        call json_check_pos_found(pos, key, fail)
        if(present(fail) .and. fail) return
        item = concreteThis%getItem(pos)
        call item%value%get(value, fail)
    end subroutine JsonValue_lookupObject

    subroutine JsonValue_saveTo(this, filename, fail)
        class(JsonValue), intent(in) :: this
        character(*), intent(in) :: filename
        logical, optional, intent(inout) :: fail

        call utils_setFileContent(filename, this%toString(), fail)
    end subroutine JsonValue_saveTo

    recursive subroutine JsonValue_destroy(this)
        class(JsonValue), intent(inout) :: this

        ! Do nothing
    end subroutine JsonValue_destroy

    recursive function JsonNull_toString(this) result(res)
        class(JsonNull), intent(in) :: this
        character(:), allocatable :: res

        res = "null"
    end function JsonNull_toString

    recursive function JsonNull_clone(this) result(res)
        class(JsonNull), intent(in) :: this
        class(JsonValue), pointer :: res

        allocate(JsonNull :: res)
    end function JsonNull_clone

    recursive function JsonBool_toString(this) result(res)
        class(JsonBool), intent(in) :: this
        character(:), allocatable :: res

        if(this%value) then
            res = "true"
        else
            res = "false"
        end if
    end function JsonBool_toString

    recursive function JsonBool_clone(this) result(res)
        class(JsonBool), intent(in) :: this
        class(JsonBool), pointer :: localRes
        class(JsonValue), pointer :: res

        allocate(JsonBool :: localRes)
        localRes%value = this%value
        res => localRes
    end function JsonBool_clone

    recursive function JsonInteger_toString(this) result(res)
        class(JsonInteger), intent(in) :: this
        character(:), allocatable :: res

        res = utils_longToStr(this%value)
    end function JsonInteger_toString

    recursive function JsonInteger_clone(this) result(res)
        class(JsonInteger), intent(in) :: this
        class(JsonInteger), pointer :: localRes
        class(JsonValue), pointer :: res

        allocate(JsonInteger :: localRes)
        localRes%value = this%value
        res => localRes
    end function JsonInteger_clone

    recursive function JsonNumber_toString(this) result(res)
        class(JsonNumber), intent(in) :: this
        character(:), allocatable :: res

        res = utils_doubleToStr(this%value)
    end function JsonNumber_toString

    recursive function JsonNumber_clone(this) result(res)
        class(JsonNumber), intent(in) :: this
        class(JsonNumber), pointer :: localRes
        class(JsonValue), pointer :: res

        allocate(JsonNumber :: localRes)
        localRes%value = this%value
        res => localRes
    end function JsonNumber_clone

    recursive function JsonString_toString(this) result(res)
        class(JsonString), intent(in) :: this
        character(:), allocatable :: res

        res = '"' // utils_strReplace(this%value, '"', '\"') // '"'
    end function JsonString_toString

    recursive function JsonString_clone(this) result(res)
        class(JsonString), intent(in) :: this
        class(JsonString), pointer :: localRes
        class(JsonValue), pointer :: res

        allocate(JsonString :: localRes)
        allocate(character(len(this%value)) :: localRes%value)
        localRes%value = this%value
        res => localRes
    end function JsonString_clone

    recursive subroutine JsonString_destroy(this)
        class(JsonString), intent(inout) :: this

        deallocate(this%value)
    end subroutine JsonString_destroy

    recursive function JsonArray_toString(this) result(res)
        class(JsonArray), intent(in) :: this
        character(:), allocatable :: res
        type(JsonItem) :: item
        integer :: i

        res = '['

        do i = 1, this%value%size()
            if(i > 1) then
                res = res // ","
            end if

            item = this%getItem(i)
            res = res // item%value%toString()
        end do

        res = res // ']'
    end function JsonArray_toString

    recursive function JsonArray_clone(this) result(res)
        class(JsonArray), intent(in) :: this
        class(JsonArray), pointer :: localRes
        class(JsonValue), pointer :: res
        type(JsonItem) :: oldItem, newItem
        integer :: i

        allocate(JsonArray :: localRes)
        localRes%value = Vector(this%value%size())

        do i = 1, this%value%size()
            oldItem = this%getItem(i)
            newItem%value => oldItem%value%clone()
            call localRes%setItem(i, newItem)
        end do

        res => localRes
    end function JsonArray_clone

    subroutine JsonArray_add(this, value)
        class(JsonArray), intent(inout) :: this
        class(JsonValue), pointer, intent(in) :: value
        type(JsonItem) :: item

        item%value => value
        call this%value%add(transfer(item, void))
    end subroutine JsonArray_add

    function JsonArray_getItem(this, index) result(res)
        class(JsonArray), intent(in) :: this
        integer, intent(in) :: index
        type(JsonItem) :: res, nullItem

        res = transfer(this%value%get(index), nullItem)
    end function JsonArray_getItem

    subroutine JsonArray_setItem(this, index, value)
        class(JsonArray), intent(inout) :: this
        integer, intent(in) :: index
        type(JsonItem), intent(in) :: value

        call this%value%set(index, transfer(value, void))
    end subroutine JsonArray_setItem

    function JsonArray_size(this) result(res)
        class(JsonArray), intent(in) :: this
        integer :: res

        res = this%value%size()
    end function JsonArray_size

    recursive subroutine JsonArray_destroy(this)
        class(JsonArray), intent(inout) :: this
        type(JsonItem) :: item
        integer :: i

        do i = 1, this%value%size()
            item = this%getItem(i)
            call item%value%destroy()
            deallocate(item%value)
        end do

        call this%value%destroy()
    end subroutine JsonArray_destroy

    recursive function JsonObject_toString(this) result(res)
        class(JsonObject), intent(in) :: this
        character(:), allocatable :: res
        character(:), allocatable :: serializedName
        type(JsonPair) :: item
        integer :: i

        res = '{'

        do i = 1, this%value%size()
            if(i > 1) then
                res = res // ","
            end if

            item = this%getItem(i)
            serializedName = '"' // utils_strReplace(item%name, '"', '\"') // '"'
            res = res // serializedName // ':' // item%value%toString()
        end do

        res = res // '}'
    end function JsonObject_toString

    recursive function JsonObject_clone(this) result(res)
        class(JsonObject), intent(in) :: this
        class(JsonObject), pointer :: localRes
        class(JsonValue), pointer :: res
        type(JsonPair) :: oldItem, newItem
        integer :: i

        allocate(JsonObject :: localRes)
        localRes%value = Vector(this%value%size())

        do i = 1, this%value%size()
            oldItem = this%getItem(i)
            allocate(character(len(oldItem%name)) :: newItem%name)
            newItem%name = oldItem%name
            newItem%value => oldItem%value%clone()
            call localRes%setItem(i, newItem)
        end do

        res => localRes
    end function JsonObject_clone

    subroutine JsonObject_add(this, name, value)
        class(JsonObject), intent(inout) :: this
        character(*), intent(in) :: name
        character(:), pointer :: newName
        class(JsonValue), pointer, intent(in) :: value
        type(JsonPair) :: item

        allocate(character(len(name)) :: newName)
        newName = name
        item%name => newName 
        item%value => value
        call this%value%add(transfer(item, void))
    end subroutine JsonObject_add

    function JsonObject_getItem(this, index) result(res)
        class(JsonObject), intent(in) :: this
        integer, intent(in) :: index
        type(JsonPair) :: res, nullPair

        res = transfer(this%value%get(index), nullPair)
    end function JsonObject_getItem

    subroutine JsonObject_setItem(this, index, item)
        class(JsonObject), intent(inout) :: this
        integer, intent(in) :: index
        type(JsonPair), intent(in) :: item

        call this%value%set(index, transfer(item, void))
    end subroutine JsonObject_setItem

    function JsonObject_size(this) result(res)
        class(JsonObject), intent(in) :: this
        integer :: res

        res = this%value%size()
    end function JsonObject_size

    recursive subroutine JsonObject_destroy(this)
        class(JsonObject), intent(inout) :: this
        type(JsonPair) :: item
        integer :: i

        do i = 1, this%value%size()
            item = this%getItem(i)
            deallocate(item%name)
            call item%value%destroy()
            deallocate(item%value)
        end do

        call this%value%destroy()
    end subroutine JsonObject_destroy
end module netorcai_json

