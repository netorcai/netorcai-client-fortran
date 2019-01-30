! Fun notes:
!     - Allocatable behave like unique_ptr in C++ and seems cool but its a trap!
!     - Allocatable classes are totally buggy (apparently both in gfortran and ifort)
!     - Allocatable array are just buggy with ifort < 2019
!     - Returned allocatable variables MUST be filled (crash otherwise) even when the function fail
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

    implicit none
    private

    ! Load a Json document from a file.
    ! If fail is not set, the function crashes on error.
    public :: json_load

    ! Load a Json document from a string.
    ! If fail is not set, the function crashes on error.
    public :: json_parse

    ! TODO
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
        class(JsonValue), pointer :: value => null()
    contains
        ! Serialize the whole json document into a string.
        ! Proxy to the JsonValue class. See it for more information.
        procedure :: toString => JsonDocument_toString

        ! Clone the whole json document.
        ! Proxy to the JsonValue class. See it for more information.
        procedure :: clone => JsonDocument_clone

        ! Save the whole document into a file.
        ! Proxy to the JsonValue class. See it for more information.
        procedure :: saveTo => JsonDocument_saveTo

        ! Manual destruction: should not be called unless the destructor is not
        ! automatically called (which should not be the case, but in practice it is...).
        ! Note that this method deallocate its content but not itself, 
        ! which is needed if this is an allocated pointer.
        procedure :: destroy => JsonDocument_destroy

        ! Clean all child nodes when destroyed
        final :: JsonDocument_destructor
    end type JsonDocument

    ! Main class of this module: represent an abstract json value.
    type, abstract, public :: JsonValue
    contains
        ! Serialize the value into a string (with its children).
        ! Return an allocated string that should be deallocated by the user.
        procedure(JsonValue_toString), deferred :: toString

        ! Clone the value (and its children).
        ! Useful for creating a new json document since no value should be used 
        ! in multiple other value (due to automatic recursive deletion).
        procedure(JsonValue_clone), deferred :: clone

        ! Save the value into a file (with its children).
        ! If fail is not set, the function crashes on error.
        procedure :: saveTo => JsonValue_saveTo

        ! Deallocate all children and internal structures.
        ! Note that this method deallocate its content but not itself, 
        ! which is needed if this is an allocated pointer.
        procedure :: destroy => JsonValue_destroy
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
        procedure :: toString => JsonNull_toString
        procedure :: clone => JsonNull_clone
    end type JsonNull

    type, extends(JsonValue), public :: JsonBool
        logical :: value
    contains
        procedure :: toString => JsonBool_toString
        procedure :: clone => JsonBool_clone
    end type JsonBool

    type, extends(JsonValue), public :: JsonInteger
        integer(8) :: value
    contains
        procedure :: toString => JsonInteger_toString
        procedure :: clone => JsonInteger_clone
    end type JsonInteger

    type, extends(JsonValue), public :: JsonNumber
        real(8) :: value
    contains
        procedure :: toString => JsonNumber_toString
        procedure :: clone => JsonNumber_clone
    end type JsonNumber

    type, extends(JsonValue), public :: JsonString
        character(:), allocatable :: value
    contains
        procedure :: toString => JsonString_toString
        procedure :: clone => JsonString_clone
        procedure :: destroy => JsonString_destroy
    end type JsonString

    ! Funny note: you cannot declare array of pointer in FORTRAN so you need this...
    type JsonItem
        class(JsonValue), pointer :: value => null()
    end type JsonItem

    type, extends(JsonValue), public :: JsonArray
        type(JsonItem), dimension(:), allocatable :: value
    contains
        procedure :: toString => JsonArray_toString
        procedure :: clone => JsonArray_clone
        procedure :: destroy => JsonArray_destroy
        procedure :: add => JsonArray_add
    end type JsonArray

    type, public :: JsonPair
        character(:), allocatable :: name
        class(JsonValue), pointer :: value => null()
    end type JsonPair

    type, extends(JsonValue), public :: JsonObject
        type(JsonPair), dimension(:), allocatable :: value
    contains
        procedure :: toString => JsonObject_toString
        procedure :: clone => JsonObject_clone
        procedure :: destroy => JsonObject_destroy
        procedure :: add => JsonObject_add
    end type JsonObject
contains
    function json_load(filename, fail) result(res)
        character(*), intent(in) :: filename
        logical, optional, intent(out) :: fail
        type(JsonDocument) :: res
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
    ! To avoid a O(n**2) parsing algorithm, string slices 
    recursive function json_llParse(jsonStr, offset, fail) result(res)
        character(*), intent(in) :: jsonStr
        integer, intent(inout) :: offset
        logical, intent(out) :: fail
        class(JsonValue), pointer :: res
        class(JsonBool), pointer :: resBool
        class(JsonString), pointer :: resString
        class(JsonInteger), pointer :: resInteger
        class(JsonNumber), pointer :: resNumber
        class(JsonArray), pointer :: resArray
        class(JsonObject), pointer :: resObject
        character(:), allocatable :: tmpStr
        type(JsonItem), pointer :: tmpItem
        type(JsonPair), pointer :: tmpPair
        class(JsonValue), pointer :: tmpValue
        type(JsonItem), dimension(:), pointer :: tmpArray
        type(JsonItem), dimension(:), pointer :: tmpArraySave
        type(JsonPair), dimension(:), pointer :: tmpObject
        type(JsonPair), dimension(:), pointer :: tmpObjectSave
        integer(8) :: tmpInt
        real(8) :: tmpReal

        ! TODO: free memory when error occurs with a GOTO-based error recovery 

        fail = .false.
        res => null()

        ! For debugging purposes
        resBool => null()
        resString => null()
        resInteger => null()
        resNumber => null()
        resArray => null()
        resObject => null()
        tmpItem => null()
        tmpPair => null()
        tmpArray => null()
        tmpArraySave => null()
        tmpObject => null()
        tmpObjectSave => null()

        call json_skipSpaces(jsonStr, offset)

        if(offset > len(jsonStr)) then
            fail = .true.
            return
        end if

        select case(jsonStr(offset:offset))
            case('n')
                fail = .not. json_expect(jsonStr, "null", offset)
                if(fail) return
                allocate(JsonNull :: res)

            case('t')
                fail = .not. json_expect(jsonStr, "true", offset)
                if(fail) return
                allocate(resBool)
                resBool%value = .true.
                res => resBool

            case('f')
                fail = .not. json_expect(jsonStr, "false", offset)
                if(fail) return
                allocate(resBool)
                resBool%value = .false.
                res => resBool

            case('"')
                tmpStr = json_parseString(jsonStr, offset, fail)
                if(fail) return
                allocate(resString)
                call move_alloc(tmpStr, resString%value)
                res => resString

            case('[')
                offset = offset + 1
                allocate(tmpArray(0))
                call json_skipSpaces(jsonStr, offset)
                if(.not. json_expect(jsonStr, ']', offset)) then
                    do
                        tmpValue => json_llParse(jsonStr, offset, fail)
                        if(fail) return
                        allocate(tmpArraySave(size(tmpArray)+1)) ! Needed since FORTRAN copies MUST not alias
                        tmpArraySave = [tmpArray, JsonItem(tmpValue)] ! This is a concatenation in FORTRAN !
                        deallocate(tmpArray)
                        tmpArray => tmpArraySave
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
                allocate(resArray)
                resArray%value = tmpArray
                deallocate(tmpArray)
                res => resArray

            case('{')
                offset = offset + 1
                allocate(tmpObject(0))
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
                        allocate(tmpObjectSave(size(tmpObject)+1))
                        tmpObjectSave = [tmpObject, JsonPair(tmpStr, tmpValue)]
                        deallocate(tmpObject)
                        tmpObject => tmpObjectSave
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
                allocate(resObject)
                allocate(resObject%value(size(tmpObject)))
                resObject%value = tmpObject
                deallocate(tmpObject)
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
                    allocate(resNumber)
                    resNumber%value = tmpReal
                    res => resNumber
                else
                    allocate(resInteger)
                    resInteger%value = tmpInt
                    res => resInteger
                end if

            case default
                fail = .true.
                return
        end select
    end function json_llParse

    function json_parse(jsonStr, fail) result(res)
        character(*), intent(in) :: jsonStr
        logical, optional, intent(out) :: fail
        type(JsonDocument) :: res
        class(JsonValue), pointer :: resPtr
        integer :: offset
        logical :: internalFail

        offset = 1
        resPtr => json_llParse(jsonStr, offset, internalFail)

        ! Fail if there is non-space trailing characters
        call json_skipSpaces(jsonStr, offset)
        internalFail = internalFail .or. offset <= len(jsonStr)

        if(present(fail)) then
            fail = internalFail
        elseif(internalFail) then
            print *, "Parse error"
            stop 1
        end if

        if(.not. internalFail) then
            res%value => resPtr
        else
            ! TODO: call destroy on value (in the case of trailing characters)
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
        res%value = value
    end function json_makeString

    function json_makeArray() result(res)
        type(JsonArray), pointer :: res

        allocate(res)
        allocate(res%value(0))
    end function json_makeArray

    function json_makeObject() result(res)
        type(JsonObject), pointer :: res

        allocate(res)
        allocate(res%value(0))
    end function json_makeObject

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
        localRes%value = this%value
        res => localRes
    end function JsonString_clone

    recursive subroutine JsonString_destroy(this)
        class(JsonString), intent(inout) :: this

        if(allocated(this%value)) then
            deallocate(this%value)
        end if
    end subroutine JsonString_destroy

    recursive function JsonArray_toString(this) result(res)
        class(JsonArray), intent(in) :: this
        character(:), allocatable :: res
        integer :: i

        res = '['

        do i = 1, size(this%value)
            if(i > 1) then
                res = res // ","
            end if

            res = res // this%value(i)%value%toString()
        end do

        res = res // ']'
    end function JsonArray_toString

    recursive function JsonArray_clone(this) result(res)
        class(JsonArray), intent(in) :: this
        class(JsonArray), pointer :: localRes
        class(JsonValue), pointer :: res
        integer :: i

        allocate(JsonArray :: localRes)
        allocate(localRes%value(size(this%value)))

        do i = 1, size(this%value)
            localRes%value(i)%value => this%value(i)%value%clone()
        end do

        res => localRes
    end function JsonArray_clone

    subroutine JsonArray_add(this, value)
        class(JsonArray), intent(inout) :: this
        class(JsonValue), intent(in) :: value
        type(JsonItem), dimension(:), allocatable :: tmp

        ! Innefficient, but simple
        ! The copy prevents aliasing issues
        tmp = [this%value, JsonItem(value)]
        this%value = tmp
    end subroutine JsonArray_add

    recursive subroutine JsonArray_destroy(this)
        class(JsonArray), intent(inout) :: this
        integer :: i

        if(allocated(this%value)) then
            do i = 1, size(this%value)
                call this%value(i)%value%destroy()
                deallocate(this%value(i)%value)
            end do

            deallocate(this%value)
        end if
    end subroutine JsonArray_destroy

    recursive function JsonObject_toString(this) result(res)
        class(JsonObject), intent(in) :: this
        character(:), allocatable :: res
        character(:), allocatable :: serializedName
        integer :: i

        res = '{'

        do i = 1, size(this%value)
            if(i > 1) then
                res = res // ","
            end if

            serializedName = utils_strReplace(this%value(i)%name, '"', '\"')
            res = res // serializedName // ':' // this%value(i)%value%toString()
        end do

        res = res // '}'
    end function JsonObject_toString

    recursive function JsonObject_clone(this) result(res)
        class(JsonObject), intent(in) :: this
        class(JsonObject), pointer :: localRes
        class(JsonValue), pointer :: res
        integer :: i

        allocate(JsonObject :: localRes)
        allocate(localRes%value(size(this%value)))

        do i = 1, size(this%value)
            localRes%value(i)%name = this%value(i)%name
            localRes%value(i)%value => this%value(i)%value%clone()
        end do

        res => localRes
    end function JsonObject_clone

    subroutine JsonObject_add(this, name, value)
        class(JsonObject), intent(inout) :: this
        character(*), intent(in) :: name
        class(JsonValue), intent(in) :: value
        type(JsonPair), dimension(:), allocatable :: tmp

        ! Innefficient, but simple
        ! The copy prevents aliasing issues
        tmp = [this%value, JsonPair(name, value)]
        this%value = tmp
    end subroutine JsonObject_add

    recursive subroutine JsonObject_destroy(this)
        class(JsonObject), intent(inout) :: this
        integer :: i

        if(allocated(this%value)) then
            do i = 1, size(this%value)
                if(allocated(this%value(i)%name)) then
                    deallocate(this%value(i)%name)
                end if

                call this%value(i)%value%destroy()
                deallocate(this%value(i)%value)
            end do

            deallocate(this%value)
        end if
    end subroutine JsonObject_destroy
end module netorcai_json

