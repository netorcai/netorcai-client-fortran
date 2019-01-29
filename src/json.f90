! Fun notes:
!     - Pure virtual methods are called "deferred" in FORTRAN and are both uggly and cumbersome
!     - import is a magic keyword to solve issues with the symbol definition order
!     - class should be used for polymorphic types (YES! in FORTRAN!), type otherwise
!     - allocatable behave like unique_ptr in C++ and is quite cool!

! Json local library
module netorcai_json
    use netorcai_utils

    implicit none
    private

    ! Load a Json document from a file.
    ! See the definition for further information.
    public :: json_load

    ! Load a Json document from a string.
    ! See the definition for further information.
    public :: json_parse

    ! Main class of this module: represent an abstract json value.
    type, abstract, public :: JsonValue
    contains
        ! Serialize the value into a string.
        ! Return an allocated string that should be deallocated by the user.
        procedure(JsonValue_toString), deferred :: toString

        ! Clone the value.
        ! Useful for creating a new json document since no value should be used 
        ! in multiple other value (due to automatic recursive deletion).
        procedure(JsonValue_clone), deferred :: clone

        ! Save a Json document to a file.
        ! See the definition for further information.
        procedure :: saveTo => JsonValue_saveTo
    end type JsonValue

    ! For deferred procedures
    abstract interface
        function JsonValue_toString(this) result(res)
            import JsonValue
            class(JsonValue), intent(in) :: this
            character(:), allocatable :: res
        end function JsonValue_toString

        function JsonValue_clone(this) result(res)
            import JsonValue
            class(JsonValue), intent(in) :: this
            class(JsonValue), allocatable :: res
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
    end type JsonString

    ! Funny note: type apparently usefull for gfortran to not segfault... 
    type JsonItem
        class(JsonValue), allocatable :: value
    end type JsonItem

    type, extends(JsonValue), public :: JsonArray
        type(JsonItem), dimension(:), allocatable :: value
    contains
        procedure :: toString => JsonArray_toString
        procedure :: clone => JsonArray_clone
    end type JsonArray

    type, public :: JsonPair
        character(:), allocatable :: name
        class(JsonValue), allocatable :: value
    end type JsonPair

    type, extends(JsonValue), public :: JsonObject
        type(JsonPair), dimension(:), allocatable :: value
    contains
        procedure :: toString => JsonObject_toString
        procedure :: clone => JsonObject_clone
    end type JsonObject
contains
    ! Load a Json document from a file.
    ! If fail is not set, the function crashes on error.
    function json_load(filename, fail) result(res)
        character(*), intent(in) :: filename
        logical, optional, intent(out) :: fail
        class(JsonValue), allocatable :: res
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
        class(JsonValue), allocatable :: res
        character(:), allocatable :: tmpStr
        class(JsonValue), allocatable :: tmpValue
        type(JsonItem), dimension(:), allocatable :: tmpArray
        type(JsonPair), dimension(:), allocatable :: tmpObject
        integer(8) :: tmpInt
        real(8) :: tmpReal

        fail = .false.

        call json_skipSpaces(jsonStr, offset)

        if(offset > len(jsonStr)) then
            fail = .true.
            return
        end if

        select case(jsonStr(offset:offset))
            case('n')
                fail = .not. json_expect(jsonStr, "null", offset)
                if(fail) return
                res = JsonNull()

            case('t')
                fail = .not. json_expect(jsonStr, "true", offset)
                if(fail) return
                res = JsonBool(.true.)

            case('f')
                fail = .not. json_expect(jsonStr, "false", offset)
                if(fail) return
                res = JsonBool(.false.)

            case('"')
                tmpStr = json_parseString(jsonStr, offset, fail)
                if(fail) return
                res = JsonString(tmpStr)

            case('[')
                offset = offset + 1
                allocate(tmpArray(0)) ! TODO: check this
                call json_skipSpaces(jsonStr, offset)
                if(.not. json_expect(jsonStr, ']', offset)) then
                    do
                        tmpValue = json_llParse(jsonStr, offset, fail)
                        if(fail) return
                        tmpArray = [tmpArray, JsonItem(tmpValue)] ! This is a concatenation in FORTRAN !
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
                res = JsonArray(tmpArray) ! TODO: cause a segfault of the compiler!!!

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
                        tmpValue = json_llParse(jsonStr, offset, fail)
                        if(fail) return
                        tmpObject = [tmpObject, JsonPair(tmpStr, tmpValue)]
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
                res = JsonObject(tmpObject)

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
                    res = JsonNumber(tmpReal)
                else
                    res = JsonInteger(tmpInt)
                end if

            case default
                fail = .true.
                return
        end select
    end function json_llParse

    ! Load a Json document from a string
    ! If fail is not set, the function crashes on error.
    function json_parse(jsonStr, fail) result(res)
        character(*), intent(in) :: jsonStr
        logical, optional, intent(out) :: fail
        class(JsonValue), allocatable :: res
        integer :: offset
        logical :: internalFail

        offset = 1
        res = json_llParse(jsonStr, offset, internalFail)

        ! Fail if there is non-space trailing characters
        call json_skipSpaces(jsonStr, offset)
        internalFail = internalFail .or. offset <= len(jsonStr)

        if(present(fail)) then
            fail = internalFail
        elseif(internalFail) then
            print *, "Parse error"
            stop 1
        end if
    end function json_parse

    ! Save a Json document to a file.
    ! If fail is not set, the function crashes on error.
    subroutine JsonValue_saveTo(this, filename, fail)
        class(JsonValue), intent(in) :: this
        character(*), intent(in) :: filename
        logical, optional, intent(inout) :: fail

        call utils_setFileContent(filename, this%toString(), fail)
    end subroutine JsonValue_saveTo

    function JsonNull_toString(this) result(res)
        class(JsonNull), intent(in) :: this
        character(:), allocatable :: res

        res = "null"
    end function JsonNull_toString

    function JsonNull_clone(this) result(res)
        class(JsonNull), intent(in) :: this
        class(JsonValue), allocatable :: res

        res = JsonNull()
    end function JsonNull_clone

    function JsonBool_toString(this) result(res)
        class(JsonBool), intent(in) :: this
        character(:), allocatable :: res

        if(this%value) then
            res = "true"
        else
            res = "false"
        end if
    end function JsonBool_toString

    function JsonBool_clone(this) result(res)
        class(JsonBool), intent(in) :: this
        class(JsonValue), allocatable :: res

        res = JsonBool(this%value)
    end function JsonBool_clone

    function JsonInteger_toString(this) result(res)
        class(JsonInteger), intent(in) :: this
        character(:), allocatable :: res

        res = utils_longToStr(this%value)
    end function JsonInteger_toString

    function JsonInteger_clone(this) result(res)
        class(JsonInteger), intent(in) :: this
        class(JsonValue), allocatable :: res

        res = JsonInteger(this%value)
    end function JsonInteger_clone

    function JsonNumber_toString(this) result(res)
        class(JsonNumber), intent(in) :: this
        character(:), allocatable :: res

        res = utils_doubleToStr(this%value)
    end function JsonNumber_toString

    function JsonNumber_clone(this) result(res)
        class(JsonNumber), intent(in) :: this
        class(JsonValue), allocatable :: res

        res = JsonNumber(this%value)
    end function JsonNumber_clone

    function JsonString_toString(this) result(res)
        class(JsonString), intent(in) :: this
        character(:), allocatable :: res

        res = '"' // utils_strReplace(this%value, '"', '\"') // '"'
    end function JsonString_toString

    function JsonString_clone(this) result(res)
        class(JsonString), intent(in) :: this
        class(JsonValue), allocatable :: res

        res = JsonString(this%value)
    end function JsonString_clone

    function JsonArray_toString(this) result(res)
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

    function JsonArray_clone(this) result(res)
        class(JsonArray), intent(in) :: this
        class(JsonValue), allocatable :: res
        !class(JsonArray), allocatable :: localRes
        class(JsonValue), dimension(:), allocatable :: copy

        ! TODO: duplicate the array? => YES!
        !allocate(copy(size(this%value)), source=this%value)
        !localRes = JsonArray(copy)
        !res = JsonArray(copy)
    end function JsonArray_clone

    function JsonObject_toString(this) result(res)
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

    function JsonObject_clone(this) result(res)
        class(JsonObject), intent(in) :: this
        class(JsonValue), allocatable :: res

        ! TODO: duplicate the array? => YES!
        res = JsonObject(this%value)
    end function JsonObject_clone
end module netorcai_json

