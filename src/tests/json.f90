module netorcai_test_json
    use netorcai_json
    use netorcai_utils
    use zofu

    implicit none
    public
contains
    subroutine setup()
        ! Nothing to do
    end subroutine setup

    subroutine test_tostring(test)
        class(unit_test_type), intent(inout) :: test
        class(JsonValue), pointer :: jsonVal
        class(JsonArray), pointer :: jsonArr
        class(JsonObject), pointer :: jsonObj
        character(len=:), allocatable :: jsonStr

        ! Null
        jsonVal => json_makeNull()
        call test%assert(utils_toLower(jsonVal%toString()), "null")
        call jsonVal%destroy()

        ! Logical (true)
        jsonVal => json_makeBool(.true.)
        call test%assert(utils_toLower(jsonVal%toString()), "true")
        call jsonVal%destroy()

        ! Logical (false)
        jsonVal => json_makeBool(.false.)
        call test%assert(utils_toLower(jsonVal%toString()), "false")
        call jsonVal%destroy()

        ! Positive integers (int)
        jsonVal => json_makeInt(42_4)
        call test%assert(jsonVal%toString(), "42")
        call jsonVal%destroy()

        ! Positive integers (long)
        jsonVal => json_makeLong(420000000000_8)
        call test%assert(jsonVal%toString(), "420000000000")
        call jsonVal%destroy()

        ! Negative integers (int)
        jsonVal => json_makeInt(-42_4)
        call test%assert(jsonVal%toString(), "-42")
        call jsonVal%destroy()

        ! Negative integers (long)
        jsonVal => json_makeLong(-420000000000_8)
        call test%assert(jsonVal%toString(), "-420000000000")
        call jsonVal%destroy()

        ! Positive reals (float)
        jsonVal => json_makeFloat(3.1415926535_4)
        jsonStr = jsonVal%toString()
        call test%assert(jsonStr(1:6), "3.1415")
        call jsonVal%destroy()

        ! Negative reals (float)
        jsonVal => json_makeFloat(-3.1415926535_4)
        jsonStr = jsonVal%toString()
        call test%assert(jsonStr(1:7), "-3.1415")
        call jsonVal%destroy()

        ! Positive reals (double)
        jsonVal => json_makeDouble(3.1415926535_8)
        jsonStr = jsonVal%toString()
        call test%assert(jsonStr(1:12), "3.1415926535")
        call jsonVal%destroy()

        ! Negative reals (double)
        jsonVal => json_makeDouble(-3.1415926535_8)
        jsonStr = jsonVal%toString()
        call test%assert(jsonStr(1:13), "-3.1415926535")
        call jsonVal%destroy()

        ! Strings
        jsonVal => json_makeString('test')
        call test%assert(jsonVal%toString(), '"test"')
        call jsonVal%destroy()

        ! Empty arrays
        jsonVal => json_makeArray()
        jsonStr = utils_strReplace(jsonVal%toString(), " ", "")
        call test%assert(jsonStr, '[]')
        call jsonVal%destroy()

        ! Arrays
        jsonArr => json_makeArray()
        call jsonArr%add(json_makeInt(42_4))
        call jsonArr%add(json_makeInt(0_4))
        jsonVal => jsonArr
        jsonStr = utils_strReplace(jsonVal%toString(), " ", "")
        call test%assert(jsonStr, '[42,0]')
        call jsonVal%destroy()

        ! Empty objects
        jsonVal => json_makeObject()
        jsonStr = utils_strReplace(jsonVal%toString(), " ", "")
        call test%assert(jsonStr, '{}')
        call jsonVal%destroy()

        ! Objects
        jsonObj => json_makeObject()
        call jsonObj%add("bouh", json_makeInt(42_4))
        call jsonObj%add("bwa", json_makeInt(0_4))
        jsonVal => jsonObj
        jsonStr = utils_strReplace(jsonVal%toString(), " ", "")
        call test%assert(jsonStr, '{"bouh":42,"bwa":0}')
        call jsonVal%destroy()
    end subroutine test_tostring

    subroutine test_string_invalid(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument), allocatable :: doc
        class(Jsonvalue), pointer :: jsonValue
        character(len=:), allocatable :: inJsonStr, outJsonStr
        logical :: fail

        ! Control character are forbidden in JSON

        doc = json_parse('"' // achar(0) // '"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"' // achar(8) // '"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"' // achar(9) // '"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"' // achar(10) // '"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"' // achar(13) // '"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"' // achar(27) // '"', fail)
        call test%assert(fail)
        deallocate(doc)
    end subroutine test_string_invalid

    subroutine test_string_escape(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument), allocatable :: doc
        class(Jsonvalue), pointer :: jsonValue
        character(len=:), allocatable :: inJsonStr, outJsonStr
        logical :: fail

        inJsonStr = '{"NameWith\"Inside":"ValueWith\"Inside"}'
        doc = json_parse(inJsonStr, fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%lookup('NameWith"Inside', outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, 'ValueWith"Inside')
        outJsonStr = doc%toString()
        call test%assert(utils_strReplace(outJsonStr, " ", ""), inJsonStr)
        deallocate(doc)

        doc = json_parse('"\n"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(10))
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)), '"\n"')
        deallocate(doc)

        doc = json_parse('"\r"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(13))
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)), '"\r"')
        deallocate(doc)

        doc = json_parse('"\b"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(8))
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)), '"\b"')
        deallocate(doc)

        doc = json_parse('"\t"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(9))
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)), '"\t"')
        deallocate(doc)

        doc = json_parse('"\\"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, '\')
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)), '"\\"')
        deallocate(doc)

        doc = json_parse('"\""', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, '"')
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)), '"\""')
        deallocate(doc)

        doc = json_parse('"\/"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, '/')
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"\/"' .or. outJsonStr == '"/"')
        deallocate(doc)

        doc = json_parse('" "', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(32))
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)), '" "')
        deallocate(doc)

        doc = json_parse('"\x"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse("'", fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"\"', fail)
        call test%assert(fail)
        deallocate(doc)
    end subroutine test_string_escape

    subroutine test_string_unicode(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument), allocatable :: doc
        class(Jsonvalue), pointer :: jsonValue
        character(len=:), allocatable :: inJsonStr, outJsonStr
        logical :: fail

        doc = json_parse('"\u0041"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, 'A')
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)) == '"A"' .or. trim(adjustl(outJsonStr)) == '"\u0041"')
        deallocate(doc)

        doc = json_parse('"\u0020"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, ' ')
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)) == '" "' .or. trim(adjustl(outJsonStr)) == '"\u0020"')
        deallocate(doc)

        doc = json_parse('"\u0000"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(0))
        outJsonStr = doc%toString()
        call test%assert(trim(adjustl(outJsonStr)), '"\u0000"')
        deallocate(doc)

        doc = json_parse('"\u000A"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(10))
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"\n"' .or. outJsonStr == '"\u000A"')
        deallocate(doc)

        doc = json_parse('"\u000D"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(13))
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"\r"' .or. outJsonStr == '"\u000D"')
        deallocate(doc)

        doc = json_parse('"\u0008"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(8))
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"\b"' .or. outJsonStr == '"\u0008"')
        deallocate(doc)

        doc = json_parse('"\u0009"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(9))
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"\t"' .or. outJsonStr == '"\u0009"')
        deallocate(doc)

        doc = json_parse('"\u005C"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, '\')
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"\\"' .or. outJsonStr == '"\u005C"')
        deallocate(doc)

        doc = json_parse('"\u002F"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, '/')
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"/"' .or. outJsonStr == '"\/"' .or. outJsonStr == '"\u002F"')
        deallocate(doc)

        doc = json_parse('"\u0022"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, '"')
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"\""' .or. outJsonStr == '"\u0022"')
        deallocate(doc)

        ! This is the only ASCII character not printable but apparently accepted...
        doc = json_parse('"\u007F"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, achar(127))
        outJsonStr = doc%toString()
        ! The ascii form would be possible but not a good idea here
        call test%assert(trim(adjustl(outJsonStr)) == '"\u007F"')
        deallocate(doc)

        inJsonStr = ' { "\u0020in\u0020": "\u0020out\u0020" } '
        doc = json_parse(inJsonStr, fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%lookup(' in ', outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, ' out ')
        deallocate(doc)

        doc = json_parse('\u0041', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"\u"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"\u00"', fail)
        call test%assert(fail)
        deallocate(doc)

        ! Note: non-ascii unicode characters are not implemented.
        ! But, still, the behavior must be contrôled.
        ! This include \uXXXX with 0xXXXX >= 128 or any direct utf-8 string.
    end subroutine test_string_unicode

    subroutine test_parse(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument), allocatable :: doc
        logical :: fail

        doc = json_parse('42', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('-42', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('0', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('-0', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('3.141592')
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('""', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('"\""', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('"test"', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('null', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('true', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('false', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('{}', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('{"value": 42}', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('[]', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('[1]', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('[1, 2]', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('0x42', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('01', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('-01', fail)
        call test%assert(fail)
        deallocate(doc)
    end subroutine test_parse

    ! TODO: test get & lookup

    subroutine test_perf_arrays(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonArray), pointer :: arr
        type(JsonItem) :: item
        type(JsonDocument), allocatable :: doc
        class(JsonValue), pointer :: root
        character(len=:), allocatable :: jsonStrBase
        character(len=:), allocatable :: jsonStr
        logical :: fail
        integer :: i, cur, elemCount

        elemCount = 64000
        allocate(character(elemCount*10) :: jsonStrBase)

        ! Build the string (huge json document)
        jsonStrBase(1:2) = '[0'
        cur = 3
        do i = 1, elemCount-1
            jsonStrBase(cur:cur) = ','
            jsonStr = utils_intToStr(i)
            jsonStrBase(cur+1:cur+len(jsonStr)) = jsonStr
            deallocate(jsonStr)
            cur = cur + len(jsonStr) + 1
        end do
        jsonStrBase(cur:cur+1) = '] '
        jsonStr = jsonStrBase(1:cur+1)

        ! Parse the document once.
        ! Remember that FORTRAN deallocates allocatable variables only at
        ! the end of the function, except if you use a FORTRAN 2008 block.
        ! Note also that ALL values related to the document are deallocated
        ! when the document is deallocated, unless they are cloned.
        doc = json_parse(jsonStr, fail)
        call test%assert(.not. fail)
        root => doc%getRoot()
        call root%get(arr, fail)
        call test%assert(.not. fail)
        call test%assert(arr%size(), elemCount)
        item = arr%getItem(1)
        call item%value%get(i, fail)
        call test%assert(.not. fail)
        call test%assert(i, 0)
        item = arr%getItem(2)
        call item%value%get(i, fail)
        call test%assert(.not. fail)
        call test%assert(i, 1)
        item = arr%getItem(arr%size())
        call item%value%get(i, fail)
        call test%assert(.not. fail)
        call test%assert(i, elemCount-1)
        deallocate(doc)

        ! 15~20 Mo/s on my laptop
        do i = 2,10
            doc = json_parse(jsonStr, fail)
            call test%assert(.not. fail)
            deallocate(doc)
        end do
    end subroutine test_perf_arrays

    subroutine test_perf_objects(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonArray), pointer :: arr
        type(JsonItem) :: item
        type(JsonObject), pointer :: obj
        type(JsonPair) :: pair
        type(JsonDocument), allocatable :: doc
        class(JsonValue), pointer :: root
        character(len=:), allocatable :: jsonStrBase
        character(len=:), allocatable :: jsonStr
        logical :: fail
        integer :: i, cur, elemCount

        elemCount = 32000
        jsonStr = '{"x":1,"y":2}'
        allocate(character(elemCount*(len(jsonStr)+1)+16) :: jsonStrBase)

        ! Build the string (huge json document)
        jsonStrBase(1:len(jsonStr)+1) = '[' // jsonStr
        cur = len(jsonStr)+2
        do i = 1, elemCount-1
            jsonStrBase(cur:cur) = ','
            jsonStrBase(cur+1:cur+len(jsonStr)) = jsonStr
            cur = cur + len(jsonStr) + 1
        end do
        jsonStrBase(cur:cur+1) = '] '
        jsonStr = jsonStrBase(1:cur+1)

        ! Parse the document once.
        doc = json_parse(jsonStr, fail)
        call test%assert(.not. fail)
        root => doc%getRoot()
        call root%get(arr, fail)
        call test%assert(.not. fail)
        call test%assert(arr%size(), elemCount)
        item = arr%getItem(1)
        call item%value%get(obj, fail)
        call test%assert(.not. fail)
        call test%assert(obj%size(), 2)
        pair = obj%getItem(1)
        call test%assert(pair%name, 'x')
        call pair%value%get(i, fail)
        call test%assert(i, 1)
        item = arr%getItem(arr%size())
        call item%value%get(obj, fail)
        call test%assert(.not. fail)
        call test%assert(obj%size(), 2)
        pair = obj%getItem(2)
        call test%assert(pair%name, 'y')
        call pair%value%get(i, fail)
        call test%assert(i, 2)
        deallocate(doc)

        ! 8~10 Mo/s on my laptop
        do i = 2,10
            doc = json_parse(jsonStr, fail)
            call test%assert(.not. fail)
            deallocate(doc)
        end do
    end subroutine test_perf_objects
end module netorcai_test_json

