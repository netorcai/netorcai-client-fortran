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

    subroutine test_clone(test)
        class(unit_test_type), intent(inout) :: test
        class(JsonValue), pointer :: jsonVal
        class(JsonValue), pointer :: jsonCopy
        class(JsonArray), pointer :: jsonArr
        class(JsonObject), pointer :: jsonObj
        character(len=:), allocatable :: jsonStr

        ! Null
        jsonVal => json_makeNull()
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        call test%assert(utils_toLower(jsonCopy%toString()), "null")
        call jsonCopy%destroy()

        ! Logical (true)
        jsonVal => json_makeBool(.true.)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        call test%assert(utils_toLower(jsonCopy%toString()), "true")
        call jsonCopy%destroy()

        ! Logical (false)
        jsonVal => json_makeBool(.false.)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        call test%assert(utils_toLower(jsonCopy%toString()), "false")
        call jsonCopy%destroy()

        ! Positive integers (int)
        jsonVal => json_makeInt(42_4)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        call test%assert(jsonCopy%toString(), "42")
        call jsonCopy%destroy()

        ! Positive integers (long)
        jsonVal => json_makeLong(420000000000_8)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        call test%assert(jsonCopy%toString(), "420000000000")
        call jsonCopy%destroy()

        ! Negative integers (int)
        jsonVal => json_makeInt(-42_4)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        call test%assert(jsonCopy%toString(), "-42")
        call jsonCopy%destroy()

        ! Negative integers (long)
        jsonVal => json_makeLong(-420000000000_8)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        call test%assert(jsonCopy%toString(), "-420000000000")
        call jsonCopy%destroy()

        ! Positive reals (float)
        jsonVal => json_makeFloat(3.1415926535_4)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        jsonStr = jsonCopy%toString()
        call test%assert(jsonStr(1:6), "3.1415")
        call jsonCopy%destroy()

        ! Negative reals (float)
        jsonVal => json_makeFloat(-3.1415926535_4)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        jsonStr = jsonCopy%toString()
        call test%assert(jsonStr(1:7), "-3.1415")
        call jsonCopy%destroy()

        ! Positive reals (double)
        jsonVal => json_makeDouble(3.1415926535_8)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        jsonStr = jsonCopy%toString()
        call test%assert(jsonStr(1:12), "3.1415926535")
        call jsonCopy%destroy()

        ! Negative reals (double)
        jsonVal => json_makeDouble(-3.1415926535_8)
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        jsonStr = jsonCopy%toString()
        call test%assert(jsonStr(1:13), "-3.1415926535")
        call jsonCopy%destroy()

        ! Strings
        jsonVal => json_makeString('test')
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        call test%assert(jsonCopy%toString(), '"test"')
        call jsonCopy%destroy()

        ! Empty arrays
        jsonVal => json_makeArray()
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        jsonStr = utils_strReplace(jsonCopy%toString(), " ", "")
        call test%assert(jsonStr, '[]')
        call jsonCopy%destroy()

        ! Arrays
        jsonArr => json_makeArray()
        call jsonArr%add(json_makeInt(42_4))
        call jsonArr%add(json_makeInt(0_4))
        jsonCopy => jsonArr%clone()
        call jsonArr%destroy()
        jsonStr = utils_strReplace(jsonCopy%toString(), " ", "")
        call test%assert(jsonStr, '[42,0]')
        call jsonCopy%destroy()

        ! Empty objects
        jsonVal => json_makeObject()
        jsonCopy => jsonVal%clone()
        call jsonVal%destroy()
        jsonStr = utils_strReplace(jsonCopy%toString(), " ", "")
        call test%assert(jsonStr, '{}')
        call jsonCopy%destroy()

        ! Objects
        jsonObj => json_makeObject()
        call jsonObj%add("bouh", json_makeInt(42_4))
        call jsonObj%add("bwa", json_makeInt(0_4))
        jsonCopy => jsonObj%clone()
        call jsonObj%destroy()
        jsonStr = utils_strReplace(jsonCopy%toString(), " ", "")
        call test%assert(jsonStr, '{"bouh":42,"bwa":0}')
        call jsonCopy%destroy()
    end subroutine test_clone

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

        doc = json_parse('3.141592', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('3.141592e+00', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('314.1592e-2', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('314.1592e-02', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('0.03141592E+02', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('3141592e-6', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('-3.141592e-0', fail)
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

        doc = json_parse('{ }', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('{"value": 42}', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('[ ]', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('[1]', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('[1, 2]', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('{"v1": [], "v2": null, "v3": true, "v4": {}, "v5": "", "v6": 1}', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('[[1], null, true, 42, 3.14, {"val": false}, "ok"]', fail)
        call test%assert(.not. fail)
        deallocate(doc)

        doc = json_parse('', fail)
        call test%assert(fail)
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

        doc = json_parse('.1', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('+', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('-', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('--1', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('"\"', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('[', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('{', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('nul', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('t', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('true false', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('{true: false}', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('[1 2]', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('[[]', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('[]]', fail)
        call test%assert(fail)
        deallocate(doc)

        doc = json_parse('{"v1": 1 "v2": 2}', fail)
        call test%assert(fail)
        deallocate(doc)
    end subroutine test_parse

   subroutine test_parse_overflow(test)
       class(unit_test_type), intent(inout) :: test
       type(JsonDocument), allocatable :: doc
       logical :: fail

       ! Overflows cause fail to be set to true.
       ! Thus, the value cannot be read, but still, tests should not crash 

       ! Overflow, but should work (seen as a number)!
       doc = json_parse('12345678901234567890123456789012345678901234567890', fail)
       !call test%assert(.not. fail)
       deallocate(doc)

       ! Overflow, but should work and be ~1.2346
       doc = json_parse('12345678901234567890123456789012345678901234567890e-49', fail)
       !call test%assert(.not. fail)
       deallocate(doc)

       ! Overflow, but should work and be +inf
       doc = json_parse('1e10000000000000000000000000000000000000000000000000', fail)
       !call test%assert(.not. fail)
       deallocate(doc)

       ! Overflow, but should work and be -inf
       doc = json_parse('-1e10000000000000000000000000000000000000000000000000', fail)
       !call test%assert(.not. fail)
       deallocate(doc)

       ! Overflow, but should work and be ~0.0
       doc = json_parse('1e-10000000000000000000000000000000000000000000000000', fail)
       !call test%assert(.not. fail)
       deallocate(doc)
   end subroutine test_parse_overflow

    ! TODO: test get & lookup

    subroutine test_string_invalid(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument), allocatable :: doc
        class(Jsonvalue), pointer :: jsonValue
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
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '"A"' .or. outJsonStr == '"\u0041"')
        deallocate(doc)

        doc = json_parse('"\u0020"', fail)
        call test%assert(.not. fail)
        jsonValue => doc%getRoot()
        call jsonValue%get(outJsonStr, fail)
        call test%assert(.not. fail)
        call test%assert(outJsonStr, ' ')
        outJsonStr = doc%toString()
        outJsonStr = trim(adjustl(outJsonStr))
        call test%assert(outJsonStr == '" "' .or. outJsonStr == '"\u0020"')
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

    subroutine test_lookup(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument), allocatable :: doc
        class(JsonValue), pointer :: root
        logical :: valBool
        integer(4) :: valInt
        integer(8) :: valLong
        real(4) :: valFloat
        real(8) :: valDouble
        character(:), allocatable :: valStr
        type(JsonArray), pointer :: valArr
        type(JsonObject), pointer :: valObj
        type(JsonItem) :: valItem
        logical :: fail

        doc = json_parse('&
                            & { &
                            &   "v1": null, &
                            &   "v2": true, &
                            &   "v3": false, &
                            &   "v4": 42, &
                            &   "v5": 3.141592, &
                            &   "v6": "test", &
                            &   "v7": [4, 8, 15, 16, 23, 42], &
                            &   "v8": {"subValueA": null, "subValueB": "OK", "subValueC": 42}, &
                            &   "v9": {"conflict": 815, "conflict": 7418880} &
                            & } &
                            &', fail)
        call test%assert(.not. fail)
        root => doc%getRoot()

        ! It should not be possible to read null (no implicit type conversion)
        call root%lookup("v1", valBool, fail)
        call test%assert(fail)
        call root%lookup("v1", valInt, fail)
        call test%assert(fail)
        call root%lookup("v1", valLong, fail)
        call test%assert(fail)
        call root%lookup("v1", valFloat, fail)
        call test%assert(fail)
        call root%lookup("v1", valDouble, fail)
        call test%assert(fail)
        !call root%lookup("v1", valStr, fail)
        !call test%assert(fail)
        call root%lookup("v1", valArr, fail)
        call test%assert(fail)
        call root%lookup("v1", valObj, fail)
        call test%assert(fail)

        call root%lookup("v2", valBool, fail)
        call test%assert(.not. fail)
        call test%assert(valBool, .true.)
        call root%lookup("v3", valBool, fail)
        call test%assert(.not. fail)
        call test%assert(valBool, .false.)

        call root%lookup("v4", valInt, fail)
        call test%assert(.not. fail)
        call test%assert(valInt, 42_4)
        call root%lookup("v4", valLong, fail)
        call test%assert(.not. fail)
        call test%assert(valLong, 42_8)
        call root%lookup("v4", valFloat, fail) ! type conversion should be possible
        call test%assert(.not. fail)
        call test%assert(int(valFloat * 10000.0 + 0.5), 420000)
        call root%lookup("v4", valDouble, fail) ! type conversion should be possible
        call test%assert(.not. fail)
        call test%assert(int(valDouble * 1000000.0 + 0.5), 42000000)

        call root%lookup("v5", valFloat, fail)
        call test%assert(.not. fail)
        call test%assert(int(valFloat * 10000.0 + 0.01), 31415)
        call root%lookup("v5", valDouble, fail)
        call test%assert(.not. fail)
        call test%assert(int(valDouble * 1000000.0 + 0.01), 3141592)
        call root%lookup("v5", valInt, fail) ! type conversion should be possible
        call test%assert(.not. fail)
        call test%assert(valInt, 3_4)
        call root%lookup("v5", valLong, fail) ! type conversion should be possible
        call test%assert(.not. fail)
        call test%assert(valLong, 3_8)

        call root%lookup("v6", valStr, fail)
        call test%assert(.not. fail)
        call test%assert(valStr, "test")

        call root%lookup("v7", valArr, fail)
        call test%assert(.not. fail)
        call test%assert(valArr%size(), 6)
        valItem = valArr%getItem(5)
        call valItem%value%get(valInt)
        call test%assert(valInt, 23_4)

        call root%lookup("v8", valObj, fail)
        call test%assert(.not. fail)
        call test%assert(valObj%size(), 3)
        call valObj%lookup("subValueA", valStr, fail)
        call test%assert(fail) ! associated to null
        call valObj%lookup("subValueB", valStr, fail)
        call test%assert(.not. fail)
        call test%assert(valStr, "OK")
        call valObj%lookup("subvaluec", valStr, fail)
        call test%assert(fail) ! case sensitive

        ! Should just not crash
        call root%lookup("v9", valObj, fail)

        ! If it accepted, the value must be correct
        if(.not. fail) then
            call valObj%lookup("conflict", valInt, fail)
            call test%assert(.not. fail)
            call test%assert(valInt == 815_4 .or. valInt == 7418880_4)
        end if

        deallocate(doc)
    end subroutine test_lookup

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

    subroutine test_readWrite(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument), allocatable :: doc
        character(len=:), allocatable :: inJsonStr, outJsonStr, filename
        real :: randVal
        integer :: randInt
        logical :: fail

        inJsonStr = '{"v1": [], "v2": null, "v3": true, "v4": {}, "v5": "", "v6": 1}'
        outJsonStr = utils_strReplace(inJsonStr, ' ', '')

        ! Choose a random filename with 1 chance over 1 000 000 000 
        ! to fail if the test is run in parallel...
        call random_number(randVal)
        randInt = floor(randVal * 1000000000)
        filename = "/tmp/delete_me" // utils_intToHex(randInt)

        ! Parse and write the json on a file
        doc = json_parse(inJsonStr, fail)
        call test%assert(.not. fail)
        call doc%saveTo(filename, fail)
        call test%assert(.not. fail)
        deallocate(doc)

        ! Read the written file, check it and remove it
        doc = json_load(filename, fail)
        call test%assert(.not. fail)
        outJsonStr = doc%toString()
        call test%assert(utils_strReplace(outJsonStr, ' ', ''), utils_strReplace(inJsonStr, ' ', ''))
        call utils_removeFile(filename, fail)
        call test%assert(.not. fail)
        deallocate(doc)
    end subroutine test_readWrite
end module netorcai_test_json

