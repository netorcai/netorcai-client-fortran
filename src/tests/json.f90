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
        class(JsonValue), pointer :: jsonValue
        class(JsonArray), pointer :: jsonArray
        class(JsonObject), pointer :: jsonObject
        character(len=:), allocatable :: jsonStr

        ! Null
        jsonValue => json_makeNull()
        call test%assert(utils_toLower(jsonValue%toString()), "null")
        call jsonValue%destroy()

        ! Logical (true)
        jsonValue => json_makeBool(.true.)
        call test%assert(utils_toLower(jsonValue%toString()), "true")
        call jsonValue%destroy()

        ! Logical (false)
        jsonValue => json_makeBool(.false.)
        call test%assert(utils_toLower(jsonValue%toString()), "false")
        call jsonValue%destroy()

        ! Positive integers (int)
        jsonValue => json_makeInt(42_4)
        call test%assert(jsonValue%toString(), "42")
        call jsonValue%destroy()

        ! Positive integers (long)
        jsonValue => json_makeLong(420000000000_8)
        call test%assert(jsonValue%toString(), "420000000000")
        call jsonValue%destroy()

        ! Negative integers (int)
        jsonValue => json_makeInt(-42_4)
        call test%assert(jsonValue%toString(), "-42")
        call jsonValue%destroy()

        ! Negative integers (long)
        jsonValue => json_makeLong(-420000000000_8)
        call test%assert(jsonValue%toString(), "-420000000000")
        call jsonValue%destroy()

        ! Positive reals (float)
        jsonValue => json_makeFloat(3.1415926535_4)
        jsonStr = jsonValue%toString()
        call test%assert(jsonStr(1:6), "3.1415")
        call jsonValue%destroy()

        ! Negative reals (float)
        jsonValue => json_makeFloat(-3.1415926535_4)
        jsonStr = jsonValue%toString()
        call test%assert(jsonStr(1:7), "-3.1415")
        call jsonValue%destroy()

        ! Positive reals (double)
        jsonValue => json_makeDouble(3.1415926535_8)
        jsonStr = jsonValue%toString()
        call test%assert(jsonStr(1:12), "3.1415926535")
        call jsonValue%destroy()

        ! Negative reals (double)
        jsonValue => json_makeDouble(-3.1415926535_8)
        jsonStr = jsonValue%toString()
        call test%assert(jsonStr(1:13), "-3.1415926535")
        call jsonValue%destroy()

        ! Strings
        jsonValue => json_makeString('test')
        call test%assert(jsonValue%toString(), '"test"')
        call jsonValue%destroy()

        ! Empty arrays
        jsonValue => json_makeArray()
        jsonStr = utils_strReplace(jsonValue%toString(), " ", "")
        call test%assert(jsonStr, '[]')
        call jsonValue%destroy()

        ! Arrays
        jsonArray => json_makeArray()
        call jsonArray%add(json_makeInt(42_4))
        call jsonArray%add(json_makeInt(0_4))
        jsonValue => jsonArray
        jsonStr = utils_strReplace(jsonValue%toString(), " ", "")
        call test%assert(jsonStr, '[42,0]')
        call jsonValue%destroy()

        ! Empty objects
        jsonValue => json_makeObject()
        jsonStr = utils_strReplace(jsonValue%toString(), " ", "")
        call test%assert(jsonStr, '{}')
        call jsonValue%destroy()

        ! Objects
        jsonObject => json_makeObject()
        call jsonObject%add("bouh", json_makeInt(42_4))
        call jsonObject%add("bwa", json_makeInt(0_4))
        jsonValue => jsonObject
        jsonStr = utils_strReplace(jsonValue%toString(), " ", "")
        call test%assert(jsonStr, '{"bouh":42,"bwa":0}')
        call jsonValue%destroy()
    end subroutine test_tostring

    subroutine test_string_escape(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument) :: doc
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
    end subroutine test_string_escape

    subroutine test_parse(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonDocument) :: doc
        logical :: fail

        doc = json_parse('42', fail)
        call test%assert(.not. fail)

        doc = json_parse('-42', fail)
        call test%assert(.not. fail)

        doc = json_parse('3.141592')
        call test%assert(.not. fail)

        doc = json_parse('""', fail)
        call test%assert(.not. fail)

        doc = json_parse('"\""', fail)
        call test%assert(.not. fail)

        doc = json_parse('"test"', fail)
        call test%assert(.not. fail)

        doc = json_parse('null', fail)
        call test%assert(.not. fail)

        doc = json_parse('true', fail)
        call test%assert(.not. fail)

        doc = json_parse('false', fail)
        call test%assert(.not. fail)

        doc = json_parse('{}', fail)
        call test%assert(.not. fail)

        doc = json_parse('{"value": 42}', fail)
        call test%assert(.not. fail)

        doc = json_parse('[]', fail)
        call test%assert(.not. fail)

        doc = json_parse('[1]', fail)
        call test%assert(.not. fail)

        doc = json_parse('[1, 2]', fail)
        call test%assert(.not. fail)
    end subroutine test_parse

    ! TODO: test get & lookup

    subroutine test_perf(test)
        class(unit_test_type), intent(inout) :: test
        type(JsonItem), dimension(:), allocatable :: arr
        type(JsonDocument), allocatable :: doc
        class(JsonValue), pointer :: root
        character(len=:), allocatable :: jsonStr
        logical :: fail
        integer :: i

        ! Build the string: a 50 Ko json document
        jsonStr = "[0"
        do i = 1, 10000
            jsonStr = jsonStr // ',' // utils_intToStr(i)
        end do
        jsonStr = jsonStr // ']'

        ! Too slow... (150 Ko/s on my laptop)
        doc = json_parse(jsonStr, fail)
        call test%assert(.not. fail)
        root => doc%getRoot()
        call root%get(arr, fail)
        call test%assert(.not. fail)
        call test%assert(size(arr) == 10001)
    end subroutine test_perf
end module netorcai_test_json

