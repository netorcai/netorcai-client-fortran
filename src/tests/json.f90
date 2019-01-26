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
        type(fson_value), pointer :: jsonValue
        character(len=:), allocatable :: jsonStr

        ! Null
        jsonValue => fson_value_create_null()
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(utils_toLower(jsonStr), "null")
        call fson_destroy(jsonValue)

        ! Logical (true)
        jsonValue => fson_value_create_logical(.true.)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(utils_toLower(jsonStr), "true")
        call fson_destroy(jsonValue)

        ! Logical (false)
        jsonValue => fson_value_create_logical(.false.)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(utils_toLower(jsonStr), "false")
        call fson_destroy(jsonValue)

        ! Positive integers (int)
        jsonValue => fson_value_create_int(42)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(jsonStr, "42")
        call fson_destroy(jsonValue)

        ! Positive integers (long)
        jsonValue => fson_value_create_long(420000000000_8)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(jsonStr, "420000000000")
        call fson_destroy(jsonValue)

        ! Negative integers (int)
        jsonValue => fson_value_create_int(-42)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(jsonStr, "-42")
        call fson_destroy(jsonValue)

        ! Negative integers (long)
        jsonValue => fson_value_create_long(-420000000000_8)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(jsonStr, "-420000000000")
        call fson_destroy(jsonValue)

        ! Reals (float)
        jsonValue => fson_value_create_float(3.1415926535_4)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(jsonStr(1:6), "3.1415")
        call fson_destroy(jsonValue)

        ! Reals (double)
        jsonValue => fson_value_create_double(3.1415926535_8)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(jsonStr(1:12), "3.1415926535")
        call fson_destroy(jsonValue)

        ! Strings
        jsonValue => fson_value_create_string("test")
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(jsonStr, '"test"')
        call fson_destroy(jsonValue)

        ! Arrays
        jsonValue => fson_value_create_array()
        call fson_value_add(jsonValue, fson_value_create_int(42))
        call fson_value_add(jsonValue, fson_value_create_int(0))
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(utils_strReplace(jsonStr, " ", ""), '[42,0]')
        call fson_destroy(jsonValue)

        ! Structs
        jsonValue => fson_value_create_struct()
        call fson_value_add_pair(jsonValue, "value", fson_value_create_int(42))
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(utils_strReplace(jsonStr, " ", ""), '{"value":42}')
        call fson_destroy(jsonValue)
    end subroutine test_tostring

    subroutine test_string_escape(test)
        class(unit_test_type), intent(inout) :: test
        type(fson_value), pointer :: jsonValue
        character(len=256) :: jsonFixedStr
        character(len=:), allocatable :: inJsonStr
        character(len=:), allocatable :: outJsonStr

        inJsonStr = '{"NameWith\"Inside":"ValueWith\"Inside"}'
        jsonValue => fson_parse(str=inJsonStr)
        call fson_get(jsonValue, 'NameWith"Inside', jsonFixedStr)
        call test%assert(trim(jsonFixedStr), 'ValueWith"Inside')
        outJsonStr = fson_value_toString(jsonValue)
        call test%assert(utils_strReplace(outJsonStr, " ", ""), inJsonStr)
        call fson_destroy(jsonValue)
    end subroutine test_string_escape

    subroutine test_fson_bugs(test)
        class(unit_test_type), intent(inout) :: test
        type(fson_value), pointer :: jsonValue

        ! Infinite loop
        ! jsonValue => fson_parse(str='42')
        ! call fson_destroy(jsonValue)
        ! call test%assert(.true.)

        ! Infinite loop
        ! jsonValue => fson_parse(str='-42')
        ! call fson_destroy(jsonValue)
        ! call test%assert(.true.)

        ! Infinite loop
        ! jsonValue => fson_parse(str='3.141592')
        ! call fson_destroy(jsonValue)
        ! call test%assert(.true.)

        jsonValue => fson_parse(str='""')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='"\""')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='"test"')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='null')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='true')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='false')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='{}')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='{"value": 42}')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='[]')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='[1]')
        call fson_destroy(jsonValue)
        call test%assert(.true.)

        jsonValue => fson_parse(str='[1, 2]')
        call fson_destroy(jsonValue)
        call test%assert(.true.)
    end subroutine test_fson_bugs

    subroutine test_perf(test)
        class(unit_test_type), intent(inout) :: test
        type(fson_value), pointer :: jsonValue
        character(len=:), allocatable :: jsonStr
        integer :: i

        ! Build the string
        jsonStr = ""
        do i = 1, 65536/2
            jsonStr = jsonStr // " "
        end do
        jsonStr = jsonStr // '"s"'
        do i = 1, 65536/2
            jsonStr = jsonStr // " "
        end do

        do i = 1, 3
            jsonValue => fson_parse(str=jsonStr)
            call fson_destroy(jsonValue)
        end do
    end subroutine test_perf
end module netorcai_test_json

