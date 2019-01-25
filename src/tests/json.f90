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
        call test%assert(fson_value_toString(jsonValue), "null")
        call fson_destroy(jsonValue)

        ! Positive integers
        jsonValue => fson_value_create_int(42)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(fson_value_toString(jsonValue), "42")
        call fson_destroy(jsonValue)

        ! Negative integers
        jsonValue => fson_value_create_int(-1)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(fson_value_toString(jsonValue), "-1")
        call fson_destroy(jsonValue)

        ! Reals
        jsonValue => fson_value_create_float(3.1415926535)
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(jsonStr(1:4), "3.14")
        call fson_destroy(jsonValue)

        ! Strings
        jsonValue => fson_value_create_string("test")
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(fson_value_toString(jsonValue), '"test"')
        call fson_destroy(jsonValue)

        ! Arrays
        jsonValue => fson_value_create_array()
        call fson_value_add(jsonValue, fson_value_create_int(42))
        call fson_value_add(jsonValue, fson_value_create_int(0))
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(utils_strReplace(fson_value_toString(jsonValue), " ", ""), '[42,0]')
        call fson_destroy(jsonValue)

        ! Structs
        jsonValue => fson_value_create_struct()
        call fson_value_add_pair(jsonValue, "value", fson_value_create_int(42))
        jsonStr = fson_value_toString(jsonValue)
        call test%assert(utils_strReplace(fson_value_toString(jsonValue), " ", ""), '{"value":42}')
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
end module netorcai_test_json

