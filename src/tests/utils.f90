module netorcai_test_utils
    use netorcai_utils
    use zofu

    implicit none
    public
contains
    subroutine setup()
        ! Nothing to do
    end subroutine setup

    subroutine test_toLower(test)
        class(unit_test_type), intent(inout) :: test
        character(:), allocatable :: inStr, outStr

        inStr = " !""#$%&\'()*+,-./0369:;<=>?@AFKPUZ[\\]^_`afkpuz{|}~"
        outStr = " !""#$%&\'()*+,-./0369:;<=>?@afkpuz[\\]^_`afkpuz{|}~"
        call test%assert(utils_toLower(inStr), outStr)
    end subroutine test_toLower

    subroutine test_toUpper(test)
        class(unit_test_type), intent(inout) :: test
        character(:), allocatable :: inStr, outStr

        inStr = " !""#$%&\'()*+,-./0369:;<=>?@AFKPUZ[\\]^_`afkpuz{|}~"
        outStr = " !""#$%&\'()*+,-./0369:;<=>?@AFKPUZ[\\]^_`AFKPUZ{|}~"
        call test%assert(utils_toUpper(inStr), outStr)
    end subroutine test_toUpper

    subroutine test_startsWith(test)
        class(unit_test_type), intent(inout) :: test

        call test%assert(utils_startsWith("aba", "aba"))
        call test%assert(utils_startsWith("aba", "a"))
        call test%assert(utils_startsWith("aba", ""))
        call test%assert(utils_startsWith("aba", "ba", 2))
        call test%assert(utils_startsWith("abax", "x", 4))
        call test%assert(utils_startsWith("abax", "bax", 2))
        call test%assert(utils_startsWith("", ""))
        call test%assert(utils_startsWith("aaa", "a", 3))
        call test%assert(.not. utils_startsWith("aba", "b"))
        call test%assert(.not. utils_startsWith("abax", "a", 2))
        call test%assert(.not. utils_startsWith("abax", "x", 3))
        call test%assert(.not. utils_startsWith("abax", "x", 5))
        call test%assert(.not. utils_startsWith("aba", "abax", 2))
        call test%assert(.not. utils_startsWith("", " "))
        call test%assert(.not. utils_startsWith("aaa", "a", 4))
    end subroutine test_startsWith

    subroutine test_strReplace(test)
        class(unit_test_type), intent(inout) :: test

        call test%assert(utils_strReplace("aba", "a", "b"), "bbb")
        call test%assert(utils_strReplace("aba", "aba", ""), "")
        call test%assert(utils_strReplace("abbab", "ab", "a"), "aba")
        call test%assert(utils_strReplace("abbab", "a", "ab"), "abbbabb")
        call test%assert(utils_strReplace("abbab", "a", "a"), "abbab")
        call test%assert(utils_strReplace(" this is a test ", " ", "  "), "  this  is  a  test  ")
    end subroutine test_strReplace

    subroutine test_hexToInt(test)
        class(unit_test_type), intent(inout) :: test
        integer :: value
        logical :: fail

        value = utils_hexToInt('0', fail)
        call test%assert(.not. fail)
        call test%assert(value, 0_4)

        value = utils_hexToInt('0000', fail)
        call test%assert(.not. fail)
        call test%assert(value, 0_4)

        value = utils_hexToInt('9', fail)
        call test%assert(.not. fail)
        call test%assert(value, 9_4)

        value = utils_hexToInt('A', fail)
        call test%assert(.not. fail)
        call test%assert(value, 10_4)

        value = utils_hexToInt('F', fail)
        call test%assert(.not. fail)
        call test%assert(value, 15_4)

        value = utils_hexToInt('1A', fail)
        call test%assert(.not. fail)
        call test%assert(value, 26_4)

        value = utils_hexToInt('9F', fail)
        call test%assert(.not. fail)
        call test%assert(value, 159_4)

        value = utils_hexToInt('A0', fail)
        call test%assert(.not. fail)
        call test%assert(value, 160_4)

        value = utils_hexToInt('FF', fail)
        call test%assert(.not. fail)
        call test%assert(value, 255_4)

        value = utils_hexToInt('100', fail)
        call test%assert(.not. fail)
        call test%assert(value, 256_4)

        value = utils_hexToInt('FFFF', fail)
        call test%assert(.not. fail)
        call test%assert(value, 65535_4)

        value = utils_hexToInt('10000', fail)
        call test%assert(.not. fail)
        call test%assert(value, 65536_4)

        value = utils_hexToInt('B90C9A2', fail)
        call test%assert(.not. fail)
        call test%assert(value, 194038178_4)

        value = utils_hexToInt('01234567', fail)
        call test%assert(.not. fail)
        call test%assert(value, 19088743_4)

        value = utils_hexToInt('09AbCdEf', fail)
        call test%assert(.not. fail)
        call test%assert(value, 162254319_4)

        value = utils_hexToInt('7FffffFF', fail)
        call test%assert(.not. fail)
        call test%assert(value, 2147483647_4)

        value = utils_hexToInt('0xFF', fail)
        call test%assert(fail)

        value = utils_hexToInt('BADVALUE', fail)
        call test%assert(fail)

        value = utils_hexToInt(' BAD ', fail)
        call test%assert(fail)

        ! Overflow
        value = utils_hexToInt('FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF', fail)
        call test%assert(fail)
    end subroutine test_hexToInt

    subroutine test_intToHex(test)
        class(unit_test_type), intent(inout) :: test

        call test%assert(utils_toUpper(utils_intToHex(0_4)), '0')
        call test%assert(utils_toUpper(utils_intToHex(1_4)), '1')
        call test%assert(utils_toUpper(utils_intToHex(9_4)), '9')
        call test%assert(utils_toUpper(utils_intToHex(10_4)), 'A')
        call test%assert(utils_toUpper(utils_intToHex(15_4)), 'F')
        call test%assert(utils_toUpper(utils_intToHex(16_4)), '10')
        call test%assert(utils_toUpper(utils_intToHex(26_4)), '1A')
        call test%assert(utils_toUpper(utils_intToHex(159_4)), '9F')
        call test%assert(utils_toUpper(utils_intToHex(160_4)), 'A0')
        call test%assert(utils_toUpper(utils_intToHex(255_4)), 'FF')
        call test%assert(utils_toUpper(utils_intToHex(256_4)), '100')
        call test%assert(utils_toUpper(utils_intToHex(65535_4)), 'FFFF')
        call test%assert(utils_toUpper(utils_intToHex(65536_4)), '10000')
        call test%assert(utils_toUpper(utils_intToHex(194038178_4)), 'B90C9A2')
        call test%assert(utils_toUpper(utils_intToHex(2147483647_4)), '7FFFFFFF')
    end subroutine test_intToHex

    subroutine test_intToStr(test)
        class(unit_test_type), intent(inout) :: test

        call test%assert(utils_intToStr(0_4), '0')
        call test%assert(utils_intToStr(-1_4), '-1')
        call test%assert(utils_intToStr(1_4), '1')
        call test%assert(utils_intToStr(9_4), '9')
        call test%assert(utils_intToStr(42_4), '42')
        call test%assert(utils_intToStr(-42_4), '-42')
        call test%assert(utils_intToStr(1094827_4), '1094827')
        call test%assert(utils_intToStr(-1094827_4), '-1094827')
        call test%assert(utils_intToStr(1089482769_4), '1089482769')
        call test%assert(utils_intToStr(-1089482769_4), '-1089482769')
    end subroutine test_intToStr

    subroutine test_longToStr(test)
        class(unit_test_type), intent(inout) :: test

        call test%assert(utils_longToStr(0_8), '0')
        call test%assert(utils_longToStr(-1_8), '-1')
        call test%assert(utils_longToStr(1_8), '1')
        call test%assert(utils_longToStr(-42_8), '-42')
        call test%assert(utils_longToStr(1809482769_8), '1809482769')
        call test%assert(utils_longToStr(-1809482769_8), '-1809482769')
        call test%assert(utils_longToStr(190794568237113_8), '190794568237113')
        call test%assert(utils_longToStr(-190794568237113_8), '-190794568237113')
    end subroutine test_longToStr

    subroutine test_floatToStr(test)
        class(unit_test_type), intent(inout) :: test

        ! Since the scientific notation may be actually used its hard to test this
        call test%assert(utils_startsWith(utils_floatToStr(0.0_4), '0'))
        call test%assert(utils_startsWith(utils_floatToStr(-1.0_4), '-1'))
        !call test%assert(utils_startsWith(utils_floatToStr(42.0_4), '42'))
        !call test%assert(utils_startsWith(utils_floatToStr(-42.0_4), '-42'))
        call test%assert(utils_startsWith(utils_floatToStr(3.141592_4), '3.1415'))
        call test%assert(utils_startsWith(utils_floatToStr(-3.141592_4), '-3.1415'))
        !call test%assert(utils_startsWith(utils_floatToStr(12345.67_4), '12345.6'))
        !call test%assert(utils_startsWith(utils_floatToStr(1234567.0_4), '123456'))
    end subroutine test_floatToStr

    subroutine test_doubleToStr(test)
        class(unit_test_type), intent(inout) :: test

        ! Since the scientific notation may be actually used its hard to test this
        call test%assert(utils_startsWith(utils_doubleToStr(0.0_8), '0'))
        call test%assert(utils_startsWith(utils_doubleToStr(-1.0_8), '-1'))
        !call test%assert(utils_startsWith(utils_doubleToStr(42.0_8), '42'))
        !call test%assert(utils_startsWith(utils_doubleToStr(-42.0_8), '-42'))
        call test%assert(utils_startsWith(utils_doubleToStr(3.1415926535_8), '3.1415926535'))
        call test%assert(utils_startsWith(utils_doubleToStr(-3.1415926535_8), '-3.1415926535'))
        !call test%assert(utils_startsWith(utils_doubleToStr(12345.6789_8), '12345.6789'))
        !call test%assert(utils_startsWith(utils_doubleToStr(123456789.0_8), '123456789'))
    end subroutine test_doubleToStr

    subroutine test_files(test)
        class(unit_test_type), intent(inout) :: test
        character(:), allocatable :: filename, content, tmpStr
        real :: randVal
        integer :: randInt
        integer :: i
        logical :: fail

        ! Should not crash
        randInt = utils_getFileUnit()

        ! Choose a random filename with 1 chance over 1 000 000 000 
        ! to fail if the test is run in parallel...
        call random_number(randVal)
        randInt = floor(randVal * 1000000000)
        filename = "/tmp/delete_me_" // utils_intToHex(randInt)

        call utils_removeFile(filename)

        ! Simple test with a multi-line content
        tmpStr = "This is a" // achar(10) // "multiline content"
        call utils_setFileContent(filename, tmpStr, fail)
        call test%assert(.not. fail)
        content = utils_getFileContent(filename, fail)
        call test%assert(.not. fail)
        call test%assert(content, tmpStr)
        call utils_removeFile(filename, fail)
        call test%assert(.not. fail)

        ! Big file to test chunks (~ 70K per lines)
        tmpStr = "0"
        do i = 1, 9999
            tmpStr = tmpStr // " - " // utils_intToStr(i)
        end do
        call utils_setFileContent(filename, tmpStr // achar(10) // tmpStr, fail)
        call test%assert(.not. fail)
        content = utils_getFileContent(filename, fail)
        call test%assert(.not. fail)
        call test%assert(content, tmpStr // achar(10) // tmpStr)
        call utils_removeFile(filename, fail)
        call test%assert(.not. fail)

        filename = "/this/path/does/not/exists/" // utils_intToHex(randInt)
        content = utils_getFileContent(filename, fail)
        call test%assert(fail)
        call utils_setFileContent(filename, "", fail)
        call test%assert(fail)
        call utils_removeFile(filename, fail)
        call test%assert(fail)
    end subroutine test_files
end module netorcai_test_utils

