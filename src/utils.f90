! Useful functions that does not exist FORTRAN 
module netorcai_utils
    implicit none
    private

    public :: utils_toLower
    public :: utils_strReplace
    public :: utils_intToStr
    public :: utils_longToStr
    public :: utils_floatToStr
    public :: utils_doubleToStr
contains
    ! Seriously FORTRAN does not implement such a basic function...
    ! Return an allocated string that is a lower-case version of str
    ! Only ASCII characters are converted
    function utils_toLower(str) result(outStr)
        character(len=*), intent(in) :: str
        character(len=:), allocatable :: outStr
        integer :: i, c

        outStr = str

        do i = 1, len(str)
            c = ichar(outStr(i:i))
            if(c >= ichar('A') .and. c < ichar('Z')) then
                outStr(i:i) = achar(c + 32)
            end if
        end do
    end function utils_toLower

    function utils_intToStr(value) result(outValue)
        integer(4), intent(in) :: value
        character(len=16) :: tmpStr
        character(len=:), allocatable :: outValue

        write(tmpStr, *) outValue
        outValue = trim(adjustl(tmpStr))
    end function utils_intToStr

    function utils_longToStr(value) result(outValue)
        integer(8), intent(in) :: value
        character(len=24) :: tmpStr
        character(len=:), allocatable :: outValue

        write(tmpStr, *) outValue
        outValue = trim(adjustl(tmpStr))
    end function utils_longToStr

    function utils_floatToStr(value) result(outValue)
        real(4), intent(in) :: value
        character(len=24) :: tmpStr
        character(len=:), allocatable :: outValue

        write(tmpStr, "(ES14.7)") outValue
        outValue = trim(adjustl(tmpStr))
    end function utils_floatToStr

    function utils_doubleToStr(value) result(outValue)
        real(8), intent(in) :: value
        character(len=48) :: tmpStr
        character(len=:), allocatable :: outValue

        write(tmpStr, "(ES24.15)") outValue
        outValue = trim(adjustl(tmpStr))
    end function utils_doubleToStr

    ! Replace all occurences of seekStr by replaceStr in str and return the result.
    ! Return an allocated string that should be deallocated by the user.
    function utils_strReplace(str, seekStr, replaceStr) result(outStr)
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: seekStr
        character(len=*), intent(in) :: replaceStr
        character(len=:), allocatable :: outStr
        integer :: i

        outStr = ""
        i = 1

        ! Naive algorithm (very inefficient)
        do while(i <= len(str))
            if(str(i:min(i+len(seekStr)-1,len(str))) == seekStr) then
                outStr = outStr // replaceStr
                i = i + len(seekStr)
            else
                outStr = outStr // str(i:i)
                i = i + 1
            end if
        end do
    end function utils_strReplace
end module netorcai_utils

