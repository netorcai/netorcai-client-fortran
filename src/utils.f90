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

    ! Convert a standard integer to a string
    ! Return tha allocated string
    function utils_intToStr(value) result(outValue)
        integer(4), intent(in) :: value
        character(len=16) :: tmpStr
        character(len=:), allocatable :: outValue

        write(tmpStr, *) value
        outValue = trim(adjustl(tmpStr))
    end function utils_intToStr

    ! Convert a long integer to a string
    ! Return tha allocated string
    function utils_longToStr(value) result(outValue)
        integer(8), intent(in) :: value
        character(len=24) :: tmpStr
        character(len=:), allocatable :: outValue

        write(tmpStr, *) value
        outValue = trim(adjustl(tmpStr))
    end function utils_longToStr

    ! Convert a simple precision number to a string
    ! Return tha allocated string
    function utils_floatToStr(value) result(outValue)
        real(4), intent(in) :: value
        character(len=24) :: tmpStr
        character(len=:), allocatable :: outValue

        write(tmpStr, "(ES14.7)") value
        outValue = trim(adjustl(tmpStr))
    end function utils_floatToStr

    ! Convert a double precision number to a string
    ! Return tha allocated string
    function utils_doubleToStr(value) result(outValue)
        real(8), intent(in) :: value
        character(len=48) :: tmpStr
        character(len=:), allocatable :: outValue

        write(tmpStr, "(ES24.15)") value
        outValue = trim(adjustl(tmpStr))
    end function utils_doubleToStr

    ! Replace all occurences of seekStr by replaceStr in str and return the result.
    ! Return an allocated string that should be deallocated by the user.
    function utils_strReplace(str, seekStr, replaceStr) result(outStr)
        character(len=*), intent(in) :: str
        character(len=*), intent(in) :: seekStr
        character(len=*), intent(in) :: replaceStr
        character(len=:), allocatable :: outStr
        integer :: i, copyLen

        outStr = ""
        i = 1
        copyLen = 0

        ! Efficient only if there is no/few pieces to be replaced
        do while(i <= len(str))
            if(str(i:min(i+len(seekStr)-1,len(str))) == seekStr) then
                outStr = outStr // str(i-copyLen:i-1) // replaceStr
                i = i + len(seekStr)
                copyLen = 0
            else
                i = i + 1
                copyLen = copyLen + 1
            end if
        end do

        outStr = outStr // str(i-copyLen:i-1)
    end function utils_strReplace
end module netorcai_utils

