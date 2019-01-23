! Useful functions that does not exist FORTRAN 
module netorcai_utils
    implicit none
    private

    public :: utils_strReplace
contains
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

