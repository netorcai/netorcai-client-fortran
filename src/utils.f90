! Useful functions that does not exist FORTRAN
!
! Funny note: Apparently NASA have done the same thing (but more complete)
! See https://naif.jpl.nasa.gov/pub/naif/toolkit_docs/FORTRAN/spicelib/
module netorcai_utils
    implicit none
    private

    public :: utils_toLower
    public :: utils_strReplace
    public :: utils_hexToInt
    public :: utils_intToHex
    public :: utils_startsWith
    public :: utils_intToStr
    public :: utils_longToStr
    public :: utils_floatToStr
    public :: utils_doubleToStr
    public :: utils_getFileUnit
    public :: utils_getFileContent
    public :: utils_setFileContent
    public :: utils_removeFile
    public :: utils_readLine
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

    ! Return true if str starts with prefix, false otherwise.
    ! If strOffset is set, compare str(strOffset:) with prefix.
    ! Assumption: 1 <= strOffset.
    function utils_startsWith(str, prefix, strOffset) result(res)
        character(*), intent(in) :: str
        character(*), intent(in) :: prefix
        integer, optional, intent(in) :: strOffset
        integer :: offset
        logical :: res

        if(present(strOffset)) then
            offset = strOffset
        else
            offset = 0
        end if

        res = str(offset:min(offset+len(prefix)-1,len(str))) == prefix
    end function utils_startsWith

    ! ! Safe substring function with clamped bounds.
    ! ! mini and maxi are included in the range.
    ! function utils_substr(str, mini, maxi) result(subStr)
    !     character(*), intent(in) :: subStr
    !     integer, intent(in) :: mini
    !     integer, intent(in) :: maxi
    !     character(:), allocatable :: subStr

    !     subStr = str(max(1,mini):min(maxi,len(str)))
    ! end function utils_substr

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

    ! Convert a non-prefixed hexadecimal string to an integer
    ! If fail is not set, the function end silently
    function utils_hexToInt(str, fail) result(res)
        character(*), intent(in) :: str
        logical, optional, intent(out) :: fail
        integer :: i, tmp
        integer(4) :: res

        res = 0

        if(present(fail)) then
            fail = .false.
        end if

        do i = 1, len(str)
            tmp = ichar(str(i:i))

            ! If overflow (allow signed overflow)
            if(res >= ishft(1, 28) .and. present(fail)) then
                fail = .true.
                return
            end if

            if(tmp >= ichar('0') .and. tmp <= ichar('9')) then
                res = res * 16 + (tmp - ichar('0'))
            else if(tmp >= ichar('A') .and. tmp <= ichar('F')) then
                res = res * 16 + (tmp - ichar('A') + 10)
            else if(tmp >= ichar('a') .and. tmp <= ichar('f')) then
                res = res * 16 + (tmp - ichar('a') + 10)
            else if(present(fail)) then
                fail = .true.
                return
            else
                ! Other characters are ignored silently...
            end if
        end do
    end function utils_hexToInt

    ! Convert an integer to a non-prefixed hexadecimal string
    ! The input value must not be negative
    function utils_intToHex(intVal) result(res)
        integer(4), intent(in) :: intVal
        character(:), allocatable :: res
        integer :: i, tmpInt
        integer(4) :: val
        character :: tmpChar

        res = ""
        val = intVal

        do while(val /= 0 .or. len(res) == 0)
            tmpInt = and(val, 15)
            val = ishft(val, -4)

            if(tmpInt < 10) then
                res = res // achar(tmpInt + iachar('0'))
            else
                res = res // achar(tmpInt - 10 + iachar('A'))
            end if
        end do

        ! Reverse the string
        do i = 1, len(res)/2
            tmpChar = res(i:i)
            res(i:i) = res(len(res)+1-i:len(res)+1-i)
            res(len(res)+1-i:len(res)+1-i) = tmpChar
        end do
    end function utils_intToHex

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

    ! Return a free unit to open a file
    function utils_getFileUnit() result(unit)
        integer :: unit, iostat
        logical :: opened

        ! Funny note: the management of files in FORTRAN is a joke, the user 
        ! must provide a unique int to identify the file, but how we can know
        ! the value to provide in a non-monolithic "I-know-everything" app ?
        ! We bruteforce all the value until one is OK!
        do unit = 32767, 1, -1
            inquire(unit=unit, opened=opened, iostat=iostat)
            if(iostat /= 0) cycle
            if(.not. opened) return
        end do
    end function utils_getFileUnit

    ! Read the whole content of a given file
    ! Return an allocated string
    ! If fail is not set, the function crashes on error
    function utils_getFileContent(filename, fail) result(fileContent)
        character(*), intent(in) :: filename
        logical, optional, intent(out) :: fail
        character(:), allocatable :: lineBuff
        character(:), allocatable :: fileContent
        integer :: unit, iostat
        logical :: end

        fileContent = ""
        end = .false.

        unit = utils_getFileUnit()

        if(present(fail)) then
            open(unit=unit, file=filename, iostat=iostat, status="old")
            fail = iostat /= 0
            if(fail) return
        else
            open(unit=unit, file=filename, status="old")
        end if

        do while(.not. end)
            call utils_readLine(unit, lineBuff, end)

            fileContent = fileContent // lineBuff
        end do

        close(unit=unit)
    end function utils_getFileContent

    ! Write the whole content of a given file
    ! If fail is not set, the function crashes on error
    subroutine utils_setFileContent(filename, content, fail)
        character(*), intent(in) :: filename
        character(*), intent(in) :: content
        logical, optional, intent(out) :: fail
        integer :: unit, iostat

        unit = utils_getFileUnit()

        if(present(fail)) then
            open(unit=unit, file=filename, iostat=iostat, status="new")
            fail = iostat /= 0
            if(fail) return
        else
            open(unit=unit, file=filename, status="new")
        end if

        if(present(fail)) then
            write(unit, "(a)", iostat=iostat) content
            fail = iostat /= 0
        else
            write(unit, "(a)") content
        end if

        close(unit)
    end subroutine utils_setFileContent

    ! Remove a filename
    ! If fail is set to true if the file does not exists
    subroutine utils_removeFile(filename, fail)
        character(*), intent(in) :: filename
        logical, optional, intent(out) :: fail
        integer :: unit, iostat

        unit = utils_getFileUnit()
        open(unit=unit, iostat=iostat, file=filename, status='old')
        if(present(fail)) fail = iostat /= 0
        if(iostat == 0) close(unit, status='delete')
    end subroutine utils_removeFile

    ! Read a line from a file
    ! If fail is not set, the function fail silently
    ! Funny note: FORTRAN cannot tell if there is an empty line at the end of the file...
    subroutine utils_readLine(unit, line, end)
        integer, intent(in) :: unit
        character(LEN=:), allocatable, intent(out) :: line
        logical, intent(out) :: end
        character(LEN=1024) :: buff
        integer status, size

        line = ""
        end = .false.

        do
            ! Funny note: prior to FORTRAN 2003, we should have used fixed-size strings here:
            !     - Fixed-size strings are padded with spaces
            !     - Static strings have to be trim like in MATLAB which cause issue with withspaces...
            !     - trim(adjustl(s)) is needed for really trim a string (trim is only for the right)
            read(unit, '(a)', advance='NO', iostat=status, size=size) buff

            if(is_iostat_end(status)) then
                end = .true.
                return
            end if

            line = line // buff(1:size)

            ! In FORTRAN a "record" seems to be a line
            if(is_iostat_eor(status)) then
                return
            end if
        end do
    end subroutine utils_readLine
end module netorcai_utils

