! Implement a generic C++-like vector but in FORTRAN
! Generics in FORTRAN is a bit like void*-based generics in C!
module netorcai_vector
    implicit none
    private

    ! Use to transfer any type to a Variant (aka. reinterpret cast)
    character, dimension(1), parameter, public :: void = [achar(0)]

    ! Internal type of items
    type :: Variant
        character, dimension(:), allocatable :: value
    end type Variant

    public :: Vector

    ! Use transfer to wrap any type into a Variant (array of characters)
    type, public :: Vector
        ! Note: allocation can probably be avoided by packing binary values 
        ! directly to a raw variable-sized binary array
        ! Funny note: attributes cannot starts with '_'
        type(Variant), dimension(:), pointer, private :: mRawData
        integer, private :: mSize
        integer, private :: mCapacity
    contains
        ! Append a value at the end.
        procedure, public :: add => vector_add

        ! Get a value from its index (starting from 1).
        procedure, public :: get => vector_get

        ! Set a value from its index (starting from 1).
        procedure, public :: set => vector_set

        ! Retrieve number of element currently in the vector.
        procedure, public :: size => vector_size

        ! Manual destruction of the object.
        ! Since items are opaque types, items containing
        ! objets and allocated types are not deleted.
        procedure, public :: destroy => vector_destroy

        ! Automatic destruction of the object.
        ! Since items are opaque types, items containing
        ! objets and allocated types are not deleted.
        final :: vector_destructor
    end type Vector

    interface Vector
        module procedure :: vector_init_default
        module procedure :: vector_init_withSize
    end interface
contains
    function vector_init_default() result(res)
        type(Vector) :: res

        res%mSize = 0
        res%mCapacity = 4
        allocate(res%mRawData(4))
    end function vector_init_default

    function vector_init_withSize(size) result(res)
        integer, intent(in) :: size
        type(Vector) :: res

        res%mSize = size
        res%mCapacity = size + 4
        allocate(res%mRawData(size + 4))
    end function vector_init_withSize

    subroutine vector_add(this, newItem)
        class(Vector), intent(inout) :: this
        character, dimension(:), intent(in) :: newItem
        type(Variant), dimension(:), pointer :: newData
        type(Variant) :: newInternalItem
        integer :: i

        if(this%mSize == this%mCapacity) then
            this%mCapacity = this%mCapacity * 2
            allocate(newData(this%mCapacity))
            do i = 1, this%mSize
                call move_alloc(this%mRawData(i)%value, newData(i)%value)
            end do
            deallocate(this%mRawData)
            this%mRawData => newData
        end if

        this%mSize = this%mSize + 1
        if(allocated(this%mRawData(this%mSize)%value)) then
            ! Should never be possible...
            deallocate(this%mRawData(this%mSize)%value)
        end if
        newInternalItem%value = newItem
        call move_alloc(newInternalItem%value, this%mRawData(this%mSize)%value)
    end subroutine vector_add

    function vector_get(this, index) result(res)
        class(Vector), intent(in) :: this
        integer, intent(in) :: index
        character, dimension(:), pointer :: res

        ! For debugging purpose
        if(index < 1 .or. index > this%mSize) then
            print *, "Invalid index"
            stop 1
        end if

        res => this%mRawData(index)%value
    end function vector_get

    subroutine vector_set(this, index, item)
        class(Vector), intent(inout) :: this
        integer, intent(in) :: index
        character, dimension(:), intent(in) :: item

        ! For debugging purpose
        if(index < 1 .or. index > this%mSize) then
            print *, "Invalid index"
            stop 1
        end if

        ! Not very efficient: a copy can be done if the array is
        ! already allocated en the data size matches.
        if(allocated(this%mRawData(index)%value)) then
            deallocate(this%mRawData(index)%value)
        end if
        this%mRawData(index) = Variant(item)
    end subroutine vector_set

    function vector_size(this) result(res)
        class(Vector), intent(in) :: this
        integer :: res

        res = this%mSize
    end function vector_size

    subroutine vector_destroy(this)
        class(Vector), intent(inout) :: this
        integer :: i

        do i = 1, this%mSize
            if(allocated(this%mRawData(i)%value)) then
                deallocate(this%mRawData(i)%value)
            end if
        end do

        deallocate(this%mRawData)
        nullify(this%mRawData) ! For the automatic destructor
    end subroutine vector_destroy

    subroutine vector_destructor(this)
        type(Vector), intent(inout) :: this

        if(associated(this%mRawData)) then
            call this%destroy()
        end if
    end subroutine vector_destructor
end module netorcai_vector

! program test
!     use netorcai_vector

!     implicit none

!     type :: MyStruct
!         integer :: a
!         integer, pointer :: b
!     end type MyStruct

!     type(Vector) :: v
!     integer, target :: val
!     integer, pointer :: ptr

!     v = Vector()
!     val = 17500
!     ptr => val
!     call v%add(transfer(MyStruct(42, ptr), void))
!     ptr => null()
!     print *, transfer(v%get(1), MyStruct(-1, null()))
! end program test

