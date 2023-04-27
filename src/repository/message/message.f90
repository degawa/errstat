!>The `errstat_repository_message` module provides an abstract data type
!>related to error handling.
!>
!>The abstract type `message_repository_atype` is used to get
!>the error message correspoinding to a error code.
!>
module errstat_repository_message
    implicit none
    private
    public :: stat_to_string

    !>An abstract data type to store error codes and messages
    type, public, abstract :: message_repository_atype

    contains
        procedure(Iget_message), public, pass, deferred :: get_message
        !* set the error message corresponding to the error code
        generic :: get => get_message
    end type message_repository_atype

    abstract interface
        function Iget_message(this, stat) result(msg)
            use, intrinsic :: iso_fortran_env
            import message_repository_atype
            class(message_repository_atype), intent(in) :: this
                !! passed-object dummy argument
            integer(int32), intent(in) :: stat
                !! the error code
            character(:), allocatable :: msg
                !! the error message corresponding to the error code
        end function Iget_message
    end interface

contains
    !>Returns string converted from the `i32`.
    function stat_to_string(i32) result(str)
        use, intrinsic :: iso_fortran_env
        implicit none
        integer(int32), intent(in) :: i32
            !! intger to be converted
        character(:), allocatable :: str
            !! string converted from `i32`

        character(13) :: buffer
        write (buffer, '(I0)') i32
        str = trim(buffer)
    end function stat_to_string
end module errstat_repository_message
