!>The `errstat_type_errorStat` module provides an user-derived type
!>related to error handling.
!>
!>The derived type `error_stat_type` is used for handling an error status and message.
!>It has components to store the status and message.
!>It also has type-bound procedure to set and get the components.
!>
module errstat_type_errorStat
    use, intrinsic :: iso_fortran_env
    use :: errstat_constant_status
    implicit none
    private

    !>The user-derived type for handling an error status and message.
    !>### Example
    !>```Fortran
    !>use :: errstat
    !>
    !>real(real64) :: y
    !>type(error_stat_type) :: err
    !>
    !>y = inverse(x=0d0) ! user-defined arithmetic function to compute 1/x
    !>print *, y
    !>
    !>y = inverse(x=0d0, err_stat=err)
    !>if (does_error_occur(err)) then
    !>    print *, err%get_message() ! "input value is invalid"
    !>end if
    !>```
    !>
    type, public :: error_stat_type
        integer(int32), private :: stat = success_status_code
            !! an error status code
        character(:), allocatable, private :: msg
            !! an error message.<br>
            !! This is not allocated when no error occurred
    contains
        procedure, public, pass :: set_status_and_message
        !* set error status and message
        procedure, public, pass :: append_message
        !* append message to error message
        procedure, public, pass :: get_status
        !* returns the error sutatus
        procedure, public, pass :: get_message
        !* returns the error message
        procedure, public, pass :: error_occurred
        !* returns `.true.` if error occurred
        procedure, public, pass :: initialize
        !* initialize the `error_stat_type` instance
        final :: finalize
        !* finalize the `error_stat_type` instance
    end type error_stat_type

contains
    !>Sets the error status code and message
    subroutine set_status_and_message(this, stat, msg)
        implicit none
        class(error_stat_type), intent(inout) :: this
            !! passed-object dummy argument.
        integer(int32), intent(in) :: stat
            !! an error status.
        character(*), intent(in) :: msg
            !! an error message.

        this%stat = stat
        this%msg = msg
    end subroutine set_status_and_message

    !>Appends the message to the error message
    subroutine append_message(this, msg)
        implicit none
        class(error_stat_type), intent(inout) :: this
            !! passed-object dummy argument.
        character(*), intent(in) :: msg
            !! an error message.

        this%msg = this%msg//" - "//msg
    end subroutine append_message

    !>Returns the status code.
    function get_status(this) result(stat)
        implicit none
        class(error_stat_type), intent(in) :: this
            !! passed-object dummy argument.
        integer(int32) :: stat
            !! the error status code.

        stat = this%stat
    end function get_status

    !>Returns the error message.
    function get_message(this) result(msg)
        implicit none
        class(error_stat_type), intent(in) :: this
            !! passed-object dummy argument.
        character(:), allocatable :: msg
            !! the error message.

        msg = ""
        if (allocated(this%msg)) msg = this%msg
    end function get_message

    !>Returns `.true.` if error occurred and returns `.false.` elsewhere.
    function error_occurred(this)
        implicit none
        class(error_stat_type), intent(in) :: this
            !! passed-object dummy argument.
        logical :: error_occurred
            !! the flag that error occurred or not.

        error_occurred = (this%get_status() /= success_status_code)
    end function error_occurred

    !>Initializes `error_stat_type` object.
    !>This procedure
    !>
    !>- set `stat` to `success_status_code`
    !>- deallocate `msg`
    !>
    subroutine initialize(this)
        implicit none
        class(error_stat_type), intent(inout) :: this
            !! passed-object dummy argument.

        this%stat = success_status_code
        if (allocated(this%msg)) deallocate (this%msg)
    end subroutine initialize

    !>Finializes an `error_stat_type` object.
    subroutine finalize(this)
        implicit none
        type(error_stat_type), intent(inout) :: this
            !! passed-object dummy argument.

        if (allocated(this%msg)) deallocate (this%msg)
    end subroutine finalize
end module errstat_type_errorStat
