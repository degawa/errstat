module task
    use, intrinsic :: iso_fortran_env
    use :: errstat
    implicit none
    private
    public :: append_msg

    type, public, extends(task_type) :: append_task_type
        character(:), allocatable :: msg
    contains
        procedure, public, pass :: execute
    end type append_task_type

contains
    subroutine execute(this, err, stat, msg)
        implicit none
        class(append_task_type), intent(in) :: this
        type(error_stat_type), intent(inout), optional :: err
        integer(int32), intent(inout), optional :: stat
        character(*), intent(inout), optional :: msg

        if (present(err)) then
            call err%append_message(this%msg)
        end if
    end subroutine execute

    function append_msg(msg) result(new_task)
        implicit none
        character(*), intent(in) :: msg

        type(append_task_type) :: new_task

        new_task%msg = msg
    end function append_msg
end module task

program err_msg_append
    use, intrinsic :: iso_fortran_env
    use :: errstat
    use :: task
    implicit none

    type(error_stat_type) :: status
    real(real32), allocatable :: f(:)

    integer(int32) :: stat
    character(128) :: msg

    call alloc(f, 10, stat, msg)

    if (stat /= 0) then
        call catch_error(-1, "allocation failed", status, append_msg(trim(msg)))
    end if

    print *, status%get_message()

contains
    subroutine alloc(val, size, stat, msg)
        real(real32), allocatable, intent(inout) :: val(:)
        integer(int32), intent(in) :: size
        integer(int32), intent(out) :: stat
        character(128), intent(out) :: msg

        allocate (val(size))
        allocate (val(size), stat=stat, errmsg=msg)
    end subroutine alloc
end program err_msg_append
