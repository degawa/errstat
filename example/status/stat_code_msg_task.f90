module ex_stat_code_msg_task
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: errstat
    implicit none
    private
    public :: inverse

    enum, bind(c)
        enumerator :: ERROR_VALUE_IS_NAN = 1
        enumerator :: ERROR_INVALID_INPUT
        enumerator :: ERROR_VALUE_IS_INFINITE
    end enum

    type, public, extends(task_type) :: print_task_type
        real(real64) :: x
    contains
        procedure, public, pass :: execute
    end type print_task_type

contains
    subroutine execute(this, err, stat, msg)
        implicit none
        class(print_task_type), intent(in) :: this
        type(error_stat_type), intent(inout), optional :: err
        integer(int32), intent(inout), optional :: stat
        character(*), intent(inout), optional :: msg

        print '("input value x is", E16.7)', this%x
        if (present(stat)) print *, "status :", stat
        if (present(msg)) print *, "message :", msg

        return
        if (present(err)) continue
    end subroutine execute

    function print_task_factory(x) result(new_task)
        implicit none
        real(real64), intent(in) :: x

        type(print_task_type) :: new_task

        new_task%x = x
    end function print_task_factory

    function inverse(x, stat, msg) result(y)
        use, intrinsic :: ieee_arithmetic
        implicit none
        real(real64), intent(in) :: x
        integer(int32), intent(out), optional :: stat
        character(:), allocatable, intent(inout), optional :: msg

        real(real64) :: y
        y = 0d0

        if (transfer(x, mold=0_int64) == 0) then
            call catch_status(ERROR_INVALID_INPUT, &
                              "input value is invalid", &
                              stat, msg, &
                              print_task_factory(x))
            return
        end if

        y = ieee_value(x, ieee_positive_inf)

        if (ieee_is_nan(y)) then
            call catch_status(ERROR_VALUE_IS_NAN, &
                              "value is nan", &
                              stat, msg, &
                              print_task_factory(x))
            return
        end if

        if (.not. ieee_is_finite(y)) then
            call catch_status(ERROR_VALUE_IS_INFINITE, &
                              "value is infinite", &
                              stat, msg, &
                              print_task_factory(x))
            return
        end if
    end function inverse
end module ex_stat_code_msg_task

program stat_code_msg_task
    use, intrinsic :: iso_fortran_env
    use :: ex_stat_code_msg_task
    use :: errstat
    implicit none

    integer(int32) :: stat
    character(:), allocatable :: msg

    real(real64) :: y

    y = inverse(0d0)
    print *, y

    y = inverse(0d0, stat)
    if (is_status(stat, statuses%error_occurred)) then
        print *, y, stat
    end if

    y = inverse(0d0, msg=msg)
    if (is_status(msg, statuses%error_occurred)) then
        print *, y, msg
    end if

    y = inverse(0d0, stat, msg)
    if (is_status(stat, statuses%error_occurred)) then
        print *, y, stat, msg
    end if
end program stat_code_msg_task
