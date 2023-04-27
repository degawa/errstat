module ex_err_code_msg_task
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
        type(error_stat_type), intent(in), optional :: err
        integer(int32), intent(in), optional :: stat
        character(*), intent(in), optional :: msg

        print '("input value x is", E16.7)', this%x
        if (present(err)) then
            print *, "status :", err%get_status()
            print *, "message :", err%get_message()
        end if

        return
        if (present(stat)) continue
        if (present(msg)) continue
    end subroutine execute

    function print_task_factory(x) result(new_task)
        implicit none
        real(real64), intent(in) :: x

        type(print_task_type) :: new_task

        new_task%x = x
    end function print_task_factory

    function inverse(x, stat) result(y)
        use, intrinsic :: ieee_arithmetic
        implicit none
        real(real64), intent(in) :: x
        type(error_stat_type), intent(out), optional :: stat

        real(real64) :: y
        y = 0d0

        if (transfer(x, mold=0_int64) == 0) then
            call catch_error(ERROR_INVALID_INPUT, &
                             "input value is invalid", &
                             stat, &
                             print_task_factory(x))
            return
        end if

        y = ieee_value(x, ieee_positive_inf)

        if (ieee_is_nan(y)) then
            call catch_error(ERROR_VALUE_IS_NAN, &
                             "value is nan", &
                             stat, &
                             print_task_factory(x))
            return
        end if

        if (.not. ieee_is_finite(y)) then
            call catch_error(ERROR_VALUE_IS_INFINITE, &
                             "value is infinite", &
                             stat, &
                             print_task_factory(x))
            return
        end if

        call set_success(stat)
    end function inverse
end module ex_err_code_msg_task

program err_code_msg_task
    use, intrinsic :: iso_fortran_env
    use :: ex_err_code_msg_task
    use :: errstat
    implicit none

    type(error_stat_type) :: stat

    real(real64) :: y

    y = inverse(0d0)
    print *, y

    y = inverse(0d0, stat)
    if (error_occurred(stat)) then
        print *, y, stat%get_status(), stat%get_message()
    end if
end program err_code_msg_task
