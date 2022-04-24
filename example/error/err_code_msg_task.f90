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
        integer(int32), private :: stat
        character(:), allocatable, private :: msg
        real(real64) :: x
        logical, private :: stat_set
        logical, private :: msg_set
    contains
        procedure, public, pass :: execute
        procedure, public, pass :: set_stat
        procedure, public, pass :: set_msg
    end type print_task_type

contains
    subroutine execute(this)
        implicit none
        class(print_task_type), intent(in) :: this

        print '("input value x is", E16.7)', this%x
        if (this%stat_set) &
            print *, "status :", this%stat
        if (this%msg_set) &
            print *, "message :", this%msg
    end subroutine execute

    subroutine set_stat(this, stat)
        implicit none
        class(print_task_type), intent(inout) :: this
        integer(int32), intent(in), optional :: stat
        if (present(stat)) then
            this%stat = stat
            this%stat_set = .true.
        end if
    end subroutine set_stat

    subroutine set_msg(this, msg)
        implicit none
        class(print_task_type), intent(inout) :: this
        character(*), intent(in), optional :: msg
        if (present(msg)) then
            this%msg = msg
            this%msg_set = .true.
        end if
    end subroutine set_msg

    function print_task_factory(x, stat, msg) result(new_task)
        implicit none
        real(real64), intent(in) :: x
        integer(int32), intent(in), optional :: stat
        character(*), intent(in), optional :: msg

        type(print_task_type) :: new_task

        new_task%x = x
        call new_task%set_stat(stat)
        call new_task%set_msg(msg)
    end function print_task_factory

    function inverse(x, err) result(y)
        use, intrinsic :: ieee_arithmetic
        implicit none
        real(real64), intent(in) :: x
        type(error_stat_type), intent(inout), optional :: err

        real(real64) :: y
        y = 0d0

        if (transfer(x, mold=0_int64) == 0) then
            call catch_error(ERROR_INVALID_INPUT, &
                             "input value is invalid", &
                             err, &
                             print_task_factory(x, ERROR_INVALID_INPUT, &
                                                "input value is invalid"))
            return
        end if

        y = ieee_value(x, ieee_positive_inf)

        if (ieee_is_nan(y)) then
            call catch_error(ERROR_VALUE_IS_NAN, &
                             "value is nan", &
                             err, &
                             print_task_factory(x, ERROR_VALUE_IS_NAN, &
                                                "value is nan"))
            return
        end if

        if (.not. ieee_is_finite(y)) then
            call catch_error(ERROR_VALUE_IS_INFINITE, &
                             "value is infinite", &
                             err, &
                             print_task_factory(x, ERROR_VALUE_IS_INFINITE, &
                                                "value is infinite"))
            return
        end if

        call set_success(err)
    end function inverse
end module ex_err_code_msg_task

program err_code_msg_task
    use, intrinsic :: iso_fortran_env
    use :: ex_err_code_msg_task
    use :: errstat
    implicit none

    type(error_stat_type) :: err

    real(real64) :: y

    y = inverse(0d0)
    print *, y

    y = inverse(0d0, err)
    if (does_error_occur(err)) then
        print *, y, err%get_status(), err%get_message()
    end if
end program err_code_msg_task
