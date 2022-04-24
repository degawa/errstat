module ex_stat_code_msg
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

contains
    function inverse(x, stat, msg) result(y)
        use, intrinsic :: ieee_arithmetic
        implicit none
        real(real64), intent(in) :: x
        integer(int32), intent(out), optional :: stat
        character(:), allocatable, intent(inout), optional :: msg

        real(real64) :: y
        y = 0d0

        if (is_zero(x) .or. ieee_is_nan(x)) then
            call catch_status(ERROR_INVALID_INPUT, &
                              "input value is invalid", &
                              stat, msg)
            return
        end if

        y = ieee_value(x, ieee_positive_inf)

        if (ieee_is_nan(y)) then
            call catch_status(ERROR_VALUE_IS_NAN, &
                              "value is nan", &
                              stat, msg)
            return
        end if

        if (.not. ieee_is_finite(y)) then
            call catch_status(ERROR_VALUE_IS_INFINITE, &
                              "value is infinite", &
                              stat, msg)
            return
        end if

    contains
        logical function is_zero(val)
            implicit none
            real(real64), intent(in) :: val

            is_zero = (transfer(val, mold=0_int64) == 0)

        end function is_zero
    end function inverse
end module ex_stat_code_msg

program stat_code_msg
    use, intrinsic :: iso_fortran_env
    use :: ex_stat_code_msg
    use :: errstat
    implicit none

    integer(int32) :: stat
    character(:), allocatable :: msg

    real(real64) :: y

    y = inverse(1d0)
    print *, y

    y = inverse(0d0, stat)
    if (is_status(stat, statuses%error_occurred)) then
        print *, stat, y
    end if

    y = inverse(0d0, msg=msg)
    if (is_status(msg, statuses%error_occurred)) then
        print *, y, msg
    end if

    y = inverse(0d0, stat, msg)
    if (is_status(stat, statuses%error_occurred)) then
        print *, y, stat, msg
    end if
end program stat_code_msg
