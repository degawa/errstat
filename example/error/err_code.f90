module ex_err_code_msg
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
                             err)
            return
        end if

        y = ieee_value(x, ieee_positive_inf)

        if (ieee_is_nan(y)) then
            call catch_error(ERROR_VALUE_IS_NAN, "value is nan", err)
            return
        end if

        if (.not. ieee_is_finite(y)) then
            call catch_error(ERROR_VALUE_IS_INFINITE, "value is infinite", err)
            return
        end if

        call set_success(err)
    end function inverse
end module ex_err_code_msg

program err_code
    use, intrinsic :: iso_fortran_env
    use :: ex_err_code_msg
    use :: errstat
    implicit none

    type(error_stat_type) :: err

    real(real64) :: y

    y = inverse(0d0)
    print *, y

    y = inverse(1d0, err)
    if (does_error_occur(err)) then
        print *, err%get_message()
    end if
end program err_code
