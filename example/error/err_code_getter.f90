module ex_err_code_getter
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
    function get_error_string(stat) result(msg)
        implicit none
        integer(int32), intent(in) :: stat
        character(:), allocatable :: msg

        select case (stat)
        case (ERROR_VALUE_IS_NAN); msg = "value is nan"
        case (ERROR_INVALID_INPUT); msg = "input value is invalid"
        case (ERROR_VALUE_IS_INFINITE); msg = "value is infinite"
        end select
    end function get_error_string

    function inverse(x, err) result(y)
        use, intrinsic :: ieee_arithmetic
        implicit none
        real(real64), intent(in) :: x
        type(error_stat_type), intent(inout), optional :: err

        real(real64) :: y
        y = 0d0

        if (transfer(x, mold=0_int64) == 0) then
            call catch_error(ERROR_INVALID_INPUT, &
                             get_error_string, &
                             err)
            return
        end if

        y = ieee_value(x, ieee_quiet_nan)

        if (ieee_is_nan(y)) then
            call catch_error(ERROR_VALUE_IS_NAN, get_error_string, err)
            return
        end if

        if (.not. ieee_is_finite(y)) then
            call catch_error(ERROR_VALUE_IS_INFINITE, get_error_string, err)
            return
        end if

        call set_success(err)
    end function inverse
end module ex_err_code_getter

program err_code_getter
    use, intrinsic :: iso_fortran_env
    use :: ex_err_code_getter
    use :: errstat
    implicit none

    type(error_stat_type) :: err

    real(real64) :: y

    y = inverse(0d0)
    print *, y

    y = inverse(0d0, err)
    if (error_occurred(err)) then
        print *, y, err%get_status(), err%get_message()
    end if

end program err_code_getter
