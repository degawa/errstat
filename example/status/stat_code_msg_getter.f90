module ex_stat_code_msg_getter
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
                              get_error_string, &
                              stat, msg)
            return
        end if

        y = ieee_value(x, ieee_quiet_nan)

        if (ieee_is_nan(y)) then
            call catch_status(ERROR_VALUE_IS_NAN, &
                              get_error_string, &
                              stat, msg)
            return
        end if

        if (.not. ieee_is_finite(y)) then
            call catch_status(ERROR_VALUE_IS_INFINITE, &
                              get_error_string, &
                              stat, msg)
            return
        end if

        call catch_status(success_status_code, success_status_msg, stat, msg)
    end function inverse
end module ex_stat_code_msg_getter

program stat_code_msg_getter
    use, intrinsic :: iso_fortran_env
    use :: ex_stat_code_msg_getter
    use :: errstat
    implicit none

    integer(int32) :: stat
    character(:), allocatable :: msg

    real(real64) :: y

    y = inverse(0d0)
    print *, y

    y = inverse(0d0, stat=stat)
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
end program stat_code_msg_getter
