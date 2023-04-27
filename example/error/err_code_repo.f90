module error_message_repository
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: errstat
    implicit none
    private

    type, private :: means
        integer(int32) :: stat
        character(64) :: msg
    end type means

    type(means), parameter :: VALUE_IS_NAN = means(1, "value is nan")
    type(means), parameter :: INVALID_INPUT = means(2, "input value is invalid")
    type(means), parameter :: VALUE_IS_INFINITE = means(3, "value is infinit")

    type, private, extends(message_repository_atype) :: inverse_error_type
        !&<
        integer(int32) :: value_is_nan      = VALUE_IS_NAN%stat
        integer(int32) :: invalid_input     = INVALID_INPUT%stat
        integer(int32) :: value_is_infinite = VALUE_IS_INFINITE%stat
        !&>
    contains
        procedure, public, pass :: get_message
    end type inverse_error_type

    type(inverse_error_type), public, parameter :: err = inverse_error_type()

contains
    function get_message(this, stat) result(msg)
        implicit none
        !&<
        class(inverse_error_type)   , intent(in) :: this
            !! passed-object dummy argument
        integer(int32)              , intent(in) :: stat
            !! error status code
        !&>
        character(:), allocatable :: msg
            !! error message

        select case (stat)
        case (success_status_code); msg = success_status_msg
        case (VALUE_IS_NAN%stat); msg = trim(VALUE_IS_NAN%msg)
        case (INVALID_INPUT%stat); msg = trim(INVALID_INPUT%msg)
        case (VALUE_IS_INFINITE%stat); msg = trim(VALUE_IS_INFINITE%msg)
        case default
            msg = "unkown error encountered. status code = "//stat_to_string(stat)
        end select

        return
        if (same_type_as(this, this)) continue
    contains

    end function get_message
end module error_message_repository

module ex_err_repo
    use, intrinsic :: iso_fortran_env
    use :: errstat
    use :: error_message_repository
    implicit none
    private
    public :: inverse

contains

    function inverse(x, stat) result(y)
        use, intrinsic :: ieee_arithmetic
        implicit none
        real(real64), intent(in) :: x
        type(error_stat_type), intent(out), optional :: stat

        real(real64) :: y

        if (transfer(x, mold=0_int64) == 0) then
            call catch_error(err%invalid_input, err, stat)
            return
        end if

        ! y = ieee_value(x, ieee_quiet_nan)

        if (ieee_is_nan(y)) then
            call catch_error(err%value_is_nan, err, stat)
            return
        end if

        if (.not. ieee_is_finite(y)) then
            call catch_error(err%value_is_infinite, err, stat)
            return
        end if

        call set_success(stat)
    end function inverse
end module ex_err_repo

program err_code_repo
    use, intrinsic :: iso_fortran_env
    use :: ex_err_repo
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

    y = inverse(1d0, stat)
    print *, y, stat%get_status(), stat%get_message()
end program err_code_repo
