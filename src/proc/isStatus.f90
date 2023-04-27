!>The `errstat_proc_isStatus` provides constants and procedures
!>related to error statuses.
!>
!>The constants include statuses representing success and error-occurrence.
!>The statuses are defined as constants of private user-defined derived type
!>objects. The user-defined type provides `==` and `/=` operators
!>to determine if an error status code is success or error-occurrence.
!>
!>The procedures include functions to check an error status code is
!>success or error-occurrence.
!>
module errstat_proc_isStatus
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    use :: errstat_constant_status
    implicit none
    private
    public :: is_status

    !>Returns `.true.` if a status code represents
    !>a status specified by the 2nd argument.
    interface is_status
        procedure :: is_status_i4
        procedure :: is_status_str
    end interface

    !>An user-defined derived type to indicate a status of whether
    !>a process was succeeded or not.
    type, private :: errstat_status_type
        logical, public :: status
            !! status of  of whether a process was succeeded or not.<br>
            !! This component must be public for declaring constants.
    contains
        procedure, public, pass :: errstat_status_is_status_equal
        !* comparator to determine that two objects represent the same state.
        procedure, public, pass :: errstat_status_is_status_not_equal
        !* comparator to determine that two objects do not represent the same state.
        generic :: operator(==) => errstat_status_is_status_equal
        generic :: operator(/=) => errstat_status_is_status_not_equal
    end type errstat_status_type

    type(errstat_status_type), private, parameter :: succeeded = errstat_status_type(.true.)
        !! a constant representing a status of success.
    type(errstat_status_type), private, parameter :: error_occurred = errstat_status_type(.false.)
        !! a constant representing a status of error-occurrence.

    !>An user-defined derived type to list possible statuses.
    type, private :: errstat_statuses_type
        type(errstat_status_type) :: succeeded
            !! a status of success.
        type(errstat_status_type) :: error_occurred
            !! a status of error-occurrence.
    end type errstat_statuses_type

    type(errstat_statuses_type), public, parameter :: &
        statuses = errstat_statuses_type(succeeded=succeeded, &
                                         error_occurred=error_occurred)
        !! A constant list to enable users to select possible statuses.

contains
    !>Returns `.true.` if a status code `stat` represents `status`.
    logical function is_status_i4(stat, status)
        implicit none
        integer(int32), intent(in) :: stat
            !! an error status code>
        type(errstat_status_type), intent(in) :: status
            !! a status (success or error-occurrence).

        if (status == succeeded) then
            is_status_i4 = (stat == success_status_code)
        else if (status == error_occurred) then
            is_status_i4 = (stat /= success_status_code)
        end if
    end function is_status_i4

    !>Returns `.true.` if a message `msg` represents `status`.
    logical function is_status_str(msg, status)
        implicit none
        character(*), intent(in) :: msg
            !! an error message
        type(errstat_status_type), intent(in) :: status
            !! a status (success or error-occurrence).

        if (status == succeeded) then
            is_status_str = (msg == success_status_msg)
        else if (status == error_occurred) then
            is_status_str = (msg /= success_status_msg)
        end if
    end function is_status_str

    !>Returns `.true.` if lhs and rhs represents the same status.
    logical function errstat_status_is_status_equal(lhs, rhs)
        implicit none
        class(errstat_status_type), intent(in) :: lhs
            !! passed dummy argument and left-hand side value of comparator.
        type(errstat_status_type), intent(in) :: rhs
            !! right-hand side value of comparator.

        errstat_status_is_status_equal = (lhs%status .eqv. rhs%status)
    end function errstat_status_is_status_equal

    !>Returns `.true.` if lhs and rhs do not represent the same status.
    logical function errstat_status_is_status_not_equal(lhs, rhs)
        implicit none
        class(errstat_status_type), intent(in) :: lhs
            !! passed dummy argument and left-hand side value of comparator.
        type(errstat_status_type), intent(in) :: rhs
            !! right-hand side value of comparator.

        errstat_status_is_status_not_equal = .not. (lhs == rhs)
    end function errstat_status_is_status_not_equal
end module errstat_proc_isStatus
