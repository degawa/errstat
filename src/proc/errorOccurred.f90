!>The `errstat_proc_isOccurred` provides a procedure related to
!>an error occurrence.
!>
!>The procedure is a function to check an error occurred from
!>an `error_stat_type` object component.
!>
module errstat_proc_doesErrorOccurred
    use, intrinsic :: iso_fortran_env
    use :: errstat_type_errorStat
    implicit none
    private
    public :: error_occurred

contains

    !>Returns `.true.` if an `error_stat_type` object caught an error.
    logical function error_occurred(err)
        use :: errstat_proc_isStatus
        implicit none
        type(error_stat_type), intent(in) :: err
            !! an error status object.

        error_occurred = err%error_occurred()
    end function error_occurred
end module errstat_proc_doesErrorOccurred
