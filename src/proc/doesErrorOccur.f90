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
    public :: does_error_occur

contains

    !>Returns `.true.` if an `error_stat_type` object caught an error.
    logical function does_error_occur(err)
        use :: errstat_proc_isStatus
        implicit none
        type(error_stat_type), intent(in) :: err
            !! an error status object.

        does_error_occur = err%get_error_occurrence_flag()
    end function does_error_occur
end module errstat_proc_doesErrorOccurred
