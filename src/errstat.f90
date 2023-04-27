!| The `errstat` module aggregates the constans and procedures
! contained in the errstat library and exposes them to the users.
!
! When use the errstat library, the users only need to use this module
! like `use :: errstat`.
!
module errstat
    use :: errstat_type_errorStat, only: &
        error_stat_type
    use :: errstat_type_task_adt, only: &
        task_type
    use :: errstat_constant_status, only: &
        success_status_code, &
        success_status_msg
    use :: errstat_proc_catchStatus, only: &
        catch_status
    use :: errstat_proc_catchError, only: &
        catch_error, set_success
    use :: errstat_proc_isStatus, only: &
        is_status, statuses
    use :: errstat_proc_doesErrorOccurred, only: &
        error_occurred
    use :: errstat_interface_getErrorMessage, only: &
        Iget_error_message
    implicit none
end module errstat
