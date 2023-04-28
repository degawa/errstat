!| The `errstat` module aggregates the constans and procedures
! contained in the errstat library and exposes them to the users.
!
! When use the errstat library, the users only need to use this module
! like `use :: errstat`.
!
module errstat
    use :: errstat_type_errorStat
    use :: errstat_type_task_adt
    use :: errstat_constant_status
    use :: errstat_proc_catchStatus
    use :: errstat_proc_catchError
    use :: errstat_proc_isStatus
    use :: errstat_proc_errorOccurred
    use :: errstat_interface_getErrorMessage
    use :: errstat_repository_message
    private
    ! types
    public :: error_stat_type
    public :: task_type
    public :: message_repository_atype
    ! procedures
    public :: catch_status
    public :: catch_error
    public :: set_success
    public :: error_occurred
    public :: has_message
    public :: is_status
    public :: stat_to_string
    ! parameters
    public :: success_status_code
    public :: success_status_msg
    public :: statuses
    ! interfaces
    public :: Iget_error_message
end module errstat
