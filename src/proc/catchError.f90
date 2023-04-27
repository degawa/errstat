!>The `errstat_proc_catchError` provides procedures relative to catching a error.
!>
!>The procedures include subroutines to catch and reflect error status
!>to an `intent(inout)` `error_stat_type` argument with different `intent(in)` arguments.
!>
module errstat_proc_catchError
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: catch_error
    public :: set_success

    !>Set an error status code and error message to an `error_stat_type` argument.
    interface catch_error
        procedure :: catch_error_w_code_msg
        procedure :: catch_error_w_code_getter
    end interface

contains

    !>Sets `err_stat` from `stat_code` and `err_msg`.
    !>After setting those, additional task is executed
    !>if `additional_task` is passed.
    subroutine catch_error_w_code_msg(stat_code, err_msg, err_stat &
                                      , additional_task)
        use :: errstat_type_task_adt
        use :: errstat_type_errorStat
        implicit none
        !&<
        integer(int32)          , intent(in)                :: stat_code
            !! an error status code
        character(*)            , intent(in)                :: err_msg
            !! an error message
        type(error_stat_type)   , intent(inout) , optional  :: err_stat
            !! an object for handling an error status and message.
        class(task_type)        , intent(in)    , optional  :: additional_task
            !! additional task to be executed
            !! after setting `stat_code` and `err_msg`
        !&>

        if (present(err_stat)) then
            call err_stat%set_status_and_message(stat_code, err_msg)
        end if

        if (present(additional_task)) &
            call additional_task%execute()
    end subroutine catch_error_w_code_msg

    !>Sets a status of `err_stat` from `stat_code` and
    !>a message of `err_stat` from callback procedure `get_message`.
    !>After setting those, additional task is executed
    !>if `additional_task` is passed.
    subroutine catch_error_w_code_getter(stat_code, get_message, err_stat &
                                         , additional_task)
        use :: errstat_type_task_adt
        use :: errstat_type_errorStat
        use :: errstat_interface_getErrorMessage
        implicit none
        !&<
        integer(int32)                  , intent(in)                :: stat_code
            !! an error status code
        procedure(Iget_error_message)                               :: get_message
            !! procedure to get the error message from the status code
        type(error_stat_type)           , intent(inout) , optional  :: err_stat
            !! an object for handling an error status and message.
        class(task_type)                , intent(in)    , optional  :: additional_task
            !! additional task to be executed
            !! after setting `stat` and `msg`
        !&>

        call catch_error_w_code_msg(stat_code, get_message(stat_code), &
                                    err_stat, additional_task)
    end subroutine catch_error_w_code_getter

    !>Sets `error_stat_type` object as the success status.
    !>This procedure
    !>
    !>- set `stat` to `success_status_code`
    !>- set `msg` to `success_status_msg`
    !>- set `does_error_occur` to `.false.`
    !>
    subroutine set_success(err_stat)
        use :: errstat_type_errorStat
        use :: errstat_constant_status
        implicit none
        type(error_stat_type), intent(inout), optional :: err_stat
            !! an object for handling an error status and message.

        if (present(err_stat)) &
            call err_stat%set_status_and_message(success_status_code, success_status_msg)
    end subroutine set_success
end module errstat_proc_catchError
