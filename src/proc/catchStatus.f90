!>The `errstat_proc_catchStatus` provides procedures relative to catching status.
!>
!>The procedures include subroutines to catch and reflect error status
!>to `intent(out)/intent(inout)` primitive variables arguments
!>with different `intent(in)` arguments.
!>
module errstat_proc_catchStatus
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: catch_status

    !>Set an error status code and error message to arguments.
    interface catch_status
        procedure :: catch_status_w_code
        procedure :: catch_status_w_code_msg
        procedure :: catch_status_w_code_getter
    end interface

contains

    !>Sets `stat` and `msg` from `stat_code` and `err_msg`, respectively.
    !>After setting those, additional task is executed
    !>if `additional_task` is passed.
    subroutine catch_status_w_code_msg(stat_code, err_msg, stat, msg &
                                       , additional_task)
        use :: errstat_type_task_adt
        use :: errstat_proc_isStatus
        use :: errstat_constant_status
        implicit none
        !&<
        integer(int32)              , intent(in)                :: stat_code
            !! an error status code
        character(*)                , intent(in)                :: err_msg
            !! an error message
        integer(int32)              , intent(out)   , optional  :: stat
            !! a variable to store status code
        character(:), allocatable   , intent(out)   , optional  :: msg
            !! a string to store error message
        ! If passing `msg` to procedures called from this procedure,
        ! must change the intent attribute from `(out)` to `(inout)`.
        class(task_type)            , intent(in)    , optional  :: additional_task
            !! additional task to be executed
            !! after setting `stat` and `msg`
        !&>

        if (present(stat)) &
            stat = stat_code

        if (present(msg)) then
            msg = err_msg
            if (stat_code == success_status_code) &
                msg = success_status_msg
        end if

        if (present(additional_task)) &
            call additional_task%execute(stat=stat, msg=msg)
    end subroutine catch_status_w_code_msg

    !>Sets `stat` from `stat_code`.
    !>After setting it, additional task is executed
    !>if `additional_task` is passed.
    subroutine catch_status_w_code(stat_code, stat, msg &
                                   , additional_task)
        use :: errstat_type_task_adt
        use :: errstat_proc_isStatus
        use :: errstat_constant_status, only:not_specified_msg
        implicit none
        !&<
        integer(int32)                  , intent(in)                :: stat_code
            !! an error status code
        integer(int32)                  , intent(out)   , optional  :: stat
            !! a variable to store status code
        character(:)    , allocatable   , intent(inout) , optional  :: msg
            !! a string to store error message
        ! intent(out) causes runtime error when compiling with gfortran.
        class(task_type)                , intent(in)    , optional  :: additional_task
            !! additional task to be executed
            !! after setting `stat` and `msg`
        !&>

        call catch_status_w_code_msg(stat_code, not_specified_msg &
                                     , stat, msg &
                                     , additional_task)
    end subroutine catch_status_w_code

    !>Sets `stat` from `stat_code`, and set `msg` from callback procedure `get_message`.
    !>After setting those, additional task is executed
    !>if `additional_task` is passed.
    subroutine catch_status_w_code_getter(stat_code, get_message, stat, msg &
                                          , additional_task)
        use :: errstat_type_task_adt
        use :: errstat_proc_isStatus
        use :: errstat_interface_getErrorMessage
        implicit none
        !&<
        integer(int32)                  , intent(in)                :: stat_code
            !! an error status code
        procedure(Iget_error_message)                               :: get_message
            !! procedure to get the error message from the status code
        integer(int32)                  , intent(out)   , optional  :: stat
            !! a variable to store status code
        character(:), allocatable       , intent(inout) , optional  :: msg
            !! a string to store error message
        ! intent(out) causes runtime error when compiling with gfortran
        class(task_type)                , intent(in)    , optional  :: additional_task
            !! additional task to be executed
            !! after setting `stat` and `msg`
        !&>

        call catch_status_w_code_msg(stat_code, get_message(stat_code) &
                                     , stat, msg &
                                     , additional_task)
    end subroutine catch_status_w_code_getter
end module errstat_proc_catchStatus
