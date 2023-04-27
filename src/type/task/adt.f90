!>The `errstat_type_task_adt` module provides abstract data type
!>relateve to additional task in the error or status catching procedures.
!>
module errstat_type_task_adt
    use :: errstat_type_errorStat
    implicit none
    private

    !>An abstract data type to define additional task executed
    !>in [[catch_status]] and [[catch_error]].
    type, public, abstract :: task_type
    contains
        procedure(ItaskExecute), public, pass, deferred :: execute
        !*executes concrete task
    end type task_type

    abstract interface
        !>An interface for type-bound procedure `task_type%execute`
        subroutine ItaskExecute(this, err, stat, msg)
            use, intrinsic :: iso_fortran_env
            import task_type
            import error_stat_type
            class(task_type), intent(in) :: this
                !! passed-object dummy argument.
            type(error_stat_type), intent(inout), optional :: err
                !! error status
            integer(int32), intent(in), optional :: stat
                !! error status
            character(*), intent(inout), optional :: msg
                !! error message
        end subroutine ItaskExecute
    end interface
end module errstat_type_task_adt
