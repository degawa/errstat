!| The `errstat_type_task_adt` module provides abstract data type
! relateve to additional task in the error or status catching procedures.
!
module errstat_type_task_adt
    use, intrinsic :: iso_fortran_env
    implicit none
    private

    !| An abstract data type to define additional task executed
    ! in [[catch_status]] and [[catch_error]].
    type, public, abstract :: task_type
    contains
        procedure(ItaskExecute), public, pass, deferred :: execute
            !! procedure to execute concrete task.
    end type task_type

    abstract interface
        !| An interface for type-bound procedure `task_type%execute`
        subroutine ItaskExecute(this)
            import task_type
            class(task_type), intent(in) :: this
                !! passed dummy argument
        end subroutine ItaskExecute
    end interface
end module errstat_type_task_adt
