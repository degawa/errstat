!| The `errstat_constant_status` provides constants related to error status.
!
! The constants include the status code and the message when a processing is succeeded.
! An alternative message is also defined. This will pass internally to
! a procedure [[catch_status_w_code_msg]] when [[catch_status]] is called
! so that the error message is not retrieved.
!
module errstat_constant_status
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: iso_c_binding
    implicit none
    private

    enum, bind(c)
        enumerator :: ERRSTAT_Default_Success_Code = 0
    end enum

    integer(int32), public, parameter :: success_status_code = ERRSTAT_Default_Success_Code
        !! The status code when a processing is succeeded.
    character(*), public, parameter :: success_status_msg = ""
        !! The message when a processing is succeeded.
    character(*), public, parameter :: not_specified_msg = "Message Not Specified"
        !! An message to pass a procedure [[catch_status_w_code_msg]]
        !! when [[catch_status]] is called so that the error message is not retrieved.

end module errstat_constant_status
