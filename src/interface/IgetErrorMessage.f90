!| The `errstat_interface_getErrorMessage` provides an abstract interface
! related to handle an error status.
!
! The abstract interface defines the interface of callback functions
! to get a error message string from outside of the procedures
! [[catch_status]] and [[catch_error]].
!
module errstat_interface_getErrorMessage
    use, intrinsic :: iso_fortran_env
    implicit none
    private
    public :: Iget_error_message

    interface
        !| interfaces to get a error message string.
        function Iget_error_message(stat) result(err_msg)
            use, intrinsic :: iso_fortran_env
            integer(int32), intent(in) :: stat
                !! an error status code.
            character(:), allocatable :: err_msg
                !! an error message string.
        end function Iget_error_message
    end interface

end module errstat_interface_getErrorMessage
