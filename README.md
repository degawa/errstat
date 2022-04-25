# errstat - error status and message handling library for Modern Fortran

## Overview
The errstat allows us to simplify the Fortran-style error handling.

In the Fortran-style error handling, a status code and message of
a processing result is acquired as `intent(out)` arguments, like

```Fortran
real(real32), allocatable :: f(:)
integer(int32) :: stat
character(128) :: msg

allocate (f(10))
allocate (f(10), stat=stat, errmsg=msg)
if (stat /= 0) then
    print *, "an error occurred: ", msg
end if
```

Compile the above with some Fortran compilers and run the executables; the results are
` an error occurred: Attempt to allocate an allocated object`(gnu gfortran)
` an error occurred: allocatable array is already allocated` (intel fortran)
` an error occurred: 割付け変数が割付け済みです` (nag fortran)

For user-defined procedures, it is troublesome to add `optional` arguments for the status code and message and to write `if` statements to branch according to those existing or not.

```Fortran
function inverse(x, stat, msg) result(y)
    use, intrinsic :: iso_fortran_env
    use, intrinsic :: ieee_arithmetic
    implicit none
    real(real64) :: x
    integer(int32), intent(out), optional :: stat
    character(:), allocatable, intent(out), optional :: msg

    real(real64) :: y

    y = 1d0/x

    if (ieee_is_nan(y)) then
        if (present(stat)) stat = 1
        if (present(msg)) msg = "the result is not-a-number"
        return
    end if

    if (.not. ieee_is_finite(y)) then
        if (present(stat)) stat = 2
        if (present(msg)) msg = "the result is infinite"
        return
    end if

    if (present(stat)) stat = 0
    if (present(msg)) msg = ""
end function inverse
```

The errstat simplifies the `if` statements as below:

```Fortran
    if (ieee_is_nan(y)) then
        call catch_status(1, "the result is not-a-number", stat, msg)
        return
    end if

    if (.not. ieee_is_finite(y)) then
        call catch_status(2, "the result is infinite", stat, msg)
        return
    end if

    call catch_status(success_status_code, success_status_msg, stat, msg)
```

Not only the simplification of the `if` statements but also the error handling on the caller becomes clearer.

```Fortran
    y = inverse(0d0, stat, msg)
    if (is_status(stat, statuses%error_occurred)) then
        print *, msg
    end if
```

The errstat also provides a user-defined derived type `error_stat_type` that combines the `stat` and `msg`.

The error handling can be more simplified.

```Fortran
    if (ieee_is_nan(y)) then
        call catch_error(1, "the result is not-a-number", err)
        return
    end if

    if (.not. ieee_is_finite(y)) then
        call catch_error(2, "the result is infinite", err)
        return
    end if

    call set_success(err)
```

```Fortran
    type(error_stat_type) :: err

    y = inverse(1d0, err)
    if (does_error_occur(err)) then
        print *, err%get_message()
    end if
```

## Usage
### Requirement
- A Fortran compiler
    - The library is tested using gfortran 10.3.0, intel fortran 2021.1, and nag fortran 7.1 on Windows 10.
- fpm
    - The library supports fpm (fortran-lang/fpm) for build. fpm 0.5.0, alpha is used.
- FORD (optional)
    - FORD is used for generating the API document.

### Get the code
To get the code, execute the following commnad:

```
git clone https://github.com/degawa/errstat.git
cd errstat
```

### Build with fpm
To build the library using fpm, execute the following command:

```
fpm build
```

### API Document
The API documentation can be generated using FORD.

```
ford api-doc-ford-settings.md
```

### Reference as a fpm project's dependency
To use the errstat in your fpm project, add the following to `fpm.toml`.

```toml
[dependencies]
errstat = {git = "https://github.com/degawa/errstat"}
```
