program check
    use iso_fortran_env
    use check_msgmod
    use check_assertequalsmod
    implicit none

    call check_msg()
    call check_assertequals()

end program check
