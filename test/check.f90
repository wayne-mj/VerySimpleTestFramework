program check
    use iso_fortran_env
    use check_msgmod
    use check_assertequalsmod
    use check_assertcontainsmod
    implicit none

    call check_msg()
    call check_assertequals()
    call check_assertcontains()

end program check
