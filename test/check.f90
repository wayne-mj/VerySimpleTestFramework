program check
    use iso_fortran_env
    use check_msgmod
    use check_assertequalsmod
    use check_assertcontainsmod
    use check_assertbasemod
    use check_assertlogicalmod
    use check_assertzeromod
    use VerySimpleTestFramework
    implicit none

    call check_msg()
    call check_assertbase()
    call check_assertequals()
    call check_assertcontains()
    call check_assertlogical()
    call check_assertzero()

end program check
