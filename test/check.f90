program check
    use iso_fortran_env
    use check_msgmod
    use check_assertequalsmod
    use check_assertcontainsmod
    use check_assertbasemod
    use check_assertlogicalmod
    use check_assertzeromod
    use check_assertnotzeromod
    use check_assertzerolengthmod
    use check_assertnullmod
    use check_assertlgemod
    use check_assertposnegmod
    use VerySimpleTestFramework
    implicit none

    call check_msg()
    call check_assertbase()
    call check_assertequals()
    call check_assertcontains()
    call check_assertlogical()
    call check_assertzero()
    call check_assertnotzero()
    call check_assertzerolength()
    call check_assertnull()
    call check_assertlge()
    call check_assertposneg()

end program check
