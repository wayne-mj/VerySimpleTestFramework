module check_assertnotzeromod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertnotzero()
        call suite("Assert Not Zero")

        call test("Assert Not Zero: 0 is not 0")
        call assert_not_zero(0)

        call test("Assert Not Zero: 1 is not 0")
        call assert_not_zero(1)

        call test("Assert Not Zero: 0. is not 0.")
        call assert_not_zero(0.)

        call test("Assert Not Zero: 1. is not 0.")
        call assert_not_zero(1.)

        call test("Assert Not Zero: (0.,0.) is  not (0.,0.)")
        call assert_not_zero((0.,0.))

        call test("Assert Not Zero: (1.,1.) is not (0.,0.)")
        call assert_not_zero((1.,1.))

        call test("Assert Not Zero: (0.,1.) is not (0.,0.)")
        call assert_not_zero((0.,1.))

        call test("Assert Not Zero: (1.,0.) is not (0.,0.)")
        call assert_not_zero((1.,0.))
        
        call results()
    end subroutine
end module check_assertnotzeromod