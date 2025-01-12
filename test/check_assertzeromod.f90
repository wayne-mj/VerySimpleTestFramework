module check_assertzeromod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertzero()
        call suite("Assert Zero")

        call test("0 is 0")
        call assert_zero(0)

        call test("1 is 0")
        call assert_zero(1)

        call test("0. is 0.")
        call assert_zero(0.)

        call test("1. is 0.")
        call assert_zero(1.)

        call test("(0.,0.) is (0.,0.)")
        call assert_zero((0.,0.))

        call test("(1.,1.) is (0.,0.)")
        call assert_zero((1.,1.))

        call test("(0.,1.) is (0.,0.)")
        call assert_zero((0.,1.))

        call test("(1.,0.) is (0.,0.)")
        call assert_zero((1.,0.))
        
        call results()
    end subroutine
end module check_assertzeromod