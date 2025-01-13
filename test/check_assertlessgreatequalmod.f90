module check_assertlgemod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertlge()
        call suite("Assert Less/Greater/Equal")

        call test("Assert Less Than: 1 < 2")
        call assert_less_than(1, 2)

        call test("Assert Less Than: 2 < 2")
        call assert_less_than(2, 2)

        call test("Assert Less Than: 1. < 2.")
        call assert_less_than(1., 2.)

        call test("Assert Less Than: 2. < 2.")
        call assert_less_than(2., 2.)

        ! **************************************

        call test("Assert Greater Than: 2 > 1")
        call assert_greater_than(2, 1)

        call test("Assert Greater Than: 2 > 2")
        call assert_greater_than(2, 2)

        call test("Assert Greater Than: 2. > 1.")
        call assert_greater_than(2., 1.)

        call test("Assert Greater Than: 2. > 2.")
        call assert_greater_than(2., 2.)
        
        ! **************************************

        call test("Assert Less Equal: 1 <= 2")
        call assert_less_equal(1, 2,0)

        call test("Assert Less Than: 2 <= 2")
        call assert_less_equal(2, 2,0)

        call test("Assert Less Than: 3 <= 2")
        call assert_less_equal(3, 2,0)

        call test("Assert Less Than: 1. <= 2.")
        call assert_less_equal(1., 2.,0.)

        call test("Assert Less Than: 2. <= 2.")
        call assert_less_equal(2., 2.,0.)

        call test("Assert Less Than: 3. <= 2.")
        call assert_less_equal(3., 2.,0.)

        ! **************************************

        call test("Assert Greater Equal: 1 >= 2")
        call assert_greater_equal(1, 2,0)

        call test("Assert Greater Than: 2 >= 2")
        call assert_greater_equal(2, 2,0)

        call test("Assert Greater Than: 3 >= 2")
        call assert_greater_equal(3, 2,0)

        call test("Assert Greater Than: 1. >= 2.")
        call assert_greater_equal(1., 2.,0.)

        call test("Assert Greater Than: 2. >= 2.")
        call assert_greater_equal(2., 2.,0.)

        call test("Assert Greater Than: 3. >= 2.")
        call assert_greater_equal(3., 2.,0.)
        

        call results()
    end subroutine
end module check_assertlgemod