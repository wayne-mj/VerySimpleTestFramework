module check_assertlogicalmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertlogical()
        call suite("Assert True/False")

        call test("Assert True: True is True")
        call assert_true(.true.)

        call test("Assert True: False is True")
        call assert_true(.false.)


        call test("Assert False: False is False")
        call assert_false(.false.)

        call test("Assert False: True is False")
        call assert_false(.true.)
        
        call results()
    end subroutine
end module check_assertlogicalmod