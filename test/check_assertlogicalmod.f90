module check_assertlogicalmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertlogical()
        call suite("Assert True/False")

        call test("True is True")
        call assert_true(.true.)

        call test("False is True")
        call assert_true(.false.)


        call test("False is False")
        call assert_false(.false.)

        call test("True is False")
        call assert_false(.true.)
        
        call results()
    end subroutine
end module check_assertlogicalmod