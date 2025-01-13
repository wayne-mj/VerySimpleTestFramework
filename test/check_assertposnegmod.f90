module check_assertposnegmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertposneg()
        call suite("Assert Positive/Negative")

        call test("Positive 1")
        call assert_is_positive(1)


        call test("Positive -1")
        call assert_is_positive(-1)


        call test("Positive 0")
        call assert_is_positive(0)

        call test("Positive 1.")
        call assert_is_positive(1.)


        call test("Positive -1")
        call assert_is_positive(-1.)


        call test("Positive 0")
        call assert_is_positive(0.)

        ! ************************************

        call test("Negative 1")
        call assert_is_negative(1)


        call test("Negative -1")
        call assert_is_negative(-1)


        call test("Negative 0")
        call assert_is_negative(0)

        call test("Negative 1.")
        call assert_is_negative(1.)


        call test("Negative -1")
        call assert_is_negative(-1.)


        call test("Negative 0")
        call assert_is_negative(0.)
        
        call results()
    end subroutine
end module check_assertposnegmod