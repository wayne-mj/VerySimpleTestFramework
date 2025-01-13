module check_assertzerolengthmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertzerolength()
        call suite("Assert Zero Length and Non Zero Length")

        call test("Assert Zero Length: Empty String")
        call assert_zero_length("")

        call test("Assert Zero Length: Not an empty string")
        call assert_zero_length(".")

        call test("Assert Non Zero Length: Empty String")
        call assert_non_zero_length("")

        call test("Assert Non Zero Length: Not an empty string")
        call assert_non_zero_length(".")
        
        call results()
    end subroutine
end module check_assertzerolengthmod