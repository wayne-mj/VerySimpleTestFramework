module check_assertbasemod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertbase()
        call suite("Assert Pass/Fail")

        call test("Assert Pass")
        call assert_pass()

        call test("Assert Fail")
        call assert_fail()
        
        call results()
    end subroutine
end module check_assertbasemod