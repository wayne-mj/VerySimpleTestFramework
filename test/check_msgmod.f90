module check_msgmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_msg()
        call suite("This is the suite")
        call test("This is a test")
        call results()
    end subroutine
end module check_msgmod