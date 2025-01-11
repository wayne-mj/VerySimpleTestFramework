module check_msgmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_msg()
        call suite("This is a test of the messages")
        call test("This is a test message")
        call results()
    end subroutine
end module check_msgmod