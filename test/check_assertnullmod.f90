module check_assertnullmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none
        character(len=1)    ::  NULL = char(0)

    contains
    subroutine check_assertnull()
        call suite("Assert Null/NotNull")

        call test("Assert is Null: NULL")
        call assert_is_null(NULL)

        call test("Assert is Null: .")
        call assert_is_null(".")

        call test("Assert is not Null: NULL")
        call assert_is_not_null(NULL)

        call test("Assert is not Null: .")
        call assert_is_not_null(".")
        
        call results()
    end subroutine
end module check_assertnullmod