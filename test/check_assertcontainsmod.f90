module check_assertcontainsmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertcontains()
        call suite("Assert Contains")
        
        call test("Assert Contains: Hello World contains World")
        call assert_contains("Hello World", "World")

        call test("Assert Contains: Hello World contains o W")
        call assert_contains("Hello World", "o W")

        call test("Assert Contains: Hello World contains O")
        call assert_contains("Hello World", "O")

        call test("Assert Contains: H contains H")
        call assert_contains("H", "H")

        call test("Assert Contains: H contains h")
        call assert_contains("H", "h")

        call results()
    end subroutine
end module check_assertcontainsmod