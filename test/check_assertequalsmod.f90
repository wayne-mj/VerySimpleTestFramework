module check_assertequalsmod
    use iso_fortran_env
    use VerySimpleTestFramework

    implicit none

    contains
    subroutine check_assertequals()
        call suite("Assert Equals")
        
        call test("Assert Equals: 1=1 Tolerance 0")
        call assert_equals(1,1,0)

        call test("Assert Equals: 1=2 Tolerance 0")
        call assert_equals(1,2,0)

        call test("Assert Equals: 1.=1. Tolerance 0.")
        call assert_equals(1.,1.,0.)

        call test("Assert Equals: 1.=2. Tolerance 0.")
        call assert_equals(1.,2.,0.)

        call test("Assert Equals: (1.,1.)=(1.,1.) Tolerance 0.")
        call assert_equals((1.,1.),(1.,1.),0.)

        call test("Assert Equals: (1.,1.)=(2.,1.) Tolerance 0.")
        call assert_equals((1.,1.),(2.,1.),0.)

        call test("Assert Equals: (1.,1.)=(1.,2.) Tolerance 0.")
        call assert_equals((1.,1.),(1.,2.),0.)

        call test("Assert Equals: (1.,1.)=(2.,2.) Tolerance 0.")
        call assert_equals((1.,1.),(2.,2.),0.)

        call test("Assert Equals: C=C")
        call assert_equals("C", "C")

        call test("Assert Equals: C=c")
        call assert_equals("C", "c")

        call test("Assert Equals: C=A")
        call assert_equals("C", "A")

        call results()
    end subroutine
end module check_assertequalsmod