module VerySimpleTestFramework
  use iso_fortran_env
  
  implicit none
  private 
  
  public :: suite, test, results, &
            assert_pass, assert_fail, &
            assert_equals, assert_contains, &
            assert_true, assert_false, &
            assert_zero, assert_not_zero, &
            assert_non_zero_length, assert_zero_length 
  
  !> Required constant parameters
  character(len=1), parameter     :: cr   = char(13)              ! Carriage return
  character(len=1), parameter     :: lf   = char(10)              ! Line feed
  character(len=1), parameter     :: ESC  = char(27)              ! Escape
  character(len=1), parameter     :: TAB  = char(9)               ! Horizontal Tab
  character(len=1), parameter     :: NULL = char(0)               ! Null
  character(len=3), parameter     :: SPC  = "   "  ! CR/LF and 3 Spaces
  
  !> ANSI Colour codes
  character(len=5), parameter     :: cred     = ESC // "[31m "
  character(len=5), parameter     :: cgreen   = ESC // "[32m "
  character(len=5), parameter     :: ccyan    = ESC // "[36m "
  character(len=5), parameter     :: cclear   = ESC // "[0m "
  character(len=5), parameter     :: cyellow  = ESC // "[93m "

  !> Predefined messages and feedback
  character(len=22), parameter    :: FAILEDMSG  =  cr // cred   //  SPC   // "X FAILED"       // cclear
  character(len=16), parameter    :: EXPMSG     =                   SPC   // "  Expected: "
  character(len=16), parameter    :: ACTMSG     =                   SPC   // "    Actual: "
  character(len=16), parameter    :: TOLMSG     =                   SPC   // " Tolerance: "
  character(len=22), parameter    :: PASSMSG    =  cr // cgreen //  SPC   // "✓ PASSED"       // cclear

  integer, parameter  ::  AEIF  =   1       ! Assert Integer Fail
  integer, parameter  ::  AERF  =   2       !        Real
  integer, parameter  ::  AECF  =   3       !        Complex
  integer, parameter  ::  AELF  =   4       !        Logical
  integer, parameter  ::  AESF  =   5       !        String
  
  !> Tracking of failed and total tests
  integer :: failed               ! = 0
  integer :: total_tests          ! = 0

  !> Variables for tracking the start and finish times and related time seeds.
  integer(int64) :: start_time    ! = 0
  integer(int64) :: finish_time   ! = 0
  integer(int64) :: c, c_r, cm

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  !> Special flags, paramaters etc go here
  !> Flag set for debug statements to print of debug messages
  logical :: DBG = .false.
  !>
  !>
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  !> Data type for passing values related to strings between procedures and functions
  type stringtype
    character(:), allocatable :: actual, expected               ! String Actual vs Expected
    integer                   :: alength, elength               ! Length of Actual vs Expcted including leading and trailing white spaces
  end type

  type error_msg_type
    integer                   :: error_cat                      ! Error category
    character(:), allocatable :: s_actual, s_expected           ! String ACTUAL/EXPECTED
    integer                   :: s_alength, s_elength           ! String Actual Length vs Expected Length
    integer                   :: i_actual, i_expected, i_tol    ! Integer Actual vs Expected within Tolerance
    real                      :: r_actual, r_expected, r_tol    ! Real Actual vs Expected within Tolerance
    complex                   :: c_actual, c_expected           ! Complex Actual vs Expected within Tolerance
    real                      :: c_tol                          !
    logical                   :: l_actual, l_expected           ! Logical Actual vs Expected
    character(:), allocatable :: msg                            ! Message to be displayed if any
    integer                   :: msglength                      ! Length of the message
    logical                   :: show_tol = .true.              ! Show the tolerance values
    logical                   :: show_exp = .true.              !          expected
  end type
  
  !> Interface for the suite(s)
  !> Originally had multiples to include a decorated header
  interface suite
    module procedure  suites
  end interface

  interface build_error_body
    module procedure  build_error_body_INT, &
                      build_error_body_REAL, &
                      build_error_body_COMPLEX, &
                      build_error_body_LOGICAL, &
                      build_error_body_STRING
  end interface

  interface assert_equals
    module procedure  assert_equals_real, &
                      assert_equals_complex, &
                      assert_equals_int, & 
                      assert_equals_str    
  end interface

  interface assert_zero
    module procedure  assert_zero_int, &
                      assert_zero_real, &
                      assert_zero_complex
  end interface

  interface assert_not_zero
    module procedure  assert_not_zero_int, &
                      assert_not_zero_real, &
                      assert_not_zero_complex
  end interface

contains
  !>---------------------------------------------------------------------------------------------------<!
  !> Prints a "banner" type message to denote that there a suite of tests
  !> This also initialises all the required variables prior to use
  subroutine suites(name)
    character(*), intent(in) :: name

    print *, cr // lf // ccyan // "** Test Suite: ", name // " ** "// cclear !// cr // lf
    
    failed = 0
    total_tests = 0
    start_time = 0
    finish_time = 0

    call system_clock(c, c_r, cm)
    call system_clock(start_time)
  end subroutine

  !> Used to signpost each of the tests
  subroutine test(name)
    character(*), intent(in) :: name

    ! print *, "Running test: ", name
    print *, cr // lf // cyellow // "Running test: " // name // cclear !// cr // lf
    total_tests = total_tests + 1
  end subroutine

  !> Display the result of the tests after completion.
  subroutine results()
    real :: total_time = 0.
    
    call system_clock(finish_time)
    total_time = ((finish_time - start_time) / real(c_r))
    print *, "Summary: ", total_tests, " tests, ", failed, " failed."
    print *, "Total time in sec: ", total_time
  end subroutine
  !>---------------------------------------------------------------------------------------------------<!
  
  !> Display failed results for tests using the datatype ERROR_MSG_TYPE
  !> This contains primatives of the datatypes required
  subroutine display_failed_message(error)
    type(error_msg_type)  :: error

    if (DBG) then
      print *, cyellow // "DISPLAY FAILED MESSAGE" // cclear
    end if

    if (error%msglength .gt. 0) then
      print *, FAILEDMSG , error%msg
    else 
      print *, FAILEDMSG
    end if

    !> 1, 2, 3 etc ASSERT EQUALS INT, REAL, COMPLEX etc
    !> Assert INTEGER fail
    if (error%error_cat .eq. AEIF) then
      call display_failed_int(error)
    !> Assert REAL fail
    else if (error%error_cat .eq. AERF) then
      call display_failed_real(error)
    !> Assert COMPLEX fail
    else if (error%error_cat .eq. AECF) then
      call display_failed_complex(error)
    !> Assert CHARACTER string fail
    else if (error%error_cat .eq. AESF) then
      call display_failed_str(error)
    !> Assert LOGICAL fail
    else if (error%error_cat .eq. AELF) then
      call display_failed_logical(error)
    end if
  end subroutine

  !> Display failed results for values of INTEGERS
  subroutine display_failed_int(error)
    type(error_msg_type) :: error 

    if (DBG) then
      print *, cyellow // "INT CALLED INTERNALLY" // cclear
    end if

    print *, ACTMSG, error%i_actual
    if(error%show_exp) then
      print *, EXPMSG, error%i_expected
    end if

    if (error%show_tol) then
      print *, TOLMSG, error%i_tol
    end if
  end subroutine

  !> Display failed results for values of REALS
  subroutine display_failed_real(error)
    type(error_msg_type) :: error

    if (DBG) then
      print *, cyellow // "REAL CALLED INTERNALLY" // cclear
    end if

    print *, ACTMSG, error%r_actual
    if (error%show_exp) then
      print *, EXPMSG, error%r_expected
    end if

    if (error%show_tol) then
      print *, TOLMSG, error%r_tol
    end if
  end subroutine

  !> Display failed results for values of COMPLEX 
  subroutine display_failed_complex(error)
    type(error_msg_type) :: error

    if (DBG) then
      print *, cyellow // "COMPLEX CALLED INTERNALLY" // cclear
    end if

    print *, ACTMSG, error%c_actual
    if (error%show_exp) then
      print *, EXPMSG, error%c_expected
    end if

    if (error%show_tol) then
      print *, TOLMSG, error%c_tol
    end if
  end subroutine

  !> Display failed results for values of CHARACTER strings
  subroutine display_failed_str(error)
    type(error_msg_type) :: error
    
    if (DBG) then
      print *, cyellow // "STR CALLED INTERNALLY" // cclear
    end if

    print *, ACTMSG, error%s_actual
    print *, ACTMSG, error%s_alength
    print *, EXPMSG, error%s_expected
    print *, EXPMSG, error%s_elength
  end subroutine

  !> As above but uses a datatype for CHARACTERS to collect common results together
  subroutine display_failed_strdata(msg, strdata)
    character(*), intent(in)  :: msg
    type(stringtype)          :: strdata
    
    if (DBG) then
      print *, cyellow // "STRDATA CALLED INTERNALLY" // cclear
    end if

    if (len(msg) .gt. 0) then
      print *, FAILEDMSG, msg
    else 
      print *, FAILEDMSG
    end if

    print *, ACTMSG, strdata%actual
    print *, ACTMSG, strdata%alength    
    print *, EXPMSG, strdata%expected
    print *, EXPMSG, strdata%elength
  end subroutine

  !> Display the failed results of LOGICAL tests
  subroutine display_failed_logical(error)
    type(error_msg_type) :: error
    
    if (DBG) then
      print *, cyellow // "LOGICAL CALLED INTERNALLY" // cclear
    end if

    print *, ACTMSG, error%l_actual
    print *, EXPMSG, error%l_expected
  end subroutine  

  !>---------------------------------------------------------------------------------------------------<!

  !> Build the error message from the error data type
  function build_error_message(msg) result(error)
    character(*), intent(in)  :: msg
    integer                   :: msglength
    type(error_msg_type)      :: error
    
    if (DBG) then
      print *, cyellow // "build_error_message called" //cclear
    end if

    msglength = len_trim(msg)

    if (msglength .gt. 0) then
      allocate(character(msglength) :: error%msg)
      error%msg = msg
      error%msglength = msglength
    end if    
  end function

  !> Build the body of the error message based on INTEGER types
  subroutine build_error_body_INT(msg, actual, expected, tol, error)
    character(*), intent(in)            :: msg
    integer, intent(in)                 :: actual, expected, tol
    type(error_msg_type), intent(inout) :: error

    if (DBG) then
      print *, cyellow // "build_error_body_INT called" //cclear
    end if

    error = build_error_message(msg)

    error%error_cat = AEIF
    error%i_actual = actual
    error%i_expected = expected
    error%i_tol = tol
  end subroutine

  !> Build the body of the error message based on REAL types
  subroutine build_error_body_REAL(msg,actual, expected, tol, error)
    character(*), intent(in)            :: msg
    real, intent(in)                    :: actual, expected, tol
    type(error_msg_type), intent(inout) :: error

    if (DBG) then
      print *, cyellow // "build_error_body_REAL called" //cclear
    end if

    error = build_error_message(msg)

    error%error_cat = AERF
    error%r_actual = actual
    error%r_expected = expected
    error%r_tol = tol
  end subroutine

  !> Build the body of the error message based on COMPLEX types
  subroutine build_error_body_COMPLEX(msg, actual, expected, tol, error)
    character(*), intent(in)            :: msg
    complex, intent(in)                 :: actual, expected
    real, intent(in)                    :: tol
    type(error_msg_type), intent(inout) :: error

    if (DBG) then
      print *, cyellow // "build_error_body_COMPLEX called" //cclear
    end if

    error = build_error_message(msg)

    error%error_cat = AECF
    error%c_actual = actual
    error%c_expected = expected
    error%c_tol = tol
  end subroutine

  !> Build the body of the error message based on LOGICAL types
  subroutine build_error_body_LOGICAL(msg, actual, expected, error)
    character(*), intent(in)            :: msg
    logical, intent(in)                 :: actual, expected
    type(error_msg_type), intent(inout) :: error

    if (DBG) then
      print *, cyellow // "build_error_body_LOGICAL called" //cclear
    end if

    error = build_error_message(msg)

    error%error_cat = AELF
    error%l_actual = actual
    error%l_expected = expected
  end subroutine

  !> Build the body of the error message based on CHARACTER string types
  subroutine build_error_body_STRING(msg, actual, expected,alength, elength, error)
    character(*), intent(in)            :: msg
    character(*), intent(in)            :: actual, expected
    integer, intent(in)                 :: alength, elength  
    type(error_msg_type), intent(inout) :: error

    if (DBG) then
      print *, cyellow // "build_error_body_STRING called" //cclear
    end if

    error = build_error_message(msg)

    error%error_cat = AESF
    error%s_actual = actual
    error%s_expected = expected
    error%s_alength = alength
    error%s_elength = elength
  end subroutine

  !>---------------------------------------------------------------------------------------------------<!

  !> Test is an actual INTEGER is the expected INTEGER - no tolerance
  subroutine assert_equals_int(actual, expected, tol)
    integer, intent(in)   :: actual, expected, tol
    type(error_msg_type)  :: error 

    if (abs(actual - expected) .gt. tol ) then
      call build_error_body("Integer Inequality", actual, expected, tol, error)
      call display_failed_message(error)
      failed = failed + 1
    else 
      print *, PASSMSG
    end if
  end subroutine

  !> Test is an actual REAL is the expected REAL are within a given tolerance
  subroutine assert_equals_real(actual, expected, tol)
    real, intent(in)      :: actual, expected, tol
    type(error_msg_type)  :: error

    if (abs(actual - expected) .gt. tol) then
      call build_error_body("Real Inequality", actual, expected, tol, error)
      call display_failed_message(error)
      failed = failed + 1
    else 
      print *, PASSMSG
    end if
  end subroutine

  !> Test is an actual COMPLEX REAL/IMAGINE and expected COMPLEX REAL/IMAGINE 
  !> value are within tolerance
  subroutine assert_equals_complex(actual, expected, tol)
    complex, intent(in)   :: actual, expected
    real, intent(in)      :: tol
    type(error_msg_type)  :: error  

    if ((abs(real(actual) - real(expected)) .gt. tol) .and. (abs(aimag(actual)-aimag(expected)) .gt. tol)) then
      call build_error_body("Complex Real and Imaginary Inequality", actual, expected, tol, error)
      call display_failed_message(error)
    else if (abs(real(actual) - real(expected)) .gt. tol) then
      call build_error_body("Complex Real Inequality", actual, expected, tol, error)
      call display_failed_message(error)
      failed = failed + 1
    else if (abs(aimag(actual)-aimag(expected)) .gt. tol) then
      call build_error_body("Complex Imaginary Inequality", actual, expected, tol, error)
      call display_failed_message(error)
      failed = failed + 1
    else
      print *, PASSMSG
    end if
  end subroutine

  !> Test if an actual CHARACTER string and expected CHARACTER string are equal
  subroutine assert_equals_str(actual, expected)
    character(*), intent(in)  :: actual, expected
    integer                   :: alength, elength
    type(error_msg_type)      :: error

    alength = len(actual)
    elength = len(expected)

    if (alength .ne. elength) then
      call build_error_body("String length mismatch", actual, expected, alength, elength, error)
      call display_failed_message(error)
      failed = failed + 1
    else if (actual .ne. expected) then
      call build_error_body("String mismatch", actual, expected, alength, elength, error)
      call display_failed_message(error)
      failed = failed + 1
    else
      print *, PASSMSG
    end if
  end subroutine

  !>---------------------------------------------------------------------------------------------------<!

  !> Test is an actual CHARACTER string and expected CHARACTER string contain a string pattern
  subroutine assert_contains(actual, expected)
    character(*), intent(in)  :: actual, expected
    type(error_msg_type)      :: error
    integer                   :: alength, elength, i
    logical                   :: found

    found = .false.
    alength = len(actual)
    elength = len(expected)

    ! print *, alength, elength

    do i=1, alength-elength+1
      ! print *, i, actual, "*", actual(i:i+elength-1), "*",  expected, "*"
      if (actual(i:i+elength-1) .eq. expected) then
        found = .true.
        exit
      else 
        found = .false.
      end if
    end do

    if (.not. found) then
      call build_error_body("Pattern not found", actual, expected, alength, elength, error)
      call display_failed_message(error)
      failed = failed + 1
    else 
      print *, PASSMSG
    end if
  end subroutine

  !>---------------------------------------------------------------------------------------------------<!

  !> Always pass test
  subroutine assert_pass()
    print *, PASSMSG
  end subroutine

  !> Always fails test
  subroutine assert_fail()
    character(len=6)      :: actual = "Actual"
    character(len=8)      :: expected = "Expected"
    integer               :: alength, elength
    type(error_msg_type)  :: error

    alength = len(actual)
    elength = len(expected)

    call build_error_body("Failed by choice", actual, expected, alength, elength, error)
    call display_failed_message(error)
    failed = failed + 1
  end subroutine

  !>---------------------------------------------------------------------------------------------------<!

  !> Test that actual LOGICAL is true
  subroutine assert_true(actual)
    logical, intent(in)     :: actual
    logical                 :: expected = .true.
    type(error_msg_type)    :: error 

    if (.not. actual) then
      call build_error_body("Failed logic", actual, expected, error)
      call display_failed_message(error)
      failed = failed + 1
    else 
      print *, PASSMSG
    end if
  end subroutine

  !> Test that actual LOGICAL is false
  subroutine assert_false(actual)
    logical, intent(in)   :: actual
    logical               :: expected = .false.
    type(error_msg_type)  :: error 

    if (actual) then
      call build_error_body("Failed logic", actual, expected, error)
      call display_failed_message(error)
      failed = failed + 1
    else 
      print *, PASSMSG
    end if
  end subroutine

  ! !>---------------------------------------------------------------------------------------------------<!

  !> Test that actual INTEGER is zero
  subroutine assert_zero_int(actual)
    integer, intent(in)   :: actual
    integer               :: expected = 0, tol = 0
    type(error_msg_type)  :: error 

    if (actual .ne. 0) then
      call build_error_body("Non zero", actual, expected, tol, error)
      error%show_tol=.false.
      call display_failed_message(error)
      failed =  failed + 1
    else
      print *, PASSMSG
    end if
  end subroutine

  !> Test that actual REAL is zero.
  subroutine assert_zero_real(actual)
    real, intent(in)      :: actual
    real                  :: expected = 0., tol = 0.
    type(error_msg_type)  :: error

    if (actual .ne. 0.) then
      call build_error_body("Non zero", actual, expected, tol, error)
      error%show_tol=.false.
      call display_failed_message(error)
      failed =  failed + 1
    else
      print *, PASSMSG
    end if
  end subroutine

  !> Test that actual COMPLEX is (zero.,zero.)
  subroutine assert_zero_complex(actual)
    complex, intent(in)   :: actual
    complex               :: expected = (0.,0.)
    real                  :: tol = 0.
    type(error_msg_type)  :: error 

    if (real(actual) .ne. 0.) then
      call build_error_body("Non zero", actual, expected, tol, error)
      error%show_tol=.false.
      call display_failed_message(error)
      failed =  failed + 1
    else if(aimag(actual) .ne. 0.) then
      call build_error_body("Non zero", actual, expected, tol, error)
      error%show_tol=.false.
      call display_failed_message(error)
      failed =  failed + 1
    else
      print *, PASSMSG
    end if
  end subroutine

  ! !> Test that actual INTEGER is NOT zero
  subroutine assert_not_zero_int(actual)
    integer, intent(in)   :: actual
    type(error_msg_type)  :: error 

    if (actual .eq. 0) then
      call build_error_body("Zero", actual, -1, 0, error)
      error%show_tol=.false.
      call display_failed_message(error)
      failed =  failed + 1
    else
      print *, PASSMSG
    end if
  end subroutine

  !> Test that actual REAL is NOT zero.
  subroutine assert_not_zero_real(actual)
    real, intent(in)      :: actual
    type(error_msg_type)  :: error

    if (actual .eq. 0.) then
      call build_error_body("Zero", actual, -1., 0., error)
      error%show_tol=.false.
      call display_failed_message(error)
      failed =  failed + 1
    else
      print *, PASSMSG
    end if
  end subroutine  

  !> Test that actual COMPLEX is NOT (zero.,zero.)
  subroutine assert_not_zero_complex(actual)
    complex, intent(in)   :: actual
    type(error_msg_type)  :: error

    if (real(actual) .eq. 0.) then
      call build_error_body("Zero", actual, (-1.,-1), 0., error)
      error%show_tol=.false.
      call display_failed_message(error)
      failed =  failed + 1
    else if (aimag(actual) .eq. 0.) then
      call build_error_body("Zero", actual, (-1.,-1), 0., error)
      error%show_tol=.false.
      call display_failed_message(error)
      failed =  failed + 1
    else
      print *, PASSMSG
    end if
  end subroutine  

  ! !>---------------------------------------------------------------------------------------------------<!

  !> Test that actual CHARACTER string is zero length
  subroutine assert_zero_length(actual)
    character(*), intent(in)  :: actual
    integer                   :: alength, elength
    type(error_msg_type)      :: error

    alength = len(actual)
    elength = 0

    if (alength .gt. 0) then
      call build_error_body("Expected zero length string", actual, "", alength, elength, error)
      call display_failed_message(error)
      failed = failed + 1
    else 
      print *, PASSMSG
    end if
  end subroutine

  !> Test that actual CHARACTER string is not zero length
  subroutine assert_non_zero_length(actual)
    character(*), intent(in)  :: actual
    integer                   :: alength, elength
    type(error_msg_type)      :: error

    alength = len(actual)
    elength = 32

    if (alength .eq. 0) then
      call build_error_body("Expected non zero length string", actual, "Expected anything but nothing", alength, elength, error)
      call display_failed_message(error)
      failed = failed + 1
    else 
      print *, PASSMSG
    end if
  end subroutine
end module VerySimpleTestFramework
