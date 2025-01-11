module VerySimpleTestFramework
  use iso_fortran_env
  
  implicit none
  private 
  
  public :: suite, test, results
  
  !> Required constant parameters
  character(len=1), parameter     :: cr   = char(13)      ! Carriage return
  character(len=1), parameter     :: lf   = char(10)      ! Line feed
  character(len=1), parameter     :: ESC  = char(27)      ! Escape
  character(len=1), parameter     :: TAB  = char(9)       ! Horizontal Tab
  character(len=1), parameter     :: NULL = char(0)       ! Null
  
  !> ANSI Colour codes
  character(len=5), parameter     :: cred     = ESC // "[31m "
  character(len=5), parameter     :: cgreen   = ESC // "[32m "
  character(len=5), parameter     :: ccyan    = ESC // "[36m "
  character(len=5), parameter     :: cclear   = ESC // "[0m "
  character(len=5), parameter     :: cyellow  = ESC // "[93m "

  !> Predefined messages and feedback
  character(len=20), parameter    :: FAILEDMSG  =  cred   //  TAB   // "X FAILED"       // cclear
  character(len=13), parameter    :: EXPMSG     =             TAB   // "  Expected: "
  character(len=13), parameter    :: ACTMSG     =             TAB   // "    Actual: "
  character(len=13), parameter    :: TOLMSG     =             TAB   // " Tolerance: "
  character(len=20), parameter    :: PASSMSG    =  cgreen //  TAB   // "✓ PASSED"       // cclear

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
  
end module VerySimpleTestFramework
