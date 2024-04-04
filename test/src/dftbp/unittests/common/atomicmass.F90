!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

#:include "fortuno.fypp"

module test_common_atomicmass
  use dftbp_common_accuracy, only : dp
  use dftbp_common_atomicmass, only : getAtomicSymbol
  use fortuno_serial, only : test => serial_case_item, check => serial_check,&
      & failed => serial_failed, suite => serial_suite_item, test_item
  implicit none

  private
  public :: get_tests

contains


  $:TEST("testSomeElements")
    @:ASSERT(getAtomicSymbol(1.0_dp) == 'H')
    @:ASSERT(getAtomicSymbol(12.0_dp) == 'C')
    @:ASSERT(getAtomicSymbol(16.0_dp) == 'O')
  $:END_TEST()


  $:TEST("testTooLargeMass")
    @:ASSERT(getAtomicSymbol(500.0_dp) == '??')
  $:END_TEST()


  $:TEST("testNegativeMass")
    @:ASSERT(getAtomicSymbol(-1.0_dp) == '??')
  $:END_TEST()


  function get_tests() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        suite("atomicmass", [&
            $:TESTS()
        ])&
    ]

  end function get_tests

end module test_common_atomicmass


program testapp_common_atomicmass
  use test_common_atomicmass, only : get_tests
  use fortuno_serial, only : execute_serial_cmd_app
  implicit none

  call execute_serial_cmd_app(testitems=[get_tests()])

end program testapp_common_atomicmass
