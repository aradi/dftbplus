!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

#:include "fortuno.fypp"


module test_io_tokenreader
  use dftbp_common_accuracy, only : dp
  use dftbp_io_tokenreader, only : getNextToken
  use fortuno_serial, only : test => serial_case_item, check => serial_check,&
      & failed => serial_failed, suite => serial_suite_item, test_item
  implicit none

  private
  public :: get_tests


  real(dp), parameter :: tol = 10.0_dp * epsilon(1.0_dp)

  type :: TComplexFx
    complex(dp) :: tokenValue
    integer :: ioStat, iStart
  end type TComplexFx

contains


  $:TEST("int_re_pls_int_im", label="complex")
    character(*), parameter :: input = "1+1i"
    complex(dp), parameter :: expected = (1, 1)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("int_re_min_int_im", label="complex")
    character(*), parameter :: input = "1-1i"
    complex(dp), parameter :: expected = (1, -1)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("fixed_re_pls_fixed_im", label="complex")
    character(*), parameter :: input = "4.321+5.425i"
    complex(dp), parameter :: expected = (4.321_dp, +5.425_dp)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("fixed_re_min_fixed_im", label="complex")
    character(*), parameter :: input = "4.321-5.425i"
    complex(dp), parameter :: expected = (4.321_dp, -5.425_dp)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("sci_re_pls_sci_im", label="complex")
    character(*), parameter :: input = "4.321e-1+5.425e+2i"
    complex(dp), parameter :: expected = (4.321e-1_dp, 5.425e2_dp)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("sci_re_pls_sci_im_capitalised", label="complex")
    character(*), parameter :: input = "4.321E-1+5.425E+2i"
    complex(dp), parameter :: expected = (4.321e-1_dp, 5.425e2_dp)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("re_only", label="complex")
    character(*), parameter :: input = "4.321e-1"
    complex(dp), parameter :: expected = (4.321e-1_dp, 0.0_dp)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("pos_re_only", label="complex")
    character(*), parameter :: input = "+4.321e-1"
    complex(dp), parameter :: expected = (4.321e-1_dp, 0.0_dp)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("neg_re_only", label="complex")
    character(*), parameter :: input = "-4.321e-1"
    complex(dp), parameter :: expected = (-4.321e-1_dp, 0.0_dp)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("im_only", label="complex")
    character(*), parameter :: input = "4.321e-1i"
    complex(dp), parameter :: expected = (0.0_dp, 4.321e-1_dp)
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat == 0)
    @:ASSERT(fx%iStart == len(input) + 2)
    @:ASSERT(abs(expected - fx%tokenValue) <= tol)
  $:END_TEST()


  $:TEST("fail_jform", label="complex")
    character(*), parameter :: input = "4.321+5.425j"
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat /= 0)
  $:END_TEST()


  $:TEST("fail_with_parentheses", label="complex")
    character(*), parameter :: input = "(3.219-4.321e-1i)"
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat /= 0)
  $:END_TEST()


  $:TEST("fail_plus_minus", label="complex")
    character(*), parameter :: input = "1.235e-1+-4.321e+2i"
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat /= 0)
  $:END_TEST()


  $:TEST("fail_missing_exp_re", label="complex")
    character(*), parameter :: input = "1.235-1+4.321e+2i"
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat /= 0)
  $:END_TEST()


  $:TEST("fail_missing_exp_im", label="complex")
    character(*), parameter :: input = "1.235e-1+4.321+2i"
    type(TComplexFx) :: fx
    fx%iStart = 1
    call getNextToken(input, fx%tokenValue, fx%iStart, iostat=fx%ioStat)
    @:ASSERT(fx%ioStat /= 0)
  $:END_TEST()

  function get_tests() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        suite("complex", [&
            $:TESTS(label="complex")
        ])&
    ]

  end function get_tests

end module test_io_tokenreader


program testapp_io_indexselection
  use test_io_tokenreader, only : get_tests
  use fortuno_serial, only : execute_serial_cmd_app
  implicit none

  call execute_serial_cmd_app(testitems=[get_tests()])

end program testapp_io_indexselection
