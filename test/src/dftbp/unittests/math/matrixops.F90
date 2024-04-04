!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

#:include "fortuno.fypp"

module test_math_matrixops
  use dftbp_common_accuracy, only : dp
  use dftbp_math_matrixops, only : adjointLowerTriangle
  use fortuno_serial, only : test => serial_case_item, check => serial_check,&
      & failed => serial_failed, suite => serial_suite_item, test_item
  implicit none

  private
  public :: get_tests

  integer, parameter :: n = 3


  type :: TDenseSymmetryFx
    real(dp) :: matReal(n,n)
    complex(dp) :: matCplx(n,n)
  end type TDenseSymmetryFx

contains


  $:TEST("real")
    type(TDenseSymmetryFx) :: fx
    integer :: ii, jj, kk

    fx%matReal(:,:) = 0.0_dp
    kk = 0
    do ii = 1, size(fx%matReal, dim=1)
      do jj = ii, size(fx%matReal, dim=2)
        kk = kk + 1
        fx%matReal(jj, ii) = real(kk, dp)
      end do
    end do
    call adjointLowerTriangle(fx%matReal)
    @:ASSERT(all(fx%matReal > epsilon(0.0_dp)))
    @:ASSERT(all(abs(fx%matReal - transpose(fx%matReal)) < epsilon(0.0_dp)))
  $:END_TEST()


  $:TEST("complex")
    type(TDenseSymmetryFx) :: fx
    integer :: ii, jj, kk

    fx%matCplx(:,:) = cmplx(0, 0, dp)
    kk = 0
    do ii = 1, size(fx%matCplx, dim=1)
      do jj = ii, size(fx%matCplx, dim=2)
        kk = kk + 1
        fx%matCplx(jj, ii) = real(kk, dp)
      end do
    end do
    kk = 0
    do ii = 1, size(fx%matCplx, dim=1)
      do jj = ii + 1, size(fx%matCplx, dim=2)
        kk = kk + 1
        fx%matCplx(jj, ii) = fx%matCplx(jj, ii) + cmplx(0, kk, dp)
      end do
    end do
    call adjointLowerTriangle(fx%matCplx)
    @:ASSERT(all(abs(fx%matCplx) > epsilon(0.0_dp)))
    @:ASSERT(all(abs(fx%matCplx - transpose(conjg(fx%matCplx))) < epsilon(0.0_dp)))
  $:END_TEST()


  function get_tests() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        suite("matrixops", [&
            $:TESTS()
        ])&
    ]

  end function get_tests

end module test_math_matrixops


program testapp_math_matrixops
  use test_math_matrixops, only : get_tests
  use fortuno_serial, only : execute_serial_cmd_app
  implicit none

  call execute_serial_cmd_app(testitems=[get_tests()])

end program testapp_math_matrixops
