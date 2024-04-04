!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2023  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

#:include "fortuno.fypp"

module test_common_memman
  use dftbp_common_accuracy, only : dp
  use dftbp_common_memman, only : TAlignedArray
  use fortuno_serial, only : test => serial_case_item, check => serial_check,&
      & failed => serial_failed, suite => serial_suite_item, test_item
  implicit none

  private
  public :: get_tests

  #:call SET_TEST_ENV("aligned")
    type(TAlignedArray) :: aligned, aligned2
    real(dp), pointer :: array(:)
  #:endcall


  $:TEST("allocate32Bytes", env="aligned")
    call aligned%allocate(7, 32)
    call aligned%getArray(array)

    @:ASSERT(associated(array))
    @:ASSERT(aligned%alignment == 32)

    @:ASSERT(aligned%size == 7)
    @:ASSERT(size(array) == 7)
    @:ASSERT(size(array, dim=1) == 7)
    @:ASSERT(lbound(array, dim=1) == 1)
    @:ASSERT(ubound(array, dim=1) == 7)

    array(:) = 1.0_dp
    array(7) = 42.0_dp
    nullify(array)
    call aligned%getArray(array)
    @:ASSERT(array(7) == 42.0_dp)
  $:END_TEST()


  $:TEST("allocateDefault64Bytes", env="aligned")
    call aligned%allocate(7)
    call aligned%getArray(array)

    @:ASSERT(associated(array))
    @:ASSERT(aligned%alignment == 64)
  $:END_TEST()


  $:TEST("deallocate", env="aligned")
    call aligned%getArray(array)
    @:ASSERT(.not. associated(array))

    call aligned%allocate(7)
    call aligned%getArray(array)
    @:ASSERT(associated(array))

    call triggerDeallocation(aligned)
    call aligned%getArray(array)
    @:ASSERT(.not. associated(array))
  $:END_TEST()


  $:TEST("reallocate", env="aligned")
    call aligned%allocate(3)
    call aligned%getArray(array)
    array(1) = 3.0_dp

    call aligned2%allocate(4)
    call aligned2%getArray(array)
    array(1) = 4.0_dp

    aligned = aligned2
    @:ASSERT(aligned%size == 4)
    call aligned%getArray(array)
    @:ASSERT(array(1) == 4.0_dp)
  $:END_TEST()


  function get_tests() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        suite("memman", [&
            $:TESTS()
        ])&
    ]

  end function get_tests


  subroutine triggerDeallocation(instance)

    type(TAlignedArray), intent(out) :: instance

  end subroutine triggerDeallocation

end module test_common_memman


program testapp_common_memman
  use test_common_memman, only : get_tests
  use fortuno_serial, only : execute_serial_cmd_app
  implicit none

  call execute_serial_cmd_app(testitems=[get_tests()])

end program testapp_common_memman
