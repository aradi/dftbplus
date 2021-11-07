!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2021  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

#:include "common.fypp"


!> Exporting the functionality from the plumed library
module dftbp_extlibs_plumed
#:if WITH_PLUMED
  use plumed_f08_module, only : TPlumedCalc => plumed, TPlumedCalc_init => plumed_create
#:endif
  implicit none
  private


  public :: TPlumedCalc, TPlumedCalc_init
  public :: withPlumed


#:if not WITH_PLUMED
  type :: TPlumedCalc
  end type TPlumedCalc
#:endif


  !> Whether package was build with PLUMED support
  logical, parameter :: withPlumed = ${FORTRAN_LOGICAL(WITH_PLUMED)}$


contains


#:if not WITH_PLUMED
  subroutine TPlumedCalc_init()
  end subroutine TPlumedCalc_init
#:endif


end module dftbp_extlibs_plumed
