!--------------------------------------------------------------------------------------------------!
!  DFTB+: general package for performing fast atomistic simulations                                !
!  Copyright (C) 2006 - 2021  DFTB+ developers group                                               !
!                                                                                                  !
!  See the LICENSE file for terms of usage and distribution.                                       !
!--------------------------------------------------------------------------------------------------!

#:include 'common.fypp'

!> Module for static linear response derivative calculations using perturbation methods
module dftbp_derivs_staticperturb
  use dftbp_common_accuracy, only : dp, mc
  use dftbp_common_constants, only : Hartree__eV, quaternionName
  use dftbp_common_environment, only : TEnvironment
  use dftbp_common_globalenv, only : stdOut
  use dftbp_derivs_fermihelper, only : theta, deltamn, invDiff
  use dftbp_derivs_linearresponse, only : dRhoStaticReal, dRhoFermiChangeStaticReal,&
      & dRhoStaticPauli, dRhoFermiChangeStaticPauli
  use dftbp_derivs_rotatedegen, only : TRotateDegen, TRotateDegen_init
  use dftbp_dftb_blockpothelper, only : appendBlockReduced
  use dftbp_dftb_dftbplusu, only : TDftbU, TDftbU_init, plusUFunctionals
  use dftbp_dftb_onsitecorrection, only : addOnsShift, onsblock_expand
  use dftbp_dftb_orbitalequiv, only : OrbitalEquiv_reduce, OrbitalEquiv_expand
  use dftbp_dftb_periodic, only : TNeighbourList
  use dftbp_dftb_populations, only : mulliken, densemulliken, getchargepershell
  use dftbp_dftb_potentials, only : TPotentials, TPotentials_init
  use dftbp_dftb_rangeseparated, only : TRangeSepFunc
  use dftbp_dftb_scc, only : TScc
  use dftbp_dftb_shift, only : add_shift, total_shift
  use dftbp_dftb_spin, only : getSpinShift, ud2qm, qm2ud
  use dftbp_dftb_thirdorder, only : TThirdOrder,  TThirdOrderInp, ThirdOrder_init
  use dftbp_io_message, only : error, warning
  use dftbp_mixer_mixer, only : TMixer, mix, reset
  use dftbp_type_commontypes, only : TOrbitals
  use dftbp_type_densedescr, only : TDenseDescr
  use dftbp_type_parallelks, only : TParallelKS, TParallelKS_init
#:if WITH_MPI
  use dftbp_extlibs_mpifx, only : mpifx_allreduceip, MPI_SUM
#:endif
#:if WITH_SCALAPACK
  use dftbp_extlibs_scalapackfx, only : DLEN_, scalafx_getdescriptor
#:else
  use dftbp_dftb_sparse2dense, only : unpackHS
#:endif
  implicit none

  private
  public :: staticPerturWrtE

contains

  !> Static (frequency independent) perturbation at dq=0
  subroutine staticPerturWrtE(env, parallelKS, filling, eigvals, eigVecsReal, eigVecsCplx, ham,&
      & over, orb, nAtom, species, neighbourList, nNeighbourSK, denseDesc, iSparseStart,&
      & img2CentCell, coord, sccCalc, maxSccIter, sccTol, isSccConvRequired, nMixElements,&
      & nIneqMixElements, iEqOrbitals, tempElec, Ef, tFixEf, spinW, thirdOrd, dftbU, iEqBlockDftbu,&
      & onsMEs, iEqBlockOnSite, rangeSep, nNeighbourLC, pChrgMixer, kPoint, kWeight, iCellVec,&
      & cellVec, tPeriodic, polarisability, dEi, dqOut, neFermi, dEfdE)

    !> Environment settings
    type(TEnvironment), intent(inout) :: env

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    !> Filling
    real(dp), intent(in) :: filling(:,:,:)

    !> Eigenvalue of each level, kpoint and spin channel
    real(dp), intent(in) :: eigvals(:,:,:)

    !> ground state eigenvectors
    real(dp), intent(in), allocatable :: eigVecsReal(:,:,:)

    !> ground state complex eigenvectors
    complex(dp), intent(in), allocatable :: eigvecsCplx(:,:,:)

    !> Sparse Hamiltonian
    real(dp), intent(in) :: ham(:,:)

    !> sparse overlap matrix
    real(dp), intent(in) :: over(:)

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> Number of central cell atoms
    integer, intent(in) :: nAtom

    !> chemical species
    integer, intent(in) :: species(:)

    !> list of neighbours for each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Index array for the start of atomic blocks in sparse arrays
    integer, intent(in) :: iSparseStart(:,:)

    !> map from image atoms to the original unique atom
    integer, intent(in) :: img2CentCell(:)

    !> atomic coordinates
    real(dp), intent(in) :: coord(:,:)

    !> SCC module internal variables
    type(TScc), intent(inout), allocatable :: sccCalc

    !> maximal number of SCC iterations
    integer, intent(in) :: maxSccIter

    !> Tolerance for SCC convergence
    real(dp), intent(in) :: sccTol

    !> Use converged derivatives of charges
    logical, intent(in) :: isSccConvRequired

    !> nr. of elements to go through the mixer - may contain reduced orbitals and also orbital
    !> blocks (if a DFTB+U or onsite correction calculation)
    integer, intent(in) :: nMixElements

    !> nr. of inequivalent charges
    integer, intent(in) :: nIneqMixElements

    !> Equivalence relations between orbitals
    integer, intent(in), allocatable :: iEqOrbitals(:,:,:)

    !> onsite matrix elements for shells (elements between s orbitals on the same shell are ignored)
    real(dp), intent(in), allocatable :: onsMEs(:,:,:,:)

    !> Equivalences for onsite block corrections if needed
    integer, intent(in), allocatable :: iEqBlockOnSite(:,:,:,:)

    !> Electron temperature
    real(dp), intent(in) :: tempElec

    !> Fermi level(s)
    real(dp), intent(in) :: Ef(:)

    !> Whether fixed Fermi level(s) should be used. (No charge conservation!)
    logical, intent(in) :: tFixEf

    !> spin constants
    real(dp), intent(in), allocatable :: spinW(:,:,:)

    !> Third order SCC interactions
    type(TThirdOrder), allocatable, intent(inout) :: thirdOrd

    !> Are there orbital potentials present
    type(TDftbU), intent(in), allocatable :: dftbU

    !> equivalence mapping for dual charge blocks
    integer, intent(in), allocatable :: iEqBlockDftbu(:,:,:,:)

    !> Data for range-separated calculation
    type(TRangeSepFunc), allocatable, intent(inout) :: rangeSep

    !> Number of neighbours for each of the atoms for the exchange contributions in the long range
    !> functional
    integer, intent(inout), allocatable :: nNeighbourLC(:)

    !> Charge mixing object
    type(TMixer), intent(inout), allocatable :: pChrgMixer

    !> k-points
    real(dp), intent(in) :: kPoint(:,:)

    !> Weights for k-points
    real(dp), intent(in) :: kWeight(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    !> Is this a periodic geometry
    logical, intent(in) :: tPeriodic

    !> Static electric polarisability
    real(dp), intent(out) :: polarisability(:,:)

    !> Derivatives of eigenvalues, if required
    real(dp), allocatable, intent(inout) :: dEi(:,:,:,:)

    !> Derivatives of Mulliken charges, if required
    real(dp), allocatable, intent(inout) :: dqOut(:,:,:,:)

    !> Number of electrons at the Fermi energy (if metallic)
    real(dp), allocatable, intent(inout) :: neFermi(:)

    !> Derivative of the Fermi energy (if metallic)
    real(dp), allocatable, intent(inout) :: dEfdE(:,:)

    integer :: iS, iK, iAt, iCart, iLev

    integer :: nSpin, nKpts, nOrbs, nIndepHam

    ! maximum allowed number of electrons in a single particle state
    real(dp) :: maxFill

    integer, allocatable :: nFilled(:,:), nEmpty(:,:)
    real(dp), allocatable :: dEiTmp(:,:,:), dEfdETmp(:)

    integer :: ii

    ! matrices for derivatives of terms in hamiltonian and outputs
    real(dp), allocatable :: dHam(:,:), idHam(:,:), idRho(:,:)
    real(dp) :: drho(size(over),size(ham, dim=2))
    real(dp) :: dqIn(orb%mOrb,nAtom,size(ham, dim=2))

    real(dp), allocatable :: dqBlockIn(:,:,:,:), SSqrReal(:,:)
    real(dp), allocatable :: dqBlockOut(:,:,:,:)

    ! derivative of potentials
    type(TPotentials) :: dPotential

    logical :: tSccCalc, tMetallic

    real(dp), allocatable :: dPsiReal(:,:,:)
    complex(dp), allocatable :: dPsiCmplx(:,:,:,:)

    ! used for range separated contributions, note this stays in the up/down representation
    ! throughout if spin polarised
    real(dp), pointer :: dRhoOutSqr(:,:,:), dRhoInSqr(:,:,:)
    real(dp), allocatable, target :: dRhoOut(:), dRhoIn(:)

    !> For transformation in the  case of degeneracies
    type(TRotateDegen), allocatable :: transform(:)

    if (tPeriodic) then
      call error("Electric field polarizability not currently implemented for periodic systems")
    end if

    if (tFixEf) then
      call error("Perturbation expressions not currently implemented for fixed Fermi energy")
    end if

    write(stdOut,*)
    write(stdOut,*)'Perturbation calculation of electric polarisability'
    write(stdOut,*)

    nSpin = size(ham, dim=2)
    select case(nSpin)
    case(1,4)
      nIndepHam = 1
    case(2)
      nIndepHam = 2
    end select
    select case(nSpin)
    case(1)
      maxFill = 2.0_dp
    case(2,4)
      maxFill = 1.0_dp
    end select

    nOrbs = size(filling,dim=1)
    nKpts = size(filling,dim=2)

    allocate(transform(nIndepHam))
    do ii = 1, nIndepHam
      call TRotateDegen_init(transform(ii))
    end do

    allocate(dHam(size(ham,dim=1),nSpin))
    if (nSpin == 4) then
      allocate(idHam(size(ham,dim=1),nSpin))
      allocate(idRho(size(ham,dim=1),nSpin))
      idHam(:,:) = 0.0_dp
    end if

    if (allocated(rangeSep)) then
      allocate(SSqrReal(nOrbs, nOrbs))
      SSqrReal(:,:) = 0.0_dp
    #:if not WITH_SCALAPACK
      call unpackHS(SSqrReal, over, neighbourList%iNeighbour, nNeighbourSK, denseDesc%iAtomStart,&
          & iSparseStart, img2CentCell)
    #:endif
      allocate(dRhoOut(nOrbs * nOrbs * nSpin))
      dRhoOutSqr(1:nOrbs, 1:nOrbs, 1:nSpin) => dRhoOut(:nOrbs*nOrbs*nSpin)
      allocate(dRhoIn(nOrbs * nOrbs * nSpin))
      dRhoInSqr(1:nOrbs, 1:nOrbs, 1:nSpin) => dRhoIn(:nOrbs*nOrbs*nSpin)
    else
      dRhoInSqr => null()
      dRhoOutSqr => null()
    end if

    tSccCalc = allocated(sccCalc)

    if (allocated(dftbU) .or. allocated(onsMEs)) then
      allocate(dqBlockIn(orb%mOrb,orb%mOrb,nAtom,nSpin))
      allocate(dqBlockOut(orb%mOrb,orb%mOrb,nAtom,nSpin))
    end if

    call TPotentials_init(dPotential,orb,nAtom,nSpin)

    allocate(nFilled(nIndepHam, nKpts))
    allocate(nEmpty(nIndepHam, nKpts))

    ! If derivatives of eigenvalues are needed
    if (allocated(dEi)) then
      dEi(:,:,:,:) = 0.0_dp
      allocate(dEiTmp(nOrbs,nKPts,nIndepHam))
    end if

    nFilled(:,:) = -1
    do iS = 1, nIndepHam
      do iK = 1, nKPts
        do iLev = 1, nOrbs
          if ( filling(iLev,iK,iS) < epsilon(1.0) ) then
            nFilled(iS,iK) = iLev - 1
            exit
          end if
        end do
        if (nFilled(iS, iK) < 0) then
          nFilled(iS, iK) = nOrbs
        end if
      end do
    end do
    nEmpty(:,:) = -1
    do iS = 1, nIndepHam
      do iK = 1, nKpts
        do iLev = 1, nOrbs
          if ( abs( filling(iLev,iK,iS) - maxFill ) > epsilon(1.0)) then
            nEmpty(iS, iK) = iLev
            exit
          end if
        end do
        if (nEmpty(iS, iK) < 0) then
          nEmpty(iS, iK) = 1
        end if
      end do
    end do

    ! should really check for each spin channel separately:
    tMetallic = (.not.all(nFilled == nEmpty -1))

    ! if derivatives of valence wavefunctions needed. Note these will have an arbitrary set of
    ! global phases
    ! if (allocated(eigVecsReal)) then
    !   allocate(dPsiReal(size(eigVecsReal,dim=1), size(eigVecsReal,dim=2), nIndepHam, 3))
    ! else
    !   allocate(dPsiCmplx(size(eigvecsCplx,dim=1), size(eigvecsCplx,dim=2), nKpts, nIndepHam, 3))
    ! end if

    if (tMetallic) then
      write(stdOut,*)'Metallic system'
    else
      write(stdOut,*)'Non-metallic system'
    end if

    if (tMetallic) then
      ! Density of electrons at the Fermi energy, required to correct later for shift in Fermi level
      ! at q=0 in metals
      if (allocated(neFermi)) then
        deallocate(neFermi)
        deallocate(dEfdE)
      end if
      allocate(neFermi(size(Ef)))
      allocate(dEfdE(size(Ef),3))
      dEfdE(:,:) = 0.0_dp
      allocate(dEfdETmp(size(Ef)))

      do iS = 1, nIndepHam
        neFermi(iS) = 0.0_dp
        do iK = 1, nKpts
          do ii = nEmpty(iS, iK), nFilled(iS, iK)
            neFermi(iS) = neFermi(iS) + kWeight(iK) * deltamn(Ef(iS), eigvals(ii,iK,iS), tempElec)
          end do
        end do
        neFermi(iS) = maxFill * neFermi(iS)
      end do
      write(stdOut,*)'Density of states at the Fermi energy Nf (a.u.):', neFermi
    end if

    dqOut(:,:,:,:) = 0.0_dp

    ! polarisation direction
    ! note, could MPI parallelise over this
    lpCart: do iCart = 1, 3

      dqIn(:,:,:) = 0.0_dp
      if (allocated(dftbU) .or. allocated(onsMEs)) then
        dqBlockIn(:,:,:,:) = 0.0_dp
        dqBlockOut(:,:,:,:) = 0.0_dp
      end if

      dPotential%extAtom(:,:) = 0.0_dp
      dPotential%extShell(:,:,:) = 0.0_dp
      dPotential%extBlock(:,:,:,:) = 0.0_dp

      ! derivative wrt to electric field as a perturbation
      do iAt = 1, nAtom
        dPotential%extAtom(iAt,1) = coord(iCart,iAt)
      end do
      call total_shift(dPotential%extShell, dPotential%extAtom, orb, species)
      call total_shift(dPotential%extBlock, dPotential%extShell, orb, species)

      if (allocated(dEfdETmp)) then
        dEfdETmp(:) = 0.0_dp
      end if
      dEiTmp(:,:,:) = 0.0_dp
      call response(env, parallelKS, dPotential, nAtom, orb, species, neighbourList, nNeighbourSK,&
          & img2CentCell, iSparseStart, denseDesc, over, iEqOrbitals, sccCalc, sccTol,&
          & isSccConvRequired, maxSccIter, pChrgMixer, nMixElements, nIneqMixElements, dqIn,&
          & dqOut(:,:,:,iCart), rangeSep, nNeighbourLC, sSqrReal, dRhoInSqr, dRhoOutSqr, dRhoIn,&
          & dRhoOut, nSpin, maxFill, spinW, thirdOrd, dftbU, iEqBlockDftbu, onsMEs, iEqBlockOnSite,&
          & dqBlockIn, dqBlockOut, eigVals, transform, dEiTmp, dEfdETmp, Ef, dHam, idHam,&
          & dRho, idRho, tempElec, tMetallic, neFermi, nFilled, nEmpty, kPoint, kWeight, cellVec,&
          & iCellVec, eigVecsReal, eigVecsCplx, dPsiReal, dPsiCmplx)

      if (allocated(dEfdE)) then
        dEfdE(:,iCart) = dEfdETmp
      end if
      if (allocated(dEiTmp)) then
        dEi(:,:,:,iCart) = dEiTmp
      end if

      if (tMetallic) then
        write(stdOut,*)
        write(stdOut,"(A,2E20.12)")'d E_f / d E_'//trim(quaternionName(iCart+1))//':',&
            & dEfdE(:,iCart)
        write(stdOut,*)
      end if

      do ii = 1, 3
        polarisability(ii, iCart) = -sum(sum(dqOut(:,:nAtom,1,iCart),dim=1)*coord(ii,:nAtom))
      end do

    end do lpCart

  #:if WITH_SCALAPACK
    if (allocated(dEi)) then
      call mpifx_allreduceip(env%mpi%globalComm, dEi, MPI_SUM)
    end if
  #:endif

    write(stdOut,*)
    write(stdOut,*)'Static polarisability (a.u.)'
    do iCart = 1, 3
      write(stdOut,"(3E20.12)")polarisability(:, iCart)
    end do
    write(stdOut,*)

  end subroutine staticPerturWrtE


  !> Evaluates response, given the external perturbation
  subroutine response(env, parallelKS, dPotential, nAtom, orb, species, neighbourList,&
      & nNeighbourSK, img2CentCell, iSparseStart, denseDesc, over, iEqOrbitals, sccCalc, sccTol,&
      & isSccConvRequired, maxSccIter, pChrgMixer, nMixElements, nIneqMixElements, dqIn, dqOut,&
      & rangeSep, nNeighbourLC, sSqrReal, dRhoInSqr, dRhoOutSqr, dRhoIn, dRhoOut, nSpin, maxFill,&
      & spinW, thirdOrd, dftbU, iEqBlockDftbu, onsMEs, iEqBlockOnSite, dqBlockIn, dqBlockOut,&
      & eigVals, transform, dEi, dEfdE, Ef, dHam, idHam,  dRho, idRho, tempElec, tMetallic,&
      & neFermi, nFilled, nEmpty, kPoint, kWeight, cellVec, iCellVec, eigVecsReal, eigVecsCplx,&
      & dPsiReal, dPsiCmplx)

    !> Environment settings
    type(TEnvironment), intent(inout) :: env

    !> K-points and spins to process
    type(TParallelKS), intent(in) :: parallelKS

    ! derivative of potentials
    type(TPotentials), intent(inout) :: dPotential

    !> Charge mixing object
    type(TMixer), intent(inout), allocatable :: pChrgMixer

    !> nr. of elements to go through the mixer - may contain reduced orbitals and also orbital
    !> blocks (if a DFTB+U or onsite correction calculation)
    integer, intent(in) :: nMixElements

    !> nr. of inequivalent charges
    integer, intent(in) :: nIneqMixElements

    !> Number of central cell atoms
    integer, intent(in) :: nAtom

    !> Atomic orbital information
    type(TOrbitals), intent(in) :: orb

    !> SCC module internal variables
    type(TScc), intent(inout), allocatable :: sccCalc

    !> Derivative of sparse Hamiltonian
    real(dp), intent(inout) :: dHam(:,:)

    !> Derivative of imaginary part of sparse Hamiltonian
    real(dp), intent(inout), allocatable :: idHam(:,:)

    !> Derivative of sparse density matrix
    real(dp), intent(inout) :: dRho(:,:)

    !> Derivative of imaginary part of sparse density matrix
    real(dp), intent(inout), allocatable :: idRho(:,:)

    !> maximal number of SCC iterations
    integer, intent(in) :: maxSccIter

    !> list of neighbours for each atom
    type(TNeighbourList), intent(in) :: neighbourList

    !> Number of neighbours for each of the atoms
    integer, intent(in) :: nNeighbourSK(:)

    !> Number of neighbours for each of the atoms for the exchange contributions in the long range
    !> functional
    integer, intent(inout), allocatable :: nNeighbourLC(:)

    !> Dense matrix descriptor
    type(TDenseDescr), intent(in) :: denseDesc

    !> Index array for the start of atomic blocks in sparse arrays
    integer, intent(in) :: iSparseStart(:,:)

    !> map from image atoms to the original unique atom
    integer, intent(in) :: img2CentCell(:)

    !> chemical species
    integer, intent(in) :: species(:)

    !> spin constants
    real(dp), intent(in), allocatable :: spinW(:,:,:)

    !> Third order SCC interactions
    type(TThirdOrder), allocatable, intent(inout) :: thirdOrd

    !> Is there a finite density of states at the Fermi energy
    logical, intent(in) :: tMetallic

    !> Number of electrons at the Fermi energy (if metallic)
    real(dp), allocatable, intent(inout) :: neFermi(:)

    !> Tolerance for SCC convergence
    real(dp), intent(in) :: sccTol

    !> Use converged derivatives of charges
    logical, intent(in) :: isSccConvRequired

    !> Maximum allowed number of electrons in a single particle state
    real(dp), intent(in) :: maxFill

    !> sparse overlap matrix
    real(dp), intent(in) :: over(:)

    !> Equivalence relations between orbitals
    integer, intent(in), allocatable :: iEqOrbitals(:,:,:)

    !> For transformation in the  case of degeneracies
    type(TRotateDegen), allocatable, intent(inout) :: transform(:)

    !> Derivative of density matrix
    real(dp), target, allocatable, intent(inout) :: dRhoIn(:)

    !> Derivative of density matrix
    real(dp), target, allocatable, intent(inout) :: dRhoOut(:)

    !> delta density matrix for rangeseparated calculations
    real(dp), pointer :: dRhoOutSqr(:,:,:), dRhoInSqr(:,:,:)

    !> Are there orbital potentials present
    type(TDftbU), intent(in), allocatable :: dftbU

    !> equivalence mapping for dual charge blocks
    integer, intent(in), allocatable :: iEqBlockDftbu(:,:,:,:)

    !> Levels with at least partial filling
    integer, intent(in) :: nFilled(:,:)

    !> Levels that are at least partially empty
    integer, intent(in) :: nEmpty(:,:)

    !> Derivatives of Mulliken charges, if required
    real(dp), intent(inout) :: dqOut(:,:,:)

    real(dp), intent(inout), allocatable :: dEi(:,:,:)

    !> Derivatives of Mulliken charges, if required
    real(dp), intent(inout) :: dqIn(:,:,:)

    integer, intent(in) :: nSpin

    !> Electron temperature
    real(dp), intent(in) :: tempElec

    !> Fermi level(s)
    real(dp), intent(in) :: Ef(:)

    !> Eigenvalue of each level, kpoint and spin channel
    real(dp), intent(in) :: eigvals(:,:,:)

    !> ground state eigenvectors
    real(dp), intent(in), allocatable :: eigVecsReal(:,:,:)

    !> ground state complex eigenvectors
    complex(dp), intent(in), allocatable :: eigvecsCplx(:,:,:)

    real(dp), intent(inout), allocatable :: dEfdE(:)

    real(dp), allocatable, intent(inout) :: dqBlockIn(:,:,:,:)
    real(dp), allocatable, intent(inout) :: dqBlockOut(:,:,:,:)

    !> onsite matrix elements for shells (elements between s orbitals on the same shell are ignored)
    real(dp), intent(in), allocatable :: onsMEs(:,:,:,:)

    !> Equivalences for onsite block corrections if needed
    integer, intent(in), allocatable :: iEqBlockOnSite(:,:,:,:)

    !> Data for range-separated calculation
    type(TRangeSepFunc), allocatable, intent(inout) :: rangeSep

    real(dp), allocatable, intent(inout) :: sSqrReal(:,:)

    !> k-points
    real(dp), intent(in) :: kPoint(:,:)

    !> Weights for k-points
    real(dp), intent(in) :: kWeight(:)

    !> Vectors (in units of the lattice constants) to cells of the lattice
    real(dp), intent(in) :: cellVec(:,:)

    !> Index for which unit cell atoms are associated with
    integer, intent(in) :: iCellVec(:)

    real(dp), allocatable, intent(inout) :: dPsiReal(:,:,:)
    complex(dp), allocatable, intent(inout) :: dPsiCmplx(:,:,:,:)

    logical :: tSccCalc, tConverged
    integer :: iSccIter
    real(dp), allocatable :: shellPot(:,:,:), atomPot(:,:)
    real(dp), allocatable :: dummy(:,:,:,:)

    real(dp) :: dqInpRed(nMixElements), dqOutRed(nMixElements), sccErrorQ
    real(dp) :: dqPerShell(orb%mShell, nAtom, nSpin)

    integer :: iAt, iKS, iK, iS, iSh, iSp
    real(dp) :: dRhoExtra(size(over),nSpin)
    real(dp), allocatable :: idRhoExtra(:,:)
    real(dp) :: dqDiffRed(nMixElements)

  #:if WITH_SCALAPACK
    ! need distributed matrix descriptors
    integer :: desc(DLEN_), nn

    nn = denseDesc%fullSize
    call scalafx_getdescriptor(env%blacs%orbitalGrid, nn, nn, env%blacs%rowBlockSize,&
        & env%blacs%columnBlockSize, desc)
  #:endif

    tSccCalc = allocated(sccCalc)

    if (allocated(spinW) .or. allocated(thirdOrd)) then
      allocate(shellPot(orb%mShell, nAtom, nSpin))
    end if
    if (allocated(thirdOrd)) then
      allocate(atomPot(nAtom, nSpin))
    end if

    if (tMetallic .and. allocated(idHam)) then
      allocate(idRhoExtra(size(over),nSpin))
    end if

    if (tSccCalc) then
        call reset(pChrgMixer, size(dqInpRed))
        dqInpRed(:) = 0.0_dp
        dqPerShell(:,:,:) = 0.0_dp
        if (allocated(rangeSep)) then
          dRhoIn(:) = 0.0_dp
          dRhoOut(:) = 0.0_dp
        end if
      end if

      if (tSccCalc) then
        write(stdOut,"(1X,A,T12,A)")'SCC Iter','Error'
      end if

      lpSCC: do iSccIter = 1, maxSccIter

        dPotential%intAtom(:,:) = 0.0_dp
        dPotential%intShell(:,:,:) = 0.0_dp
        dPotential%intBlock(:,:,:,:) = 0.0_dp

        if (allocated(dftbU) .or. allocated(onsMEs)) then
          dPotential%orbitalBlock(:,:,:,:) = 0.0_dp
        end if

        if (tSccCalc .and. iSCCiter>1) then
          call sccCalc%updateCharges(env, dqIn, orb, species)
          call sccCalc%updateShifts(env, orb, species, neighbourList%iNeighbour, img2CentCell)
          call sccCalc%getShiftPerAtom(dPotential%intAtom(:,1))
          call sccCalc%getShiftPerL(dPotential%intShell(:,:,1))

          if (allocated(spinW)) then
            call getChargePerShell(dqIn, orb, species, dqPerShell)
            call getSpinShift(shellPot, dqPerShell, species, orb, spinW)
            dPotential%intShell(:,:,:) = dPotential%intShell + shellPot
          end if

          if (allocated(thirdOrd)) then
            atomPot(:,:) = 0.0_dp
            shellPot(:,:,:) = 0.0_dp
            call thirdOrd%getdShiftdQ(atomPot(:,1), shellPot(:,:,1), species, neighbourList, dqIn,&
                & img2CentCell, orb)
            dPotential%intAtom(:,1) = dPotential%intAtom(:,1) + atomPot(:,1)
            dPotential%intShell(:,:,1) = dPotential%intShell(:,:,1) + shellPot(:,:,1)
          end if

          if (allocated(dftbU)) then
            ! note the derivatives of both FLL and pSIC potentials are the same (i.e. pSIC)
            call dftbU%getDftbUShift(dPotential%orbitalBlock, dqBlockIn, species, orb,&
                & plusUFunctionals%pSIC)
          end if
          if (allocated(onsMEs)) then
            ! onsite corrections
            call addOnsShift(dPotential%orbitalBlock, dPotential%iOrbitalBlock, dqBlockIn, dummy,&
                & onsMEs, species, orb)
          end if

        end if

        call total_shift(dPotential%intShell,dPotential%intAtom, orb, species)
        call total_shift(dPotential%intBlock,dPotential%intShell, orb, species)
        if (allocated(dftbU) .or. allocated(onsMEs)) then
          dPotential%intBlock(:,:,:,:) = dPotential%intBlock + dPotential%orbitalBlock
        end if
        dPotential%intBlock(:,:,:,:) = dPotential%intBlock + dPotential%extBlock

        dHam(:,:) = 0.0_dp
        call add_shift(dHam, over, nNeighbourSK, neighbourList%iNeighbour, species, orb,&
            & iSparseStart, nAtom, img2CentCell, dPotential%intBlock)

        if (nSpin > 1) then
          dHam(:,:) = 2.0_dp * dHam
          if (allocated(idHam)) then
            idHam(:,:) = 2.0_dp * idHam
          end if
        end if
        call qm2ud(dHam)
        if (allocated(idHam)) then
          call qm2ud(idHam)
        end if

        dRho(:,:) = 0.0_dp
        if (allocated(idRho)) then
          idRho(:,:) = 0.0_dp
        end if

        ! evaluate derivative of density matrix
        if (allocated(eigVecsReal)) then

          do iKS = 1, parallelKS%nLocalKS

            iS = parallelKS%localKS(2, iKS)

            if (allocated(dRhoOut)) then
              ! replace with case that will get updated in dRhoStaticReal
              dRhoOutSqr(:,:,iS) = dRhoInSqr(:,:,iS)
            end if

            call dRhoStaticReal(env, dHam, neighbourList, nNeighbourSK, iSparseStart, img2CentCell,&
                & denseDesc, iKS, parallelKS, nFilled(:,1), nEmpty(:,1), eigVecsReal, eigVals, Ef,&
                & tempElec, orb, drho(:,iS), dRhoOutSqr, rangeSep, over, nNeighbourLC,&
                & transform(iKS), &
                #:if WITH_SCALAPACK
                & desc,&
                #:endif
                & dEi, dPsiReal)
          end do

        elseif (nSpin > 2) then

          do iKS = 1, parallelKS%nLocalKS

            iK = parallelKS%localKS(1, iKS)

            call dRhoStaticPauli(env, dHam, idHam, neighbourList, nNeighbourSK, iSparseStart,&
                & img2CentCell, denseDesc, parallelKS, nFilled(:, iK), nEmpty(:, iK), eigvecsCplx,&
                & eigVals, Ef, tempElec, orb, dRho, idRho, kPoint, kWeight, iCellVec, cellVec, iKS,&
                & transform(iKS), &
                #:if WITH_SCALAPACK
                & desc,&
                #:endif
                & dEi, dPsiCmplx)

          end do

        else

          call error("Shouldn't be here")

        end if

      #:if WITH_SCALAPACK
        ! Add up and distribute density matrix contributions from each group
        call mpifx_allreduceip(env%mpi%globalComm, dRho, MPI_SUM)
      #:endif

        dRhoExtra(:,:) = 0.0_dp
        if (tMetallic) then
          ! correct for Fermi level shift for q=0 fields

          do iKS = 1, parallelKS%nLocalKS
            iK = parallelKS%localKS(1, iKS)
            iS = parallelKS%localKS(2, iKS)

            dqOut(:,:,iS) = 0.0_dp
            call mulliken(dqOut(:,:,iS), over, drho(:,iS), orb, &
                & neighbourList%iNeighbour, nNeighbourSK, img2CentCell, iSparseStart)

            dEfdE(iS) = -sum(dqOut(:, :, iS))
            if (abs(dEfdE(iS)) > epsilon(0.0_dp) .and. abs(neFermi(iS)) >  epsilon(0.0_dp))&
                & then
              dEfdE(iS) = -sum(dqOut(:, :, iS)) / neFermi(iS)
            else
              dEfdE(iS) = 0.0_dp
            end if

            if (abs(dEfdE(iS)) > 10.0_dp*epsilon(1.0_dp)) then
              ! Fermi level changes, so need to correct for the change in the number of charges

              if (allocated(eigVecsReal)) then

                ! real case, no k-points
                call dRhoFermiChangeStaticReal(dRhoExtra(:, iS), env, parallelKS, iKS,&
                    & neighbourList, nNeighbourSK, img2CentCell, iSparseStart, dEfdE, Ef,&
                    & nFilled(:,iK), nEmpty(:,iK), eigVecsReal, orb, denseDesc, tempElec, eigVals,&
                    & dRhoOutSqr&
                    #:if WITH_SCALAPACK
                    &, desc&
                    #:endif
                    &)

              elseif (nSpin > 2) then

                ! two component wavefunction cases
                call dRhoFermiChangeStaticPauli(dRhoExtra, idRhoExtra, env, parallelKS, iKS,&
                    & kPoint, kWeight, iCellVec, cellVec, neighbourList, nNEighbourSK,&
                    & img2CentCell, iSparseStart, dEfdE, Ef, nFilled(:,iK), nEmpty(:,iK),&
                    & eigVecsCplx, orb, denseDesc, tempElec, eigVals&
                    #:if WITH_SCALAPACK
                    &, desc&
                    #:endif
                    &)

              else

                ! k-points
                call error("Not added yet")

              end if

            end if

          end do

          #:if WITH_SCALAPACK
          ! Add up and distribute density matrix contribution from each group
          call mpifx_allreduceip(env%mpi%globalComm, dRhoExtra, MPI_SUM)
          #:endif
          dRho(:,:) = dRho + dRhoExtra

        end if

        dRho(:,:) = maxFill * drho
        if (allocated(dRhoOut)) then
          dRhoOut(:) = maxFill * dRhoOut
        end if
        call ud2qm(dRho)

        if (allocated(idRho)) then
          idRho(:,:) = maxFill * drho
          call ud2qm(idRho)
        end if

        dqOut(:,:,:) = 0.0_dp
        do iS = 1, nSpin
          call mulliken(dqOut(:,:,iS), over, drho(:,iS), orb, &
              & neighbourList%iNeighbour, nNeighbourSK, img2CentCell, iSparseStart)
          if (allocated(dftbU) .or. allocated(onsMEs)) then
            dqBlockOut(:,:,:,iS) = 0.0_dp
            call mulliken(dqBlockOut(:,:,:,iS), over, drho(:,iS), orb, neighbourList%iNeighbour,&
                & nNeighbourSK, img2CentCell, iSparseStart)
          end if
        end do

        if (tSccCalc) then

          if (allocated(rangeSep)) then
            dqDiffRed(:) = dRhoOut - dRhoIn
          else
            dqOutRed(:) = 0.0_dp
            call OrbitalEquiv_reduce(dqOut, iEqOrbitals, orb, dqOutRed(:nIneqMixElements))
            if (allocated(dftbU)) then
              call AppendBlockReduced(dqBlockOut, iEqBlockDFTBU, orb, dqOutRed)
            end if
            if (allocated(onsMEs)) then
              call AppendBlockReduced(dqBlockOut, iEqBlockOnsite, orb, dqOutRed)
            end if
            dqDiffRed(:) = dqOutRed - dqInpRed
          end if
          sccErrorQ = maxval(abs(dqDiffRed))

          write(stdOut,"(1X,I0,T10,E20.12)")iSCCIter, sccErrorQ
          tConverged = (sccErrorQ < sccTol)

          if ((.not. tConverged) .and. iSCCiter /= maxSccIter) then
            if (iSCCIter == 1) then
              if (allocated(rangeSep)) then
                dRhoIn(:) = dRhoOut
                call denseMulliken(dRhoInSqr, SSqrReal, denseDesc%iAtomStart, dqIn)
              else
                dqIn(:,:,:) = dqOut
                dqInpRed(:) = dqOutRed
                if (allocated(dftbU) .or. allocated(onsMEs)) then
                  dqBlockIn(:,:,:,:) = dqBlockOut
                end if
              end if

            else

              if (allocated(rangeSep)) then
                call mix(pChrgMixer, dRhoIn, dqDiffRed)
                call denseMulliken(dRhoInSqr, SSqrReal, denseDesc%iAtomStart, dqIn)
              else
                call mix(pChrgMixer, dqInpRed, dqDiffRed)
                #:if WITH_MPI
                ! Synchronise charges in order to avoid mixers that store a history drifting apart
                call mpifx_allreduceip(env%mpi%globalComm, dqInpRed, MPI_SUM)
                dqInpRed(:) = dqInpRed / env%mpi%globalComm%size
                #:endif

                call OrbitalEquiv_expand(dqInpRed(:nIneqMixElements), iEqOrbitals, orb, dqIn)

                if (allocated(dftbU) .or. allocated(onsMEs)) then
                  dqBlockIn(:,:,:,:) = 0.0_dp
                  if (allocated(dftbU)) then
                    call dftbU%expandBlock(dqInpRed, iEqBlockDFTBU, orb, dqBlockIn, species,&
                        & orbEquiv=iEqOrbitals)
                  else
                    call Onsblock_expand(dqInpRed, iEqBlockOnSite, orb, dqBlockIn,&
                        & orbEquiv=iEqOrbitals)
                  end if
                end if
              end if

            end if

            if (allocated(rangeSep)) then
              call ud2qm(dqIn)
            end if

          end if
        else
          tConverged = .true.
        end if

        if (tConverged) then
          exit lpSCC
        end if

        if (allocated(spinW)) then
          dqPerShell = 0.0_dp
          do iAt = 1, nAtom
            iSp = species(iAt)
            do iSh = 1, orb%nShell(iSp)
              dqPerShell(iSh,iAt,:nSpin) = dqPerShell(iSh,iAt,:nSpin) +&
                  & sum(dqIn(orb%posShell(iSh,iSp): orb%posShell(iSh+1,iSp)-1,iAt,:nSpin),dim=1)
            end do
          end do

        end if

      end do lpSCC

      if (tSccCalc .and. .not.tConverged) then
        call warning("SCC in perturbation is NOT converged, maximal SCC iterations exceeded")
        if (isSccConvRequired) then
          call env%shutdown()
        end if
      end if

  end subroutine response

end module dftbp_derivs_staticperturb
