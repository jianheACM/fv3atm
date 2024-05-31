!> \file fv3atm_catchem_io.F90
!! This file contains derived types and subroutines for CATChem scheme I/O.
!! They read and write restart files, and read emissions data.

module fv3atm_catchem_io
  use block_control_mod,  only: block_control_type
  use fms2_io_mod,        only: FmsNetcdfDomainFile_t, write_data, &
                                register_axis, register_restart_field, &
                                register_variable_attribute, register_field, &
                                get_dimension_size
  use GFS_typedefs,       only: GFS_sfcprop_type, GFS_control_type, kind_phys
  use fv3atm_common_io,   only: get_nx_ny_from_atm, create_2d_field_and_add_to_bundle, &
                                create_3d_field_and_add_to_bundle, axis_type

  implicit none

  private

  public :: catchem_state_type, catchem_state_register_axis, catchem_state_write_axis !, &
!       catchem_state_fill_data, catchem_state_register_fields, catchem_state_deallocate_data, &
!       catchem_state_copy_from_grid, catchem_state_copy_to_grid , &
!       catchem_state_final

  public :: catchem_emissions_type, catchem_emissions_final, &
       catchem_emissions_register_dust, catchem_emissions_copy_dust, &
       catchem_emissions_register_emi, catchem_emissions_copy_emi, &
       catchem_emissions_register_emi2, catchem_emissions_copy_emi2, &
       catchem_emissions_register_gbbepx, catchem_emissions_copy_gbbepx, &
       catchem_am4_type, catchem_am4_final, &
       catchem_am4_register_emi, catchem_am4_copy_emi, &
       catchem_am4_register_gbbepx, catchem_am4_copy_gbbepx, &
       catchem_am4_register_chemic, catchem_am4_copy_chemic, &
       catchem_am4_register_dfdage, catchem_am4_copy_dfdage, &
       catchem_am4_register_depvel, catchem_am4_copy_depvel

  !>\defgroup fv3atm_catchem_io module
  !> @{

  !>@ Temporary data storage for reading and writing restart data for the RRFS-SD scheme.
  type catchem_state_type
    ! The catchem_state_type stores temporary arrays used to read or
    ! write RRFS-SD restart and axis variables.

!    real(kind_phys), pointer, private, dimension(:,:) :: & ! i,j variables
!         emdust=>null(), emseas=>null(), emanoc=>null(), fhist=>null(), coef_bb_dc=>null()

!    real(kind_phys), pointer, private, dimension(:,:,:) :: &
!         fire_in=>null() ! i, j, fire_aux_data_levels

!    real(kind_phys), pointer, private, dimension(:) :: &
!         fire_aux_data_levels=>null() ! 1:Model%fire_aux_data_levels index array for metadata write

  contains
    procedure, public :: register_axis => catchem_state_register_axis ! register fire_aux_data_levels axis
    procedure, public :: write_axis => catchem_state_write_axis ! write fire_aux_data_levels variable
!    procedure, public :: allocate_data => catchem_state_allocate_data ! allocate all pointers
!    procedure, public :: fill_data => catchem_state_fill_data ! fill data with default values
!    procedure, public :: register_fields => catchem_state_register_fields ! register catchem fields
!    procedure, public :: deallocate_data => catchem_state_deallocate_data ! deallocate pointers
!    procedure, public :: copy_from_grid => catchem_state_copy_from_grid ! Copy Sfcprop to arrays
!    procedure, public :: copy_to_grid => catchem_state_copy_to_grid ! Copy arrays to Sfcprop
!    procedure, public :: bundle_fields => catchem_bundle_fields ! Point esmf bundles to arrays
!    final :: catchem_state_final ! Destructor; calls deallocate_data
  end type catchem_state_type

  ! --------------------------------------------------------------------

  !>@ Temporary data storage for reading RRFS-SD emissions data
  type catchem_emissions_type
    integer, private :: nvar_dust = 5
    integer, private :: nvar_emi = 10
    integer, private :: nvar_emi2 = 3
    integer, private :: nvar_gbbepx = 5

    character(len=32), pointer, dimension(:), private :: dust_name => null()
    character(len=32), pointer, dimension(:), private :: emi_name => null()
    character(len=32), pointer, dimension(:), private :: emi2_name => null()
    character(len=32), pointer, dimension(:), private :: gbbepx_name => null()

    real(kind=kind_phys), pointer, dimension(:,:,:,:), private :: dust_var => null()
    real(kind=kind_phys), pointer, dimension(:,:,:), private :: emi_var => null()
    real(kind=kind_phys), pointer, dimension(:,:,:,:), private :: emi2_var => null()
    real(kind=kind_phys), pointer, dimension(:,:,:), private :: gbbepx_var => null()

  contains

    procedure, public :: register_dust => catchem_emissions_register_dust
    procedure, public :: copy_dust => catchem_emissions_copy_dust

    procedure, public :: register_emi => catchem_emissions_register_emi
    procedure, public :: copy_emi => catchem_emissions_copy_emi

    procedure, public :: register_emi2 => catchem_emissions_register_emi2
    procedure, public :: copy_emi2 => catchem_emissions_copy_emi2

    procedure, public :: register_gbbepx => catchem_emissions_register_gbbepx
    procedure, public :: copy_gbbepx => catchem_emissions_copy_gbbepx

    final :: catchem_emissions_final
  end type catchem_emissions_type

  ! --------------------------------------------------------------------

  !>@ Temporary data storage for reading AM4 inputdata
  type catchem_am4_type
    integer, private :: nvar_emi = 30
    integer, private :: nvar_gbbepx = 21
    integer, private :: nvar_chemic = 26
    integer, private :: nvar_dfdage = 8
    integer, private :: nvar_depvel = 22

    character(len=32), pointer, dimension(:), private :: emi_name => null()
    character(len=32), pointer, dimension(:), private :: gbbepx_name => null()
    character(len=32), pointer, dimension(:), private :: chemic_name => null()
    character(len=32), pointer, dimension(:), private :: dfdage_name => null()
    character(len=32), pointer, dimension(:), private :: depvel_name => null()

    real(kind=kind_phys), pointer, dimension(:,:,:), private :: emi_var => null()
    real(kind=kind_phys), pointer, dimension(:,:,:), private :: gbbepx_var => null()
    real(kind=kind_phys), pointer, dimension(:,:,:), private :: depvel_var => null()
    real(kind=kind_phys), pointer, dimension(:,:,:,:), private :: chemic_var => null()
    real(kind=kind_phys), pointer, dimension(:,:,:,:), private :: dfdage_var => null()

  contains

    procedure, public :: register_emi => catchem_am4_register_emi
    procedure, public :: copy_emi => catchem_am4_copy_emi

    procedure, public :: register_gbbepx => catchem_am4_register_gbbepx
    procedure, public :: copy_gbbepx => catchem_am4_copy_gbbepx

    procedure, public :: register_chemic => catchem_am4_register_chemic
    procedure, public :: copy_chemic => catchem_am4_copy_chemic

    procedure, public :: register_dfdage => catchem_am4_register_dfdage
    procedure, public :: copy_dfdage => catchem_am4_copy_dfdage

    procedure, public :: register_depvel => catchem_am4_register_depvel
    procedure, public :: copy_depvel => catchem_am4_copy_depvel

    final :: catchem_am4_final
  end type catchem_am4_type

  ! --------------------------------------------------------------------

contains

  ! --------------------------------------------------------------------
  ! -- RRFS_SD_STATE IMPLEMENTATION ------------------------------------
  ! --------------------------------------------------------------------

  !>@ Registers the fire_aux_data_levels axis for restart I/O
  subroutine catchem_state_register_axis(data,Model,Sfc_restart)
    implicit none
    class(catchem_state_type) :: data
    type(FmsNetcdfDomainFile_t) :: Sfc_restart
    type(GFS_control_type),      intent(in) :: Model
!    call register_axis(Sfc_restart, 'fire_aux_data_levels', &
!         dimension_length=Model%fire_aux_data_levels)
  end subroutine catchem_state_register_axis

  ! --------------------------------------------------------------------

  !>@ Registers and writes the axis indices for the fire_aux_data_levels axis
  subroutine catchem_state_write_axis(data,Model,Sfc_restart)
    implicit none
    class(catchem_state_type) :: data
    type(FmsNetcdfDomainFile_t) :: Sfc_restart
    type(GFS_control_type),      intent(in) :: Model

!    call register_field(Sfc_restart, 'fire_aux_data_levels', axis_type, (/'fire_aux_data_levels'/))
!    call register_variable_attribute(Sfc_restart, 'fire_aux_data_levels', 'cartesian_axis' ,'Z', str_len=1)
!    call write_data(Sfc_restart, 'fire_aux_data_levels', data%fire_aux_data_levels)
  end subroutine catchem_state_write_axis

  ! --------------------------------------------------------------------

!lzhang
#if 0
  !>@ Allocates temporary arrays for RRFS-SD scheme I/O and stores fire_aux_data_levels axis indices
  subroutine catchem_state_allocate_data(data,Model)
    implicit none
    class(catchem_state_type) :: data
    type(GFS_control_type),   intent(in) :: Model
    integer :: nx, ny, i

    call data%deallocate_data

    nx=Model%nx
    ny=Model%ny

    allocate(data%emdust(nx,ny))
    allocate(data%emseas(nx,ny))
    allocate(data%emanoc(nx,ny))
    allocate(data%fhist(nx,ny))
    allocate(data%coef_bb_dc(nx,ny))
    allocate(data%fire_aux_data_levels(Model%fire_aux_data_levels))
    allocate(data%fire_in(nx,ny,Model%fire_aux_data_levels))

    do i=1,Model%fire_aux_data_levels
      data%fire_aux_data_levels(i) = i
    enddo

  end subroutine catchem_state_allocate_data

  ! --------------------------------------------------------------------

  !>@brief Fills RRFS-SD temporary arrays with reasonable defaults.
  !> \section catchem_state_type%fill_data() procedure
  !! Fills all temporary variables with default values.
  !! Terrible things will happen if you don't call data%allocate_data first.
  subroutine catchem_state_fill_data(data, Model, Atm_block, Sfcprop)
    implicit none
    class(catchem_state_type) :: data
    type(GFS_sfcprop_type),   intent(in) :: Sfcprop(:)
    type(GFS_control_type),   intent(in) :: Model
    type(block_control_type), intent(in) :: Atm_block

    integer :: nb, ix, isc, jsc, i, j

    isc = Model%isc
    jsc = Model%jsc

    !$omp parallel do default(shared) private(i, j, nb, ix)
    do nb = 1, Atm_block%nblks
      do ix = 1, Atm_block%blksz(nb)
        i = Atm_block%index(nb)%ii(ix) - isc + 1
        j = Atm_block%index(nb)%jj(ix) - jsc + 1

        data%emdust(i,j) = 0
        data%emseas(i,j) = 0
        data%emanoc(i,j) = 0
        data%fhist(i,j) = 1.
        data%coef_bb_dc(i,j) = 0

        data%fire_in(i,j,:) = 0
      end do
    end do
  end subroutine catchem_state_fill_data

  ! --------------------------------------------------------------------

  !>@brief Registers RRFS-SD restart variables (for read or write)
  !> \section catchem_state_type%register_fields() procedure
  !! Registers all restart fields needed by the RRFS-SD
  !! Terrible things will happen if you don't call data%allocate_data
  !! and data%register_axes first.
  subroutine catchem_state_register_fields(data,Sfc_restart)
    implicit none
    class(catchem_state_type) :: data
    type(FmsNetcdfDomainFile_t) :: Sfc_restart

    integer :: xaxis_1_chunk, yaxis_1_chunk
    integer :: chunksizes2d(3), chunksizes3d(4)

    call get_dimension_size(Sfc_restart, 'xaxis_1', xaxis_1_chunk)
    call get_dimension_size(Sfc_restart, 'yaxis_1', yaxis_1_chunk)
    chunksizes2d = (/xaxis_1_chunk, yaxis_1_chunk, 1/)
    chunksizes3d = (/xaxis_1_chunk, yaxis_1_chunk, 1, 1/)

    ! Register 2D fields
    call register_restart_field(Sfc_restart, 'emdust', data%emdust, &
         dimensions=(/'xaxis_1', 'yaxis_1', 'Time   '/), chunksizes=chunksizes2d, is_optional=.true.)
    call register_restart_field(Sfc_restart, 'emseas', data%emseas, &
         dimensions=(/'xaxis_1', 'yaxis_1', 'Time   '/), chunksizes=chunksizes2d, is_optional=.true.)
    call register_restart_field(Sfc_restart, 'emanoc', data%emanoc, &
         dimensions=(/'xaxis_1', 'yaxis_1', 'Time   '/), chunksizes=chunksizes2d, is_optional=.true.)
    call register_restart_field(Sfc_restart, 'fhist', data%fhist, &
         dimensions=(/'xaxis_1', 'yaxis_1', 'Time   '/), chunksizes=chunksizes2d, is_optional=.true.)
    call register_restart_field(Sfc_restart, 'coef_bb_dc', data%coef_bb_dc, &
         dimensions=(/'xaxis_1', 'yaxis_1', 'Time   '/), chunksizes=chunksizes2d, is_optional=.true.)

    ! Register 3D field
    call register_restart_field(Sfc_restart, 'fire_in', data%fire_in, &
         dimensions=(/'xaxis_1             ', 'yaxis_1             ', &
         'fire_aux_data_levels', 'Time                '/), &
         chunksizes=chunksizes3d, is_optional=.true.)
  end subroutine catchem_state_register_fields

  ! --------------------------------------------------------------------
  !>@brief Creates ESMF bundles for writing RRFS-SD restarts via the write component (quilt)
  !> \section catchem_state_type%bundle_fields() procedure
  !! Registers all restart fields needed by the RRFS-SD
  !! Terrible things will happen if you don't call data%allocate_data
  !! and data%register_axes first.
  subroutine catchem_bundle_fields(data, bundle, grid, Model, outputfile)
    use esmf
    use GFS_typedefs, only: GFS_control_type
    implicit none
    class(catchem_state_type) :: data
    type(ESMF_FieldBundle),intent(inout)        :: bundle
    type(ESMF_Grid),intent(inout)               :: grid
    type(GFS_control_type),          intent(in) :: Model
    character(*), intent(in)                    :: outputfile

    ! Register 2D fields
    call create_2d_field_and_add_to_bundle(data%emdust, "emdust", trim(outputfile), grid, bundle)
    call create_2d_field_and_add_to_bundle(data%emseas, "emseas", trim(outputfile), grid, bundle)
    call create_2d_field_and_add_to_bundle(data%emanoc, "emanoc", trim(outputfile), grid, bundle)
    call create_2d_field_and_add_to_bundle(data%fhist, "fhist", trim(outputfile), grid, bundle)
    call create_2d_field_and_add_to_bundle(data%coef_bb_dc, "coef_bb_dc", trim(outputfile), grid, bundle)

    ! Register 3D field
    call create_3d_field_and_add_to_bundle(data%fire_in, 'fire_in', 'fire_aux_data_levels', &
         data%fire_aux_data_levels, trim(outputfile), grid, bundle)
  end subroutine catchem_bundle_fields

  ! --------------------------------------------------------------------

  !>@brief Destructor for the catchem_state_type
  !> \section catchem_state_type destructor() procedure
  !! Final routine for catchem_state_type, called automatically when
  !! an object of that type goes out of scope.  This is a wrapper
  !! around data%deallocate_data() with necessary syntactic
  !! differences.
  subroutine catchem_state_final(data)
    implicit none
    type(catchem_state_type) :: data
    call catchem_state_deallocate_data(data)
  end subroutine catchem_state_final

  ! --------------------------------------------------------------------
  !>@brief Deallocates internal arrays in an catchem_state_type
  !> \section catchem_state_type%deallocate_data() procedure
  !! Deallocates all data used, and nullifies the pointers. The data
  !! object can safely be used again after this call. This is also
  !! the implementation of the catchem_state_deallocate_data final routine.
  subroutine catchem_state_deallocate_data(data)
    implicit none
    class(catchem_state_type) :: data

    ! This #define reduces code length by a lot
#define IF_ASSOC_DEALLOC_NULL(var) \
    if(associated(data%var)) then ; \
      deallocate(data%var) ; \
      nullify(data%var) ; \
    endif

    IF_ASSOC_DEALLOC_NULL(emdust)
    IF_ASSOC_DEALLOC_NULL(emseas)
    IF_ASSOC_DEALLOC_NULL(emanoc)
    IF_ASSOC_DEALLOC_NULL(fhist)
    IF_ASSOC_DEALLOC_NULL(coef_bb_dc)

    IF_ASSOC_DEALLOC_NULL(fire_in)

    ! Undefine this to avoid cluttering the cpp scope:
#undef IF_ASSOC_DEALLOC_NULL
  end subroutine catchem_state_deallocate_data

  ! --------------------------------------------------------------------

  !>@brief Copies from catchem_state_type internal arrays to the model grid.
  !> \section catchem_state_type%copy_to_grid() procedure
  !! This procedure is called after reading a restart, to copy restart data
  !! from the catchem_state_type to the model grid.
  subroutine catchem_state_copy_to_grid(data, Model, Atm_block, Sfcprop)
    implicit none
    class(catchem_state_type) :: data
    type(GFS_sfcprop_type),   intent(in) :: Sfcprop(:)
    type(GFS_control_type),   intent(in) :: Model
    type(block_control_type), intent(in) :: Atm_block

    integer :: nb, ix, i, j

    !$omp parallel do default(shared) private(i, j, nb, ix)
    do nb = 1, Atm_block%nblks
      do ix = 1, Atm_block%blksz(nb)
        i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
        j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1

        Sfcprop(nb)%emdust(ix) = data%emdust(i,j)
        Sfcprop(nb)%emseas(ix) = data%emseas(i,j)
        Sfcprop(nb)%emanoc(ix) = data%emanoc(i,j)
        Sfcprop(nb)%fhist(ix) = data%fhist(i,j)
        Sfcprop(nb)%coef_bb_dc(ix) = data%coef_bb_dc(i,j)

        Sfcprop(nb)%fire_in(ix,:) = data%fire_in(i,j,:)
      enddo
    enddo
  end subroutine catchem_state_copy_to_grid

  ! --------------------------------------------------------------------

  !>@brief Copies from the model grid to catchem_state_type internal arrays
  !> \section catchem_state_type%copy_from_grid() procedure
  !! This procedure is called before writing the restart, to copy data from
  !! the model grid to catchem_state_type internal arrays. The ESMF or FMS
  !! restart code will write data from those arrays, not the model grid.
  subroutine catchem_state_copy_from_grid(data, Model, Atm_block, Sfcprop)
    implicit none
    class(catchem_state_type) :: data
    type(GFS_sfcprop_type),   intent(in) :: Sfcprop(:)
    type(GFS_control_type),   intent(in) :: Model
    type(block_control_type), intent(in) :: Atm_block

    integer :: nb, ix, i, j

    !$omp parallel do default(shared) private(i, j, nb, ix)
    do nb = 1, Atm_block%nblks
      do ix = 1, Atm_block%blksz(nb)
        i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
        j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1

        data%emdust(i,j) = Sfcprop(nb)%emdust(ix)
        data%emseas(i,j) = Sfcprop(nb)%emseas(ix)
        data%emanoc(i,j) = Sfcprop(nb)%emanoc(ix)
        data%fhist(i,j) = Sfcprop(nb)%fhist(ix)
        data%coef_bb_dc(i,j) = Sfcprop(nb)%coef_bb_dc(ix)

        data%fire_in(i,j,:) = Sfcprop(nb)%fire_in(ix,:)
      enddo
    enddo
  end subroutine catchem_state_copy_from_grid
#endif
!lzhang

  ! --------------------------------------------------------------------
  ! -- RRFS_SD_EMISSIONS IMPLEMENTATION --------------------------------
  ! --------------------------------------------------------------------

  !>@ Allocates temporary arrays and registers variables for reading the dust file.
  subroutine catchem_emissions_register_dust(data, restart, Atm_block)
    implicit none
    class(catchem_emissions_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p2 => NULL()
    integer :: num, nx, ny

    if(associated(data%dust_name)) then
      deallocate(data%dust_name)
      nullify(data%dust_name)
    endif
    if(associated(data%dust_var)) then
      deallocate(data%dust_var)
      nullify(data%dust_var)
    endif

    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%dust_name(data%nvar_dust))
    allocate(data%dust_var(nx,ny,12,data%nvar_dust))

    data%dust_name(1)  = 'clay'
    data%dust_name(2)  = 'rdrag'
    data%dust_name(3)  = 'sand'
    data%dust_name(4)  = 'ssm'
    data%dust_name(5)  = 'uthr'

    !--- register axis
    call register_axis(restart, 'lon', 'X')
    call register_axis(restart, 'lat', 'Y')
    call register_axis(restart, 'time', 12)
    !--- register the 3D fields
    do num = 1,data%nvar_dust
      var3_p2 => data%dust_var(:,:,:,num)
      call register_restart_field(restart, data%dust_name(num), var3_p2, &
           dimensions=(/'time', 'lat ', 'lon '/),&
           &is_optional=.true.)
      ! That was "is_optional=.not.mand" in the original, but mand was never initialized.
    enddo
  end subroutine catchem_emissions_register_dust

  ! --------------------------------------------------------------------

  !>@ Called after register_dust() to copy data from internal arrays to the model grid and deallocate arrays
  subroutine catchem_emissions_copy_dust(data, Sfcprop, Atm_block)
    implicit none
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    class(catchem_emissions_type) :: data
    type(block_control_type), intent(in) :: Atm_block

    integer :: num, nb, i, j, ix, k

    if(.not.associated(data%dust_name) .or. .not.associated(data%dust_var)) then
      write(0,*) 'ERROR: Called copy_dust before register_dust'
      return
    endif

    !$omp parallel do default(shared) private(i, j, nb, ix, k)
    do nb = 1, Atm_block%nblks
      !--- 3D variables
      do ix = 1, Atm_block%blksz(nb)
        i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
        j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
        do k = 1, 12
          Sfcprop(nb)%dust_in(ix,k,1)  = data%dust_var(i,j,k,1)
          Sfcprop(nb)%dust_in(ix,k,2)  = data%dust_var(i,j,k,2)
          Sfcprop(nb)%dust_in(ix,k,3)  = data%dust_var(i,j,k,3)
          Sfcprop(nb)%dust_in(ix,k,4)  = data%dust_var(i,j,k,4)
          Sfcprop(nb)%dust_in(ix,k,5)  = data%dust_var(i,j,k,5)
        enddo
      enddo
    enddo

    deallocate(data%dust_name)
    nullify(data%dust_name)
    deallocate(data%dust_var)
    nullify(data%dust_var)
  end subroutine catchem_emissions_copy_dust

  ! --------------------------------------------------------------------

  !>@ Allocates temporary arrays and registers variables for reading the emissions file.
  subroutine catchem_emissions_register_emi(data, restart, Atm_block)
    implicit none
    class(catchem_emissions_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:) :: var2_p => NULL()
    integer :: num, nx, ny

    if(associated(data%emi_name)) then
      deallocate(data%emi_name)
      nullify(data%emi_name)
    endif

    if(associated(data%emi_var)) then
      deallocate(data%emi_var)
      nullify(data%emi_var)
    endif

    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%emi_name(data%nvar_emi))
    allocate(data%emi_var(nx,ny,data%nvar_emi))

    data%emi_name(1)  = 'e_bc'
    data%emi_name(2)  = 'e_oc'
    data%emi_name(3)  = 'e_sulf'
    data%emi_name(4)  = 'e_pm_25'
    data%emi_name(5)  = 'e_so2'
    data%emi_name(6)  = 'e_pm_10'
    data%emi_name(7)  = 'dm0'
    data%emi_name(8)  = 'ero1'
    data%emi_name(9)  = 'ero2'
    data%emi_name(10) = 'ero3'
    !--- register axis
    call register_axis( restart, "lon", 'X' )
    call register_axis( restart, "lat", 'Y' )
    !--- register the 2D fields
      do num = 1,data%nvar_emi
        var2_p => data%emi_var(:,:,num)
        call register_restart_field(restart, data%emi_name(num), var2_p, dimensions=(/'lat ', 'lon '/))
      enddo

  end subroutine catchem_emissions_register_emi

  ! --------------------------------------------------------------------

  !>@ Called after register_emi() to copy data from internal arrays to the model grid and deallocate arrays
  subroutine catchem_emissions_copy_emi(data, Sfcprop, Atm_block)
    implicit none
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    class(catchem_emissions_type) :: data
    type(block_control_type), intent(in) :: Atm_block

    integer :: num, nb, i, j, ix

    if(.not.associated(data%emi_name) .or. .not.associated(data%emi_var)) then
      write(0,*) 'ERROR: Called copy_emi before register_emi'
      return
    endif

    do num=1,data%nvar_emi
      !$omp parallel do default(shared) private(i, j, nb, ix)
      do nb = 1, Atm_block%nblks
        !--- 2D variables
        do ix = 1, Atm_block%blksz(nb)
          i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
          j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
          Sfcprop(nb)%emi_in_cplchp(ix,num)  = data%emi_var(i,j,num)
        enddo
      enddo
    enddo

    deallocate(data%emi_name)
    nullify(data%emi_name)
    deallocate(data%emi_var)
    nullify(data%emi_var)
  end subroutine catchem_emissions_copy_emi

  ! --------------------------------------------------------------------
  !>@ Allocates temporary arrays and registers variables for reading the GOCART background file.
  subroutine catchem_emissions_register_emi2(data, restart, Atm_block)
    implicit none
    class(catchem_emissions_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p2 => NULL()
    integer :: num, nx, ny

    if(associated(data%emi2_name)) then
      deallocate(data%emi2_name)
      nullify(data%emi2_name)
    endif

    if(associated(data%emi2_var)) then
      deallocate(data%emi2_var)
      nullify(data%emi2_var)
    endif

    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%emi2_name(data%nvar_emi2))
    allocate(data%emi2_var(nx,ny,64,data%nvar_emi2))

    data%emi2_name(1)  = 'h2o2'
    data%emi2_name(2)  = 'no3'
    data%emi2_name(3)  = 'oh'
    !--- register axis
    call register_axis(restart, 'lon', 'X')
    call register_axis(restart, 'lat', 'Y')
    call register_axis(restart, 'z', 64)
    !--- register the 2D fields
      do num = 1,data%nvar_emi2
        var3_p2 => data%emi2_var(:,:,:,num)
        call register_restart_field(restart, data%emi2_name(num), var3_p2,dimensions=(/'z', 'lat ', 'lon '/),&
                                  &is_optional=.true.)
      enddo

  end subroutine catchem_emissions_register_emi2
  ! --------------------------------------------------------------------
  !>@ Called after register_emi() to copy data from internal arrays to the model
  !grid and deallocate arrays
  subroutine catchem_emissions_copy_emi2(data, Sfcprop, Atm_block)
    implicit none
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    class(catchem_emissions_type) :: data
    type(block_control_type), intent(in) :: Atm_block

    integer :: num, nb, i, j, k, ix

    if(.not.associated(data%emi2_name) .or. .not.associated(data%emi2_var)) then
      write(0,*) 'ERROR: Called copy_emi2 before register_emi'
      return
    endif

    do num=1,data%nvar_emi2
      !$omp parallel do default(shared) private(i, j, nb, ix)
      do nb = 1, Atm_block%nblks
        !--- 2D variables
        do ix = 1, Atm_block%blksz(nb)
          i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
          j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
          do k = 1, 64
          Sfcprop(nb)%emi2_in(ix,k,num)  = data%emi2_var(i,j,k,num)
          enddo
        enddo
      enddo
    enddo

    deallocate(data%emi2_name)
    nullify(data%emi2_name)
    deallocate(data%emi2_var)
    nullify(data%emi2_var)
  end subroutine catchem_emissions_copy_emi2
  ! --------------------------------------------------------------------

  !>@ Allocates temporary arrays and registers variables for reading the fire data file.
  subroutine catchem_emissions_register_gbbepx(data, restart, Atm_block)
    implicit none
    class(catchem_emissions_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:) :: var2_p => NULL()
    integer :: num, nx, ny


    if(associated(data%gbbepx_name)) then
      deallocate(data%gbbepx_name)
      nullify(data%gbbepx_name)
    endif

    if(associated(data%gbbepx_var)) then
      deallocate(data%gbbepx_var)
      nullify(data%gbbepx_var)
    endif


    !--- allocate the various containers needed for rrfssd fire data
    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%gbbepx_name(data%nvar_gbbepx))
    allocate(data%gbbepx_var(nx,ny,data%nvar_gbbepx))
    data%gbbepx_name(1)  = 'ebu_bc'
    data%gbbepx_name(2)  = 'ebu_oc'
    data%gbbepx_name(3)  = 'ebu_pm_25'
    data%gbbepx_name(4)  = 'ebu_so2'
    data%gbbepx_name(5)  = 'ebu_frp'

    !--- register axis
    call register_axis(restart, 'lon', 'X')
    call register_axis(restart, 'lat', 'Y')
    !--- register the 2D fields


    do num = 1,data%nvar_gbbepx
      var2_p => data%gbbepx_var(:,:,num)
      call register_restart_field(restart, data%gbbepx_name(num), var2_p, dimensions=(/'lat ', 'lon '/))
    enddo

  end subroutine catchem_emissions_register_gbbepx

  ! --------------------------------------------------------------------

  !>@ Called after register_fire() to copy data from internal arrays to the model grid and deallocate arrays
  subroutine catchem_emissions_copy_gbbepx(data, Sfcprop, Atm_block,ie)
    implicit none
    class(catchem_emissions_type) :: data
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    type(block_control_type), intent(in) :: Atm_block
    integer, intent (in) :: ie

    integer :: nb, ix, k, i, j


    !$omp parallel do default(shared) private(i, j, nb, ix, k)
    do nb = 1, Atm_block%nblks
      do ix = 1, Atm_block%blksz(nb)
        i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
        j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
        Sfcprop(nb)%fire_GBBEPx(ix,1,ie)  = data%gbbepx_var(i,j,1)
        Sfcprop(nb)%fire_GBBEPx(ix,2,ie)  = data%gbbepx_var(i,j,2)
        Sfcprop(nb)%fire_GBBEPx(ix,3,ie)  = data%gbbepx_var(i,j,3)
        Sfcprop(nb)%fire_GBBEPx(ix,4,ie)  = data%gbbepx_var(i,j,4)
        Sfcprop(nb)%fire_GBBEPx(ix,5,ie)  = data%gbbepx_var(i,j,5)
      enddo
    enddo
  end subroutine catchem_emissions_copy_gbbepx

  !>@ Destructor for catchem_emissions_type
  subroutine catchem_emissions_final(data)
    implicit none
    type(catchem_emissions_type) :: data

    ! This #define reduces code length by a lot
#define IF_ASSOC_DEALLOC_NULL(var) \
    if(associated(data%var)) then ; \
      deallocate(data%var) ; \
      nullify(data%var) ; \
    endif

    IF_ASSOC_DEALLOC_NULL(dust_name)
    IF_ASSOC_DEALLOC_NULL(emi_name)
    IF_ASSOC_DEALLOC_NULL(emi2_name)
    IF_ASSOC_DEALLOC_NULL(gbbepx_name)
    IF_ASSOC_DEALLOC_NULL(dust_var)
    IF_ASSOC_DEALLOC_NULL(emi_var)
    IF_ASSOC_DEALLOC_NULL(emi2_var)
    IF_ASSOC_DEALLOC_NULL(gbbepx_var)

    ! Undefine this to avoid cluttering the cpp scope:
#undef IF_ASSOC_DEALLOC_NULL
  end subroutine catchem_emissions_final
  ! --------------------------------------------------------------------

  !>@ Allocates temporary arrays and registers variables for reading am4 emissions.
  subroutine catchem_am4_register_emi(data, restart, Atm_block)
    implicit none
    class(catchem_am4_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:) :: var2_p => NULL()
    integer :: num, nx, ny

    if(associated(data%emi_name)) then
      deallocate(data%emi_name)
      nullify(data%emi_name)
    endif

    if(associated(data%emi_var)) then
      deallocate(data%emi_var)
      nullify(data%emi_var)
    endif

    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%emi_name(data%nvar_emi))
    allocate(data%emi_var(nx,ny,data%nvar_emi))

    data%emi_name(1)  = 'e_bc'
    data%emi_name(2)  = 'e_oc'
    data%emi_name(3)  = 'e_sulf'
    data%emi_name(4)  = 'e_pm_25'
    data%emi_name(5)  = 'e_so2'
    data%emi_name(6)  = 'e_pm_10'
    data%emi_name(7)  = 'dm0'
    data%emi_name(8)  = 'ero1'
    data%emi_name(9)  = 'ero2'
    data%emi_name(10) = 'ero3'
    data%emi_name(11)  = 'e_no'
    data%emi_name(12)  = 'e_no2'
    data%emi_name(13)  = 'e_nh3'
    data%emi_name(14)  = 'e_co'
    data%emi_name(15)  = 'e_ch4'
    data%emi_name(16)  = 'e_ch2o'
    data%emi_name(17)  = 'e_c2h4'
    data%emi_name(18)  = 'e_c2h6'
    data%emi_name(19)  = 'e_c3h6'
    data%emi_name(20)  = 'e_c3h8'
    data%emi_name(21)  = 'e_c4h10'
    data%emi_name(22)  = 'e_isop'
    data%emi_name(23)  = 'e_c10h16'
    data%emi_name(24)  = 'e_ch3oh'
    data%emi_name(25)  = 'e_c2h5oh'
    data%emi_name(26)  = 'e_ch3coch3'
    data%emi_name(27)  = 'e_h2'
    data%emi_name(28)  = 'e_e90'
    data%emi_name(29)  = 'ebio_isop'
    data%emi_name(30)  = 'ebio_c10h16'

    !--- register axis
    call register_axis( restart, "lon", 'X' )
    call register_axis( restart, "lat", 'Y' )
    !--- register the 2D fields
      do num = 1,data%nvar_emi
        var2_p => data%emi_var(:,:,num)
        call register_restart_field(restart, data%emi_name(num), var2_p, dimensions=(/'lat ', 'lon '/))
      enddo

  end subroutine catchem_am4_register_emi

  ! --------------------------------------------------------------------

  !>@ Called after register_emi() to copy data from internal arrays to the model grid and deallocate arrays
  subroutine catchem_am4_copy_emi(data, Sfcprop, Atm_block)
    implicit none
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    class(catchem_am4_type) :: data
    type(block_control_type), intent(in) :: Atm_block

    integer :: num, nb, i, j, ix

    if(.not.associated(data%emi_name) .or. .not.associated(data%emi_var)) then
      write(0,*) 'ERROR: Called copy_emi before register_emi'
      return
    endif

    do num=1,data%nvar_emi
      !$omp parallel do default(shared) private(i, j, nb, ix)
      do nb = 1, Atm_block%nblks
        !--- 2D variables
        do ix = 1, Atm_block%blksz(nb)
          i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
          j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
          Sfcprop(nb)%emi_in_cplchp(ix,num)  = data%emi_var(i,j,num)
        enddo
      enddo
    enddo

    deallocate(data%emi_name)
    nullify(data%emi_name)
    deallocate(data%emi_var)
    nullify(data%emi_var)
  end subroutine catchem_am4_copy_emi

  ! --------------------------------------------------------------------

  !>@ Allocates temporary arrays and registers variables for reading the fire data file.
  subroutine catchem_am4_register_gbbepx(data, restart, Atm_block)
    implicit none
    class(catchem_am4_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:) :: var2_p => NULL()
    integer :: num, nx, ny


    if(associated(data%gbbepx_name)) then
      deallocate(data%gbbepx_name)
      nullify(data%gbbepx_name)
    endif

    if(associated(data%gbbepx_var)) then
      deallocate(data%gbbepx_var)
      nullify(data%gbbepx_var)
    endif


    !--- allocate the various containers needed for rrfssd fire data
    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%gbbepx_name(data%nvar_gbbepx))
    allocate(data%gbbepx_var(nx,ny,data%nvar_gbbepx))
    data%gbbepx_name(1)  = 'ebu_bc'
    data%gbbepx_name(2)  = 'ebu_oc'
    data%gbbepx_name(3)  = 'ebu_pm_25'
    data%gbbepx_name(4)  = 'ebu_so2'
    data%gbbepx_name(5)  = 'ebu_frp'
    data%gbbepx_name(6)  = 'ebu_ch3coch3'
    data%gbbepx_name(7)  = 'ebu_c2h4'
    data%gbbepx_name(8)  = 'ebu_c2h5oh'
    data%gbbepx_name(9)  = 'ebu_c2h6'
    data%gbbepx_name(10) = 'ebu_c3h6'
    data%gbbepx_name(11) = 'ebu_c3h8'
    data%gbbepx_name(12) = 'ebu_c4h10'
    data%gbbepx_name(13) = 'ebu_ch2o'
    data%gbbepx_name(14) = 'ebu_ch3oh'
    data%gbbepx_name(15) = 'ebu_ch4'
    data%gbbepx_name(16) = 'ebu_co'
    data%gbbepx_name(17) = 'ebu_h2'
    data%gbbepx_name(18) = 'ebu_isop'
    data%gbbepx_name(19) = 'ebu_nh3'
    data%gbbepx_name(20) = 'ebu_no'
    data%gbbepx_name(21) = 'ebu_c10h16'

    !--- register axis
    call register_axis(restart, 'lon', 'X')
    call register_axis(restart, 'lat', 'Y')
    !--- register the 2D fields


    do num = 1,data%nvar_gbbepx
      var2_p => data%gbbepx_var(:,:,num)
      call register_restart_field(restart, data%gbbepx_name(num), var2_p, dimensions=(/'lat ', 'lon '/))
    enddo

  end subroutine catchem_am4_register_gbbepx

  ! --------------------------------------------------------------------

  !>@ Called after register_fire() to copy data from internal arrays to the model grid and deallocate arrays
  subroutine catchem_am4_copy_gbbepx(data, Sfcprop, Atm_block,ie)
    implicit none
    class(catchem_am4_type) :: data
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    type(block_control_type), intent(in) :: Atm_block
    integer, intent (in) :: ie

    integer :: num, nb, ix, k, i, j

    do num=1,data%nvar_gbbepx
      !$omp parallel do default(shared) private(i, j, nb, ix, k)
      do nb = 1, Atm_block%nblks
        do ix = 1, Atm_block%blksz(nb)
          i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
          j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
          Sfcprop(nb)%fire_GBBEPx(ix,num,ie) = data%gbbepx_var(i,j,num)
        enddo
      enddo
    enddo
  end subroutine catchem_am4_copy_gbbepx

  ! --------------------------------------------------------------------
  !>@ Allocates temporary arrays and registers variables for reading the GOCART background file.
  subroutine catchem_am4_register_chemic(data, restart, Atm_block)
    implicit none
    class(catchem_am4_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p2 => NULL()
    integer :: num, nx, ny

    if(associated(data%chemic_name)) then
      deallocate(data%chemic_name)
      nullify(data%chemic_name)
    endif

    if(associated(data%chemic_var)) then
      deallocate(data%chemic_var)
      nullify(data%chemic_var)
    endif

    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%chemic_name(data%nvar_chemic))
    allocate(data%chemic_var(nx,ny,49,data%nvar_chemic))  ! AM4 level

    data%chemic_name(1) = 'co'
    data%chemic_name(2) = 'o3'
    data%chemic_name(3) = 'n2o'
    data%chemic_name(4) = 'no'
    data%chemic_name(5) = 'no2'
    data%chemic_name(6) = 'hno3'
    data%chemic_name(7) = 'n2o5'
    data%chemic_name(8) = 'ch4'
    data%chemic_name(9) = 'pan'
    data%chemic_name(10) = 'c2h6'
    data%chemic_name(11) = 'ch3coch3'
    data%chemic_name(12) = 'hcl'
    data%chemic_name(13) = 'hocl'
    data%chemic_name(14) = 'clono2'
    data%chemic_name(15) = 'clo'
    data%chemic_name(16) = 'hobr'
    data%chemic_name(17) = 'hbr'
    data%chemic_name(18) = 'brono2'
    data%chemic_name(19) = 'bro'
    data%chemic_name(20) = 'age'
    data%chemic_name(21) = 'cl'
    data%chemic_name(22) = 'cl2'
    data%chemic_name(23) = 'cl2o2'
    data%chemic_name(24) = 'br'
    data%chemic_name(25) = 'brcl'
    data%chemic_name(26) = 'extinction'

    !--- register axis
    call register_axis(restart, 'grid_xt', 'X')
    call register_axis(restart, 'grid_yt', 'Y')
    call register_axis(restart, 'pfull', 49)
    !--- register the 2D fields
      do num = 1,data%nvar_chemic
        var3_p2 => data%chemic_var(:,:,:,num)
        call register_restart_field(restart, data%chemic_name(num), var3_p2,dimensions=(/'pfull', 'grid_yt', 'grid_xt'/),&
                                  &is_optional=.true.)
      enddo

  end subroutine catchem_am4_register_chemic

  ! --------------------------------------------------------------------
  !>@ Called after register_emi() to copy data from internal arrays to the model
  !grid and deallocate arrays
  subroutine catchem_am4_copy_chemic(data, Sfcprop, Atm_block)
    implicit none
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    class(catchem_am4_type) :: data
    type(block_control_type), intent(in) :: Atm_block

    integer :: num, nb, i, j, k, ix

    if(.not.associated(data%chemic_name) .or. .not.associated(data%chemic_var)) then
      write(0,*) 'ERROR: Called copy_chemic before register_emi'
      return
    endif

    do num=1,data%nvar_chemic
      !$omp parallel do default(shared) private(i, j, nb, ix)
      do nb = 1, Atm_block%nblks
        !--- 2D variables
        do ix = 1, Atm_block%blksz(nb)
          i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
          j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
          do k = 1, 49
            Sfcprop(nb)%chemic_in(ix,k,num)  = data%chemic_var(i,j,k,num)
          enddo
        enddo
      enddo
    enddo

    deallocate(data%chemic_name)
    nullify(data%chemic_name)
    deallocate(data%chemic_var)
    nullify(data%chemic_var)
  end subroutine catchem_am4_copy_chemic

  ! --------------------------------------------------------------------

  !>@ Allocates temporary arrays and registers variables for reading the GOCART background file.
  subroutine catchem_am4_register_dfdage(data, restart, Atm_block)
    implicit none
    class(catchem_am4_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:,:) :: var3_p2 => NULL()
    integer :: num, nx, ny

    if(associated(data%dfdage_name)) then
      deallocate(data%dfdage_name)
      nullify(data%dfdage_name)
    endif

    if(associated(data%dfdage_var)) then
      deallocate(data%dfdage_var)
      nullify(data%dfdage_var)
    endif

    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%dfdage_name(data%nvar_dfdage))
    allocate(data%dfdage_var(nx,ny,72,data%nvar_dfdage)) ! AM4 input

    data%dfdage_name(1)  = 'dfdage_cfc11'
    data%dfdage_name(2)  = 'dfdage_cfc12'
    data%dfdage_name(3)  = 'dfdage_cfc113'
    data%dfdage_name(4)  = 'dfdage_ccl4'
    data%dfdage_name(5)  = 'dfdage_ch3cl'
    data%dfdage_name(6)  = 'dfdage_ch3ccl3'
    data%dfdage_name(7)  = 'dfdage_hcfc22'
    data%dfdage_name(8)  = 'dfdage_bry'

    !--- register axis
    call register_axis(restart, 'grid_xt', 'X')
    call register_axis(restart, 'grid_yt', 'Y')
    call register_axis(restart, 'pfull', 72)
    !--- register the 2D fields
      do num = 1,data%nvar_dfdage
        var3_p2 => data%dfdage_var(:,:,:,num)
        call register_restart_field(restart, data%dfdage_name(num), var3_p2,dimensions=(/'pfull', 'grid_yt', 'grid_xt'/),&
                                  &is_optional=.true.)
      enddo

  end subroutine catchem_am4_register_dfdage
  ! --------------------------------------------------------------------

  !>@ Called after register_emi() to copy data from internal arrays to the model
  !grid and deallocate arrays
  subroutine catchem_am4_copy_dfdage(data, Sfcprop, Atm_block)
    implicit none
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    class(catchem_am4_type) :: data
    type(block_control_type), intent(in) :: Atm_block

    integer :: num, nb, i, j, k, ix

    if(.not.associated(data%dfdage_name) .or. .not.associated(data%dfdage_var)) then
      write(0,*) 'ERROR: Called copy_dfdage before register_emi'
      return
    endif

    do num=1,data%nvar_dfdage
      !$omp parallel do default(shared) private(i, j, nb, ix)
      do nb = 1, Atm_block%nblks
        !--- 2D variables
        do ix = 1, Atm_block%blksz(nb)
          i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
          j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
          do k = 1, 72  ! 
            Sfcprop(nb)%dfdage_in(ix,k,num)  = data%dfdage_var(i,j,k,num)
          enddo
        enddo
      enddo
    enddo

    deallocate(data%dfdage_name)
    nullify(data%dfdage_name)
    deallocate(data%dfdage_var)
    nullify(data%dfdage_var)
  end subroutine catchem_am4_copy_dfdage

  ! --------------------------------------------------------------------

  !>@ Allocates temporary arrays and registers variables for reading the depvel file
  subroutine catchem_am4_register_depvel(data, restart, Atm_block)
    implicit none
    class(catchem_am4_type) :: data
    type(FmsNetcdfDomainFile_t) :: restart
    type(block_control_type), intent(in) :: Atm_block

    real(kind=kind_phys), pointer, dimension(:,:) :: var2_p => NULL()
    integer :: num, nx, ny

    if(associated(data%depvel_name)) then
      deallocate(data%depvel_name)
      nullify(data%depvel_name)
    endif

    if(associated(data%depvel_var)) then
      deallocate(data%depvel_var)
      nullify(data%depvel_var)
    endif

    call get_nx_ny_from_atm(Atm_block, nx, ny)
    allocate(data%depvel_name(data%nvar_depvel))
    allocate(data%depvel_var(nx,ny,data%nvar_depvel))

    data%depvel_name(1) = 'co'
    data%depvel_name(2) = 'ch2o'
    data%depvel_name(3) = 'o3'
    data%depvel_name(4) = 'no'
    data%depvel_name(5) = 'no2'
    data%depvel_name(6) = 'hno3'
    data%depvel_name(7) = 'hno4'
    data%depvel_name(8) = 'n2o5'
    data%depvel_name(9) = 'ch4'
    data%depvel_name(10) = 'ch3ooh'
    data%depvel_name(11) = 'h2o2'
    data%depvel_name(12) = 'pan'
    data%depvel_name(13) = 'pmn'
    data%depvel_name(14) = 'ch3coch3'
    data%depvel_name(15) = 'glyc'
    data%depvel_name(16) = 'hac'
    data%depvel_name(17) = 'rip'
    data%depvel_name(18) = 'so2'
    data%depvel_name(19) = 'nh3'
    data%depvel_name(20) = 'hobr'
    data%depvel_name(21) = 'hbr'
    data%depvel_name(22) = 'brno3'

    !--- register axis
    call register_axis( restart, "grid_xt", 'X' )
    call register_axis( restart, "grid_yt", 'Y' )
    !--- register the 2D fields
      do num = 1,data%nvar_depvel
        var2_p => data%depvel_var(:,:,num)
        call register_restart_field(restart, data%depvel_name(num), var2_p, dimensions=(/'grid_yt', 'grid_xt'/))
      enddo

  end subroutine catchem_am4_register_depvel

  ! --------------------------------------------------------------------

  !>@ Called after register_depvel() to copy data from internal arrays to the model grid and deallocate arrays
  subroutine catchem_am4_copy_depvel(data, Sfcprop, Atm_block)
    implicit none
    type(GFS_sfcprop_type),    intent(inout) :: Sfcprop(:)
    class(catchem_am4_type) :: data
    type(block_control_type), intent(in) :: Atm_block

    integer :: num, nb, i, j, ix

    if(.not.associated(data%depvel_name) .or. .not.associated(data%depvel_var)) then
      write(0,*) 'ERROR: Called copy_depvel before register_depvel'
      return
    endif

    do num=1,data%nvar_depvel
      !$omp parallel do default(shared) private(i, j, nb, ix)
      do nb = 1, Atm_block%nblks
        !--- 2D variables
        do ix = 1, Atm_block%blksz(nb)
          i = Atm_block%index(nb)%ii(ix) - Atm_block%isc + 1
          j = Atm_block%index(nb)%jj(ix) - Atm_block%jsc + 1
          Sfcprop(nb)%depvel_in(ix,num)  = data%depvel_var(i,j,num)
        enddo
      enddo
    enddo

    deallocate(data%depvel_name)
    nullify(data%depvel_name)
    deallocate(data%depvel_var)
    nullify(data%depvel_var)
  end subroutine catchem_am4_copy_depvel

  ! --------------------------------------------------------------------

  !>@ Destructor for catchem_am4_type
  subroutine catchem_am4_final(data)
    implicit none
    type(catchem_am4_type) :: data

    ! This #define reduces code length by a lot
#define IF_ASSOC_DEALLOC_NULL(var) \
    if(associated(data%var)) then ; \
      deallocate(data%var) ; \
      nullify(data%var) ; \
    endif

    IF_ASSOC_DEALLOC_NULL(emi_name)
    IF_ASSOC_DEALLOC_NULL(gbbepx_name)
    IF_ASSOC_DEALLOC_NULL(chemic_name)
    IF_ASSOC_DEALLOC_NULL(dfdage_name)
    IF_ASSOC_DEALLOC_NULL(depvel_name)
    IF_ASSOC_DEALLOC_NULL(emi_var)
    IF_ASSOC_DEALLOC_NULL(gbbepx_var)
    IF_ASSOC_DEALLOC_NULL(chemic_var)
    IF_ASSOC_DEALLOC_NULL(dfdage_var)
    IF_ASSOC_DEALLOC_NULL(depvel_var)

    ! Undefine this to avoid cluttering the cpp scope:
#undef IF_ASSOC_DEALLOC_NULL
  end subroutine catchem_am4_final

end module fv3atm_catchem_io

!> @}
