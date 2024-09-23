MODULE Topographic_parameters_computation

  !__________________________________________________________________________________________________________________________!
  !Interface script calling all geometry-based modules to compute the different topographical indicators required to downscale
  !temperatures and precipitation fields.
  !__________________________________________________________________________________________________________________________!


  USE Parametrization
  USE Support_functions, only: accessing_config_file
  USE WL_gridpoints_selection, ONLY: filling_WL_patterns_arrays

  IMPLICIT NONE

  INTEGER, PRIVATE :: m, i, j, k
  
CONTAINS

  !_______________________________________________________!
  !Simple subroutine used to calculate elevation anomalies between low resolution (LR)
  !DEM and high resolution (HR) DEM. Note that the LR grid must be first interpolated
  !on the HR one
  SUBROUTINE computing_elevation_anomalies(lr_surface_elevation_data, hr_surface_elevation_data, elevation_anomalies_data)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: lr_surface_elevation_data, &
         hr_surface_elevation_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: elevation_anomalies_data

    ALLOCATE(elevation_anomalies_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))

    elevation_anomalies_data(:,:,:) = lr_surface_elevation_data(:,:,:) - hr_surface_elevation_data(:,:,:)
    PRINT*,"_______________________________"
    PRINT*, "elevation anomalies lr-hr :", (sum(elevation_anomalies_data))/(hr_topo_x_size*hr_topo_y_size*hr_topo_t_size)
    PRINT*, "elevation anomalies max :", (MAXVAL(elevation_anomalies_data))
    PRINT*, "elevation anomalies min :", (MINVAL(elevation_anomalies_data))
 
  END SUBROUTINE computing_elevation_anomalies


  SUBROUTINE computing_insolation_anomalies(lr_topographic_insolation_data, hr_topographic_insolation_data, &
       topographic_insolation_anomalies_data)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: lr_topographic_insolation_data, &
         hr_topographic_insolation_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: topographic_insolation_anomalies_data

    ALLOCATE(topographic_insolation_anomalies_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))

    topographic_insolation_anomalies_data(:,:,:) = lr_topographic_insolation_data(:,:,:) - hr_topographic_insolation_data(:,:,:)

    PRINT*,"_______________________________"
    PRINT*, "insolation anomalies lr-hr :", (sum(topographic_insolation_anomalies_data))/&
         (hr_topo_x_size*hr_topo_y_size*hr_topo_t_size)
    PRINT*, "insolation anomalies max :", (MAXVAL(topographic_insolation_anomalies_data))
    PRINT*, "insolation anomalies min :", (MINVAL(topographic_insolation_anomalies_data))
    PRINT*,"_______________________________"
    
  END SUBROUTINE computing_insolation_anomalies
 

!______________________________________________________________________________________________________________________________!

  SUBROUTINE computing_WL_exposure_indexes(TEI_pointers_array, config_namelist_blockname, ios, fu) 

    IMPLICIT NONE

    TYPE(tei_arr), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: TEI_pointers_array
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu
    

    !__________________________________________________________________!
    !Reading TEI related information stored in the Configuration file
    !__________________________________________________________________!"
    
    config_namelist_blockname="Downscaled_outputs"
    CALL accessing_config_file(ios, fu)
     
    ALLOCATE(TEI_pointers_array(nbr_wdir))
    DO m=1, nbr_wdir
       ALLOCATE(TEI_pointers_array(m)%tei_arr_ptr(1:hr_topo_x_size, 1:hr_topo_y_size))
    END DO

    DO m=1, nbr_wdir
       TEI_pointers_array(m)%tei_arr_ptr(:,:) = 0.0
    END DO

    CALL filling_WL_patterns_arrays(WL_pattern_pointers_array, wdir_angle_boundaries)
    
    DO m=1, nbr_wdir
       DO i=1, hr_topo_x_size
          DO j=1, hr_topo_y_size
             DO k=1, SIZE(WL_pattern_pointers_array(m)%wl_arr_ptr)
                IF ((i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .GE. 1) &
                     .AND. (i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .LE. hr_topo_x_size) &
                     .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .GE. 1) &
                     .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .LE. hr_topo_y_size) &
                     .AND. (WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .NE. -9999)) THEN
                   TEI_pointers_array(m)%tei_arr_ptr(i, j) = TEI_pointers_array(m)%tei_arr_ptr(i, j) + 1!TEI_pointers_array(m)%tei_arr_ptr(i, j) + &
                       ! hr_surface_elevation_data(i + &
                       ! WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, &
                   ! j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative, 1)
               END IF
             END DO
          END DO
       END DO
    END DO
    
    END SUBROUTINE computing_WL_exposure_indexes

  
END MODULE Topographic_parameters_computation
