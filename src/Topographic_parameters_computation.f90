MODULE Topographic_parameters_computation

  !__________________________________________________________________________________________________________________________!
  !Interface script calling all geometry-based modules to compute the different topographical indicators required to downscale
  !temperatures and precipitation fields.
  !__________________________________________________________________________________________________________________________!


  USE Parametrization
  USE Support_functions, only: accessing_config_file
  USE WL_gridpoints_selection, ONLY: filling_WL_patterns_arrays

  IMPLICIT NONE

  INTEGER, PRIVATE :: m, i, j, k, counter
  
CONTAINS

  !__________________________________________________________________________________________________________________________!
  !Simple subroutine used to calculate elevation anomalies between high resolution (HR)
  !DEM and low resolution (LR) DEM. Note that the LR grid must be interpolated
  !first on the HR one
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


  !___________________________________________________________________________________________________________________________!
  !This routine computes the topographic insolation anomalies between the HR and
  !the LR resolution radiation maps built using the software GRASS-GIS
  !(pretreatment)
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
!Subroutine used to compute for each gridpoint the nbr_wdir TEI. The nbr_wdir TEI arrays are pointed by the pointers stored in the 
!TEI_pointers_array
  SUBROUTINE computing_WL_exposure_indexes(TEI_pointers_array, TEI_drying_effect_correction, &
        config_namelist_blockname, ios, fu) 

    IMPLICIT NONE

    TYPE(tei_arr), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: TEI_pointers_array, TEI_drying_effect_correction
    CHARACTER(LEN=str_len), INTENT(INOUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu
    
    !__________________________________________________________________!
    !Reading TEI related information stored in the Configuration file
    !__________________________________________________________________!"
    
    config_namelist_blockname="Downscaled_outputs"
    CALL accessing_config_file(ios, fu)
    
    ALLOCATE(TEI_pointers_array(nbr_wdir))
    IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
        ALLOCATE(TEI_drying_effect_correction(nbr_wdir))
    END IF
    DO m=1, nbr_wdir
       ALLOCATE(TEI_pointers_array(m)%tei_arr_ptr(1:hr_topo_x_size, 1:hr_topo_y_size))
       IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
          ALLOCATE(TEI_drying_effect_correction(m)%tei_arr_ptr(1:hr_topo_x_size,1:hr_topo_y_size))
       END IF
    END DO

    DO m=1, nbr_wdir
       TEI_pointers_array(m)%tei_arr_ptr(:,:) = 0d0
       IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN  
          TEI_drying_effect_correction(m)%tei_arr_ptr(:,:) = 0d0
       END IF
    END DO

    CALL filling_WL_patterns_arrays(WL_pattern_pointers_array, wdir_angle_boundaries)
    
    !Hereafter, the algorithm loops over every gridpoints for each of the nbr_wdir wind directions. For each cell, it retrieves the
    !relative coordinates and horizontal distance (weight) of each of the influence points in the windward direction, IF the
    !influence point is within the gridbox (IF conditions). The characteristics of the influence gridpoints are read in the
    !topography-related array (e.g. elevation) using the relative coordinates of the influence gridpoint, and the absolute
    !coordinate of the cell whose TEI is being calculated.
    
    DO m=1, nbr_wdir                                                                                              
       DO j=1, hr_topo_y_size
          DO i=1, hr_topo_x_size        
             DO k=1, SIZE(WL_pattern_pointers_array(m)%wl_arr_ptr)
                IF ((i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .GE. 1d0) &                            
                     .AND. (i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .LE. hr_topo_x_size) &
                     .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .GE. 1) &
                     .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .LE. hr_topo_y_size) &
                     .AND. (WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .NE. -9999)) THEN
                   IF ((WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist .GT. 0d0) &
                     .AND. (WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist .LE. &
                      TEI_windward_searching_dist)) THEN                                                        !Since each influence gridpoint weight is
                      TEI_pointers_array(m)%tei_arr_ptr(i, j) = TEI_pointers_array(m)%tei_arr_ptr(i, j) + &   !given by 1/horizontal distance between 
                        (hr_surface_elevation_data(i, j, 1) - hr_surface_elevation_data(i + &                 !the gridpoint of influence and the cell the TEI's
                        WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, &                             !is being computed, it is necessary to check
                        j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative, 1))* &                    !if horizontal_dist is equal to 0 in order to
                        1d0/WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist                        !avoid errors risen by null divisions                           
                   END IF
               END IF
             END DO
          END DO
       END DO
    END DO
    
    !___________________________________________________________________________________________________  
    !ACTIVATED IF broad_mountain_range_drying_effect_activator = .TRUE.
    !(Configuration_file.nml
    !___________________________________________________________________________________________________

    !The following loops are activated only if the user wants to take into account the
    !drying effect associated with broad mountains barriers (e.g. Alps, Tibetan
    !Plateau). In such contexts, most of the incoming precipitation occurs at
    !the boundaries of the mountain range, leaving the interior areas relatively
    !dry, even if they might be more exposed
    counter=0.d0 
    IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
       DO m=1, nbr_wdir                                                                                              
          DO j=1, hr_topo_y_size
             DO i=1, hr_topo_x_size        
                DO k=1, SIZE(WL_pattern_pointers_array(m)%wl_arr_ptr)
                   IF ((i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .GE. 1d0) &                            
                        .AND. (i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .LE. hr_topo_x_size) &
                        .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .GE. 1.d0) &
                        .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .LE. hr_topo_y_size) &
                        .AND. (WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .NE. -9999)) THEN
                           counter = counter + 1.d0
                           IF (TEI_pointers_array(m)%tei_arr_ptr(i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, & 
                           j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative) .GT. 0.d0) THEN
                              TEI_drying_effect_correction(m)%tei_arr_ptr(i, j) = TEI_drying_effect_correction(m)%tei_arr_ptr(i, j) + &
                               TEI_pointers_array(m)%tei_arr_ptr(i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, &
                               j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative)                 
                           END IF
                    END IF
                END DO

                IF (counter .NE. 0.d0) THEN
                   TEI_drying_effect_correction(m)%tei_arr_ptr(i,j)=TEI_drying_effect_correction(m)%tei_arr_ptr(i, j)/counter
                   counter=0.d0
                END IF

             END DO
          END DO
       END DO
    END IF

    
    !The following loop is used to correct the TEI values stored in the TEI_pointers_array 
    !the TEI_drying_effect_correction values. A calibration coefficient delta is
    !used to fit the drying effect to observations
    IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
       DO m=1, nbr_wdir
          DO j=1, hr_topo_y_size
             DO i=1, hr_topo_x_size
               IF (TEI_drying_effect_correction(m)%tei_arr_ptr(i, j) .GT. 0.d0) THEN
                  TEI_pointers_array(m)%tei_arr_ptr(i,j) = TEI_pointers_array(m)%tei_arr_ptr(i,j) - &
                  delta * TEI_drying_effect_correction(m)%tei_arr_ptr(i, j)
               END IF                   
             END DO
          END DO
       END DO
    END IF


!Loop for converting TEI to P multiplicative factor

    DO m=1, nbr_wdir
       DO j=1, hr_topo_y_size
          DO i=1, hr_topo_x_size
            IF (exp(TEI_pointers_array(m)%tei_arr_ptr(i,j)/100) .GT. max_precipitation_increase_factor) THEN 
               TEI_pointers_array(m)%tei_arr_ptr(i,j) = max_precipitation_increase_factor
            ELSE 
               TEI_pointers_array(m)%tei_arr_ptr(i,j) = exp(TEI_pointers_array(m)%tei_arr_ptr(i,j)/100)
            END IF
          END DO
       END DO
    END DO
    


!do m=1, nbr_wdir
!   TEI_pointers_array(m)%tei_arr_ptr=0.d0
!   TEI_pointers_array(m)%tei_arr_ptr=TEI_drying_effect_correction(m)%tei_arr_ptr
!end do

   
   END SUBROUTINE computing_WL_exposure_indexes

  
END MODULE Topographic_parameters_computation
