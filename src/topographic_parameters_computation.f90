MODULE Topographic_parameters_computation

  !__________________________________________________________________________________________________________________________!
  !Interface script calling all geometry-based modules to compute the different topographical indicators required to downscale
  !temperatures and precipitation fields.
  !__________________________________________________________________________________________________________________________!


  USE Parametrization
  USE Support_functions, only: accessing_config_file
  USE WL_gridpoints_selection, ONLY: filling_WL_patterns_arrays

  IMPLICIT NONE

  INTEGER, PRIVATE :: m, i, j, k, counter_TEI, counter_DE
  
CONTAINS

  !__________________________________________________________________________________________________________________________!
  !Simple subroutine used to calculate elevation anomalies between high resolution (HR)
  !DEM and low resolution (LR) DEM. Note that the LR grid must be interpolated
  !first on the HR one
  SUBROUTINE computing_elevation_anomalies(locvar__lr_surface_elevation_array, locvar__hr_surface_elevation_array, locvar_elevation_anomalies_array)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: locvar__lr_surface_elevation_array, &
         locvar__hr_surface_elevation_array
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: locvar_elevation_anomalies_array

    ALLOCATE(locvar_elevation_anomalies_array(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))

    locvar_elevation_anomalies_array(:,:,:) = locvar__lr_surface_elevation_array(:,:,:) - locvar__hr_surface_elevation_array(:,:,:)
  END SUBROUTINE computing_elevation_anomalies


  !___________________________________________________________________________________________________________________________!
  !This routine computes the topographic insolation anomalies between the HR and
  !the LR resolution radiation maps built using the software GRASS-GIS
  !(pretreatment)
  SUBROUTINE computing_insolation_anomalies(locvar__lr_topographic_insolation_array, locvar__hr_topographic_insolation_array, &
       locvar__topographic_insolation_anomalies_array)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: locvar__lr_topographic_insolation_array, &
         locvar__hr_topographic_insolation_array
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: locvar__topographic_insolation_anomalies_array

    ALLOCATE(locvar__topographic_insolation_anomalies_array(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))

    locvar__topographic_insolation_anomalies_array(:,:,:) = locvar__lr_topographic_insolation_array(:,:,:) - locvar__hr_topographic_insolation_array(:,:,:)

  END SUBROUTINE computing_insolation_anomalies
 

!______________________________________________________________________________________________________________________________!
!Subroutine used to compute for each gridpoint the nbr_wdir TEI. The nbr_wdir TEI arrays are pointed by the pointers stored in the 
!TEI_pointers_array
  SUBROUTINE computing_WL_exposure_indexes(locvar__TEI_pointers_array, locvar__TEI_drying_effect_correction_array)

    IMPLICIT NONE

    TYPE(tei_arr), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: locvar__TEI_pointers_array, locvar__TEI_drying_effect_correction_array
   
    !__________________________________________________________________!
    !Reading TEI related information stored in the Configuration file
    !__________________________________________________________________!"
    
    !config_namelist_blockname="Downscaled_outputs"
    !CALL accessing_config_file(ios, fu)
    
    !ALLOCATE(locvar__TEI_pointers_array(nbr_wdir))
    !IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
    !    ALLOCATE(locvar__TEI_drying_effect_correction_array(nbr_wdir))
    !END IF
    !DO m=1, nbr_wdir
    !   ALLOCATE(locvar__TEI_pointers_array(m)%tei_arr_ptr(1:hr_topo_x_size, 1:hr_topo_y_size))
    !   IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
    !      ALLOCATE(locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr(1:hr_topo_x_size,1:hr_topo_y_size))
    !   END IF
    !END DO

    !DO m=1, nbr_wdir
    !   locvar__TEI_pointers_array(m)%tei_arr_ptr(:,:) = 0d0
    !   IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN  
    !      locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr(:,:) = 0d0
    !   END IF
    !END DO

    !Hereafter, the algorithm loops over every gridpoints for each of the nbr_wdir wind directions. For each cell, it retrieves the
    !relative coordinates and horizontal distance (weight) of each of the influence points in the windward direction, IF the
    !influence point is within the gridbox (IF conditions). The characteristics of the influence gridpoints are read in the
    !topography-related array (e.g. elevation) using the relative coordinates of the influence gridpoint, and the absolute
    !coordinate of the cell whose TEI is being calculated.
    
    DO m=1, nbr_wdir                                                                                              
       DO j=1, hr_topo_y_size
          DO i=1, hr_topo_x_size
             counter_TEI = 0        
             DO k=1, SIZE(WL_pattern_pointers_array(m)%wl_arr_ptr)
                !The following condition is used to manage boundaries grid points 
                IF ((i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .GE. 1d0) &                            
                .AND. (i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .LE. hr_topo_x_size) &
                .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .GE. 1) &
                .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .LE. hr_topo_y_size) &
                .AND. (WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .NE. -999)) THEN
                !Only the points within the searching distance chosen by the user are used to compute the TEI
                   IF ((WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist .GT. 0d0) &
                   .AND. (WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist .LE. &
                   TEI_windward_searching_dist)) THEN
                        !The following condition prevents from taking into account grid points with error codes 
                        !in the TEI calculation
                        IF ((hr_surface_elevation_data(i, j, 1) .GT. missing_data_error_code) &
                        .AND. (hr_surface_elevation_data(i + &       !the gridpoint of influence and the cell the TEI's
                        WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, &                                       !is being computed, it is necessary to check
                        j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative, 1) .GT. &
                        missing_data_error_code)) THEN
                                counter_TEI = counter_TEI + 1                 !Since each influence gridpoint weight is
                                locvar__TEI_pointers_array(m)%tei_arr_ptr(i, j) = &
                                locvar__TEI_pointers_array(m)%tei_arr_ptr(i, j) + &   !given by 1/horizontal distance between 
                                (hr_surface_elevation_data(i, j, 1) - hr_surface_elevation_data(i + &                   !the gridpoint of influence and the cell the TEI's
                                WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, &                               !is being computed, it is necessary to check
                                j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative, 1))* &                      !if horizontal_dist is equal to 0 in order to
                                1d0/WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist
                        END IF                                                                                        !avoid errors risen by null divisions                           
                   END IF
               END IF
             END DO

             !The following IF condition is used to be consistent while computing the TEI for different spatial resolutions
             !(counter_TEI). It ensures that the TEI remains stable when switching to a finer scale DEM, if no additional topographic information 
             !is added when increasing the resolution. The term "TEI_windward_searching_dist/1000.0" is used to preserve the
             !relationship between TEI and the windward searching distance when it is increased by the user (a greater searching
             !distance is expected to generate higher TEI variations)
             IF (counter_TEI .NE. 0) THEN
                   locvar__TEI_pointers_array(m)%tei_arr_ptr(i, j) = (locvar__TEI_pointers_array(m)%tei_arr_ptr(i, j)/counter_TEI)! * &
                           !TEI_windward_searching_dist/1000.0                                                           
             END IF
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
    counter_DE=0 
    IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
       DO m=1, nbr_wdir                                                                                              
          DO j=1, hr_topo_y_size
             DO i=1, hr_topo_x_size        
                DO k=1, SIZE(WL_pattern_pointers_array(m)%wl_arr_ptr)
                   IF ((i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .GE. 1d0) &                            
                        .AND. (i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .LE. hr_topo_x_size) &
                        .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .GE. 1.d0) &
                        .AND. (j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative .LE. hr_topo_y_size) &
                        .AND. (WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative .NE. -999)) THEN
                           counter_DE = counter_DE + 1
                           IF (locvar__TEI_pointers_array(m)%tei_arr_ptr(i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, & 
                           j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative) .GT. 0.d0) THEN

                              locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr(i, j) = &
                               locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr(i, j) + &
                               locvar__TEI_pointers_array(m)%tei_arr_ptr(i + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, &
                               j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative)

                           END IF
                    END IF
                END DO

                IF (counter_DE .NE. 0) THEN
                   locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr(i,j)= &
                           locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr(i, &
                   j)/counter_DE
                   counter_DE=0
                END IF

             END DO
          END DO
       END DO
    END IF

    
    !The following loop is used to correct the TEI values stored in the locvar__TEI_pointers_array 
    !the locvar__TEI_drying_effect_correction_array values. A calibration coefficient delta is
    !used to fit the drying effect to observations
    IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
       DO m=1, nbr_wdir
          DO j=1, hr_topo_y_size
             DO i=1, hr_topo_x_size
               IF (locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr(i, j) .GT. 0.d0) &
                       !.AND. (locvar__TEI_pointers_array(m)%tei_arr_ptr(i,j) .GT. 1.d0)) &
                       THEN
                  locvar__TEI_pointers_array(m)%tei_arr_ptr(i,j) = locvar__TEI_pointers_array(m)%tei_arr_ptr(i,j) - &
                  delta * ((locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr(i, j)) * &
                  hr_surface_elevation_data(i,j,1)**3)
               END IF                   
             END DO
          END DO
       END DO
    END IF


!Loop for converting TEI to P multiplicative factor

!    DO m=1, nbr_wdir
!       DO j=1, hr_topo_y_size
!          DO i=1, hr_topo_x_size
!            IF (exp(locvar__TEI_pointers_array(m)%tei_arr_ptr(i,j)/100) .GT. max_precipitation_increase_factor) THEN 
!               locvar__TEI_pointers_array(m)%tei_arr_ptr(i,j) = max_precipitation_increase_factor
!            ELSE 
!               locvar__TEI_pointers_array(m)%tei_arr_ptr(i,j) = exp(TEI_pointers_array(m)%tei_arr_ptr(i,j)/100)
!            END IF
!          END DO
!       END DO
!    END DO
    


!do m=1, nbr_wdir
!   locvar__TEI_pointers_array(m)%tei_arr_ptr=0.d0
!   locvar__TEI_pointers_array(m)%tei_arr_ptr=locvar__TEI_drying_effect_correction_array(m)%tei_arr_ptr
!end do

   
   END SUBROUTINE computing_WL_exposure_indexes

  
END MODULE Topographic_parameters_computation
