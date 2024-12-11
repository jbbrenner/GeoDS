!Ancienne version de calcul du TEI avec approche proposée par Aurélien
!(construire un  maximum qui ne dépende pas du domaine choisi)

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
  DOUBLE PRECISION, PRIVATE :: elev_max, TEI_global_minimum, TEI_global_maximum
  DOUBLE PRECISION, DIMENSION(:,:), ALLOCATABLE, PRIVATE :: TEI_minima, TEI_maxima
  
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

!Subroutine used to compute for each gridpoint the nbr_wdir TEI. The nbr_wdir TEI arrays are pointed by the pointers stored in the 
!TEI_pointers_array
  SUBROUTINE computing_WL_exposure_indexes(TEI_pointers_array, config_namelist_blockname, ios, fu) 

    IMPLICIT NONE

    TYPE(tei_arr), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: TEI_pointers_array
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu
    

    !__________________________________________________________________!
    !Computing TEI global extrema for rescaling
    !__________________________________________________________________!

    elev_max=9000d0

    !__________________________________________________________________!
    !Reading TEI related information stored in the Configuration file
    !__________________________________________________________________!"
    
    config_namelist_blockname="Downscaled_outputs"
    CALL accessing_config_file(ios, fu)
    
    ALLOCATE(TEI_minima(1:hr_topo_x_size, 1:hr_topo_y_size),TEI_maxima(1:hr_topo_x_size, 1:hr_topo_y_size)) 
    ALLOCATE(TEI_pointers_array(nbr_wdir))
    DO m=1, nbr_wdir
       ALLOCATE(TEI_pointers_array(m)%tei_arr_ptr(1:hr_topo_x_size, 1:hr_topo_y_size))
    END DO

    TEI_minima(:,:) = 0d0
    TEI_maxima(:,:) = 0d0
    DO m=1, nbr_wdir
       TEI_pointers_array(m)%tei_arr_ptr(:,:) = 1d0
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
                   IF (WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist .GT. 0d0) THEN
                                                                                                               !Since each influence gridpoint weight is
                      TEI_pointers_array(m)%tei_arr_ptr(i, j) = TEI_pointers_array(m)%tei_arr_ptr(i, j) + &      !given by 1/horizontal distance between 
                        (hr_surface_elevation_data(i, j, 1) - hr_surface_elevation_data(i + &           !the gridpoint of influence and the cell the TEI's
                        WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative, &                        !is being computed, it is necessary to check
                        j + WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative, 1))* &                 !if horizontal_dist is equal to 0 in order to
                        1d0/WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist            !avoid errors risen by null divisions         
                     
                      TEI_minima(i,j) = TEI_minima(i,j) - elev_max * &
                        1d0/WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist 

                      TEI_maxima(i,j) = TEI_maxima(i,j) + elev_max * &
                        1d0/WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist
                   ELSE                                                                                         
                      TEI_pointers_array(m)%tei_arr_ptr(i, j) = TEI_pointers_array(m)%tei_arr_ptr(i, j)
                   END IF
               END IF
             END DO
          END DO
       END DO
    END DO

    TEI_global_minimum = MINVAL(TEI_minima)
    TEI_global_maximum = MAXVAL(TEI_maxima)

    PRINT*, ""
    PRINT*, "TEST PRECIPITATIONS"
    PRINT*, "TEI_global_min =", TEI_global_minimum
    PRINT*, "TEI_global_max =", TEI_global_maximum
    PRINT*, "TEI_min =", MINVAL(TEI_pointers_array(1)%tei_arr_ptr), MINVAL(TEI_pointers_array(3)%tei_arr_ptr)
    PRINT*, "TEI_max =", MAXVAL(TEI_pointers_array(1)%tei_arr_ptr), MAXVAL(TEI_pointers_array(3)%tei_arr_ptr)

    DO m=1, nbr_wdir
      DO j=1, hr_topo_y_size
        DO i=1, hr_topo_x_size
          TEI_pointers_array(m)%tei_arr_ptr(i, j) = 2*((TEI_pointers_array(m)%tei_arr_ptr(i, j) - TEI_global_minimum)/(TEI_global_maximum - TEI_global_minimum)) - 1d0
        END DO
      END DO 
    END DO
 
    PRINT*, ""
    PRINT*, "TEST PRECIPITATIONS after normalization"
    PRINT*, "TEI_global_min =", TEI_global_minimum
    PRINT*, "TEI_global_max =", TEI_global_maximum
    PRINT*, "TEI_min =", MINVAL(TEI_pointers_array(1)%tei_arr_ptr), MINVAL(TEI_pointers_array(3)%tei_arr_ptr)
    PRINT*, "TEI_max =", MAXVAL(TEI_pointers_array(1)%tei_arr_ptr), MAXVAL(TEI_pointers_array(3)%tei_arr_ptr)

    
    END SUBROUTINE computing_WL_exposure_indexes

  
END MODULE Topographic_parameters_computation
