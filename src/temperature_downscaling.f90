MODULE Temperature_downscaling

  !This module contains the subroutines required to downscale low resolution surface temperature (GCM's outputs)
  !to high resolution topographic grid target. The method uses relative anomalies in topographic parameters (elevation, aspect,..)
  !between the two spatial scales to generate fine-scale temperature, rather than computing high resolution fields using terrain
  !estimators absolute values directly
  !____________________________________________________________________________!
  
  USE Parametrization
  USE Topographic_parameters_computation, only: computing_elevation_anomalies, computing_insolation_anomalies
  !____________________________________________________________________________!

  IMPLICIT NONE

  INTEGER, PRIVATE :: t, i, j, k

  
CONTAINS

  !____________________________________________________________________________!
  SUBROUTINE applying_lapse_rate_correction(locvar__lr_surface_temperature_array, locvar__elevation_anomalies_array, &
       locvar__hr_surface_temperature_array, locvar__hr_lr_surface_temperature_anomalies_array)
    
    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: locvar__lr_surface_temperature_array
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: locvar__elevation_anomalies_array, locvar__hr_surface_temperature_array, &
         locvar__hr_lr_surface_temperature_anomalies_array

    ALLOCATE(locvar__hr_surface_temperature_array(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    ALLOCATE(locvar__hr_lr_surface_temperature_anomalies_array(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    
    locvar__hr_surface_temperature_array(:,:,:) = 0
    locvar__hr_lr_surface_temperature_anomalies_array(:,:,:) = 0

    !for every time step (3rd dimension of the following arrays), the high resolution surface temperature is computed as
    !a correction of the GCM's outputs. A loop is used to overcome the problem of dimensions inequality between climate
    !data arrays and topographic data arrays

!    k=1
!        DO t=1, t_extent
!           locvar__hr_surface_temperature_array(:,:,t) = locvar__lr_surface_temperature_array(:,:,t) + &
!                lambda * locvar__elevation_anomalies_array(:,:,k) + &
!                alpha * topographic_insolation_anomalies_data(:,:,k)
!           k=k+1
!           IF (k .EQ. months_nbr+1) THEN
!              k=1
!           ENDIF
!        ENDDO       

!     locvar__hr_lr_surface_temperature_anomalies_array(:,:,:) = locvar__hr_surface_temperature_array(:,:,:) &
!             - locvar__lr_surface_temperature_array(:,:,:)

    k=1
        DO t=1, t_extent
                DO j=1, hr_topo_y_size
                        DO i=1, hr_topo_x_size
                                locvar__hr_surface_temperature_array(i,j,t) = locvar__lr_surface_temperature_array(i,j,t) + &
                                locvar__elevation_anomalies_array(i,j,k) * (lapse_rate + lambda * &
                                TEI_pointers_array(INT(sorted_wind_directions_data(i,j,t)))%tei_arr_ptr(i, j)) + &
                                alpha * topographic_insolation_anomalies_data(i,j,k)                        
                        k=k+1
                        IF (k .EQ. months_nbr+1) THEN
                                k=1
                        ENDIF
                        END DO
                END DO
        END DO


     locvar__hr_lr_surface_temperature_anomalies_array(:,:,:) = locvar__hr_surface_temperature_array(:,:,:) &
             - locvar__lr_surface_temperature_array(:,:,:)


     !managing missing data
     DO t=1, t_extent
        DO j=1, hr_topo_y_size
           DO i=1, hr_topo_x_size
                IF (lr_precipitation_data(i,j,t) .LT. -100) THEN
                        locvar__hr_surface_temperature_array(i,j,t)=missing_data_error_code
                        locvar__lr_surface_temperature_array(i,j,t)=missing_data_error_code
                        locvar__hr_lr_surface_temperature_anomalies_array(i,j,t)=missing_data_error_code
                END IF
           END DO
        END DO
     END DO



  END SUBROUTINE applying_lapse_rate_correction
 
   !____________________________________________________________________________!

  END MODULE temperature_downscaling
