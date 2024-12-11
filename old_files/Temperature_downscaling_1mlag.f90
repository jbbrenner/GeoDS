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

  INTEGER, PRIVATE :: i, j, k

  
CONTAINS

  !____________________________________________________________________________!
  SUBROUTINE applying_lapse_rate_correction(lr_surface_temperature_data, elevation_anomalies_data, &
       hr_surface_temperature_data, hr_lr_surface_temperature_difference)
    
    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_surface_temperature_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: elevation_anomalies_data, hr_surface_temperature_data, &
         hr_lr_surface_temperature_difference

    !Computing elevation anomalies between low resolution grid and high resolution grid
    CALL computing_elevation_anomalies(lr_surface_elevation_data, &
         hr_surface_elevation_data, elevation_anomalies_data)
    CALL computing_insolation_anomalies(lr_topographic_insolation_data, hr_topographic_insolation_data, &
       topographic_insolation_anomalies_data)

    !PRINT*,'______________________insol_test_____________________'    
    !PRINT*, topographic_insolation_anomalies_data(1,1,1)
    !PRINT*, topographic_insolation_anomalies_data(1,1,7)
    !PRINT*,'______________________insol_test_____________________'
    ALLOCATE(hr_surface_temperature_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:lr_climate_data_t_size))
    ALLOCATE(hr_lr_surface_temperature_difference(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:lr_climate_data_t_size))
    
    hr_surface_temperature_data(:,:,:) = 0
    hr_lr_surface_temperature_difference(:,:,:) = 0

    !for every time step (3rd dimension of the following arrays), the high resolution surface temperature is computed as
    !a correction of the GCM's outputs. A loop is used to overcome the problem of dimensions inequality between climate
    !data arrays and topographic data arrays

    j=12
    k=1
        DO i=1, lr_climate_data_t_size
           hr_surface_temperature_data(:,:,i) = lr_surface_temperature_data(:,:,i) + &
                lambda * elevation_anomalies_data(:,:,j) + &
                alpha * topographic_insolation_anomalies_data(:,:,j)
           j=k
           k=k+1
           IF (k .EQ. months_nbr+1) THEN
              k=1
           ENDIF
        ENDDO       

     hr_lr_surface_temperature_difference(:,:,:) = hr_surface_temperature_data(:,:,:) &
             - lr_surface_temperature_data(:,:,:)

  END SUBROUTINE applying_lapse_rate_correction
 
   !____________________________________________________________________________!

  END MODULE Temperature_downscaling
