MODULE Precipitation_downscaling

  !____________________________________________________!

  USE PARAMETRIZATION
  !____________________________________________________!

  IMPLICIT NONE

  INTEGER, PRIVATE :: i, j, t

CONTAINS

  !____________________________________________________!
  SUBROUTINE downscaling_precipitation(lr_precipitation_data, hr_precipitation_data, &
       hr_lr_precipitation_ratio, hr_lr_precipitation_anomalies)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_precipitation_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: hr_precipitation_data, &
        hr_lr_precipitation_ratio, hr_lr_precipitation_anomalies

    PRINT*, 'Downscaling of precipitation fields'

    ALLOCATE(hr_precipitation_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    ALLOCATE(hr_lr_precipitation_ratio(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    ALLOCATE(hr_lr_precipitation_anomalies(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
   
    hr_precipitation_data(:,:,:) = 0.d0
    hr_lr_precipitation_ratio(:,:,:) = 0.d0
    hr_lr_precipitation_anomalies(:,:,:) = 0.d0

    !The precipitation at high resolution for a given gridpoint are calculated by correcting the low
    !resolution precipitation data with the TEI of the corresponding gridpoint
    !and the dominant wind direction of the month, using the following formula : 
    !P(hr) = P(lr) * exp(beta*TEI). A max_precipitation_increase_factor can be
    !used to limit the ratio P(hr)/P(lr) when TEI increases too much, but can be
    !desactivated using a very large number (e.g. 10000)
    DO t=1, t_extent
       DO j=1, hr_topo_y_size
          DO i=1, hr_topo_x_size
             IF (EXP((beta * TEI_pointers_array(INT(sorted_wind_directions_data(i,j,t)))%tei_arr_ptr(i, j))) &
             .LE. (max_precipitation_increase_factor)) THEN
                hr_precipitation_data(i,j,t) = lr_precipitation_data(i,j,t) * & 
                EXP(beta * TEI_pointers_array(INT(sorted_wind_directions_data(i,j,t)))%tei_arr_ptr(i, j))
             ELSE
                hr_precipitation_data(i,j,t) = lr_precipitation_data(i,j,t) * max_precipitation_increase_factor
             END IF
             
             IF (lr_precipitation_data(i,j,t) .NE. 0.d0) THEN
                hr_lr_precipitation_ratio(i,j,t) = hr_precipitation_data(i,j,t)/lr_precipitation_data(i,j,t)
             END IF      
          END DO
       END DO
    END DO
    
   hr_lr_precipitation_anomalies(:,:,:) = hr_precipitation_data(:,:,:) - lr_precipitation_data(:,:,:)

    PRINT*, "_______________________________"    
    END SUBROUTINE downscaling_precipitation
  
  END MODULE Precipitation_downscaling
      


