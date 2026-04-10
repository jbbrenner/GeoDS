MODULE Precipitation_downscaling

  !____________________________________________________!

  USE PARAMETRIZATION
  !____________________________________________________!

  IMPLICIT NONE

  INTEGER, PRIVATE :: i, j, t

CONTAINS

  !____________________________________________________!
  SUBROUTINE downscaling_precipitation(locvar__lr_precipitation_array, locvar__hr_precipitation_array, &
       locvar__hr_lr_precipitation_ratio_array, locvar__hr_lr_precipitation_anomalies_array)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: locvar__lr_precipitation_array
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: locvar__hr_precipitation_array, &
        locvar__hr_lr_precipitation_ratio_array, locvar__hr_lr_precipitation_anomalies_array

    ALLOCATE(locvar__hr_precipitation_array(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    ALLOCATE(locvar__hr_lr_precipitation_ratio_array(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    ALLOCATE(locvar__hr_lr_precipitation_anomalies_array(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
   
    locvar__hr_precipitation_array(:,:,:) = 0.d0
    locvar__hr_lr_precipitation_ratio_array(:,:,:) = 0.d0
    locvar__hr_lr_precipitation_anomalies_array(:,:,:) = 0.d0

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
                locvar__hr_precipitation_array(i,j,t) = locvar__lr_precipitation_array(i,j,t) * & 
                EXP(beta * TEI_pointers_array(INT(sorted_wind_directions_data(i,j,t)))%tei_arr_ptr(i, j))
             ELSE
                locvar__hr_precipitation_array(i,j,t) = locvar__lr_precipitation_array(i,j,t) * max_precipitation_increase_factor
             END IF
             
             IF (locvar__lr_precipitation_array(i,j,t) .NE. 0.d0) THEN
                locvar__hr_lr_precipitation_ratio_array(i,j,t) = locvar__hr_precipitation_array(i,j,t)/locvar__lr_precipitation_array(i,j,t)
             END IF      
          END DO
       END DO
    END DO
    
   locvar__hr_lr_precipitation_anomalies_array(:,:,:) = locvar__hr_precipitation_array(:,:,:) - locvar__lr_precipitation_array(:,:,:)

   
   !Loop to manage missing values
   DO t=1, t_extent
       DO j=1, hr_topo_y_size
          DO i=1, hr_topo_x_size
                IF (locvar__lr_precipitation_array(i,j,t) .LT. -100) THEN 
                        locvar__hr_precipitation_array(i,j,t)=missing_data_error_code
                        locvar__lr_precipitation_array(i,j,t)=missing_data_error_code
                        locvar__hr_lr_precipitation_anomalies_array(i,j,t)=missing_data_error_code
                        locvar__hr_lr_precipitation_ratio_array(i,j,t)=missing_data_error_code
                END IF
          END DO
       END DO
    END DO

    END SUBROUTINE downscaling_precipitation
  
  END MODULE Precipitation_downscaling
      


