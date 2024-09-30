MODULE Precipitation_downscaling

  !____________________________________________________!

  USE PARAMETRIZATION
  USE Topographic_parameters_computation, ONLY: computing_WL_exposure_indexes
  !____________________________________________________!

  IMPLICIT NONE

  INTEGER, PRIVATE :: i, j, t, m

CONTAINS

  !____________________________________________________!
  SUBROUTINE downscaling_precipitation(lr_precipitation_data, hr_precipitation_data, &
       lr_hr_precipitation_anomalies)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_precipitation_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: hr_precipitation_data, &
         lr_hr_precipitation_anomalies

 PRINT*, 'Downscaling of precipitation fields'

    ALLOCATE(hr_precipitation_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    ALLOCATE(lr_hr_precipitation_anomalies(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    
    hr_precipitation_data(:,:,:) = 0
    lr_hr_precipitation_anomalies(:,:,:) = 0

    DO t=1, t_extent
       DO j=1, hr_topo_y_size
          DO i=1, hr_topo_x_size
             hr_precipitation_data(i,j,t) = lr_precipitation_data(i,j,t) * & 
             TEI_pointers_array(sorted_wind_directions_data(i,j,t))%tei_arr_ptr(i, j)
          END DO
       END DO
    END DO
    
   lr_hr_precipitation_anomalies(:,:,:) = lr_precipitation_data(:,:,:) - hr_precipitation_data(:,:,:)

    PRINT*, "_______________________________"    
    END SUBROUTINE downscaling_precipitation
  
  END MODULE Precipitation_downscaling
      


