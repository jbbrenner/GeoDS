MODULE Precipitation_downscaling

  !____________________________________________________!

  USE PARAMETRIZATION
  USE Topographic_parameters_computation, ONLY: computing_WL_exposure_indexes
  !____________________________________________________!

  IMPLICIT NONE

  INTEGER, PRIVATE :: i, j

CONTAINS

  !____________________________________________________!
  SUBROUTINE downscaling_precipitation(lr_precipitation_data, hr_precipitation_data, &
       lr_hr_precipitation_data)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_precipitation_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: hr_precipitation_data, &
         lr_hr_precipitation_data

    PRINT*, 'Downscaling des précipitations'

    ALLOCATE(hr_precipitation_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    ALLOCATE(lr_hr_precipitation_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    

    hr_precipitation_data(:,:,:) = 0
    lr_hr_precipitation_data(:,:,:) = 0

    PRINT*, "_______________________________"    
    END SUBROUTINE downscaling_precipitation
  
  END MODULE Precipitation_downscaling
      


