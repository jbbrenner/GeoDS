MODULE Temperature_downscaling

  !____________________________________________________________________________!
  
  USE Parametrization
  !____________________________________________________________________________!
 

  IMPLICIT NONE

CONTAINS

  !____________________________________________________________________________!
  SUBROUTINE applying_lapse_rate_correction(lr_surface_temperature_data)
    
    IMPLICIT NONE

    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_surface_temperature_data

    lr_surface_temperature_data(:,:,:) = lapse_rate * lr_surface_temperature_data(:,:,:)

    

  END SUBROUTINE applying_lapse_rate_correction
 
   !____________________________________________________________________________!

  END MODULE Temperature_downscaling
