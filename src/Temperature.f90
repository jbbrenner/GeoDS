MODULE Temperature

  
  !-----------------------------------------------------------------------!
  USE Parametrization
  !-----------------------------------------------------------------------! 

  IMPLICIT NONE

   

  !-----------------------------------------------------------------------! 
CONTAINS

  SUBROUTINE Test(LR_surface_temperature_data)
    
    IMPLICIT NONE

    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: LR_surface_temperature_data

    LR_surface_temperature_data(:,:,:)=lapse_rate*LR_surface_temperature_data(:,:,:)

    

  END SUBROUTINE Test
 
  !-----------------------------------------------------------------------! 
  END MODULE Temperature
