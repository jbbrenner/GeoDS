MODULE Temperature

  
  !-----------------------------------------------------------------------!
  USE Parametrization
  !-----------------------------------------------------------------------! 

  IMPLICIT NONE

   

  !-----------------------------------------------------------------------! 
CONTAINS

  SUBROUTINE Test(x,T)
    
    IMPLICIT NONE

    REAL,INTENT(IN) :: x
    REAL,INTENT(OUT) :: T

    T=lapse_rate*x

    PRINT*,'Le resultat vaut :',T
    

  END SUBROUTINE Test
 
  !-----------------------------------------------------------------------! 
  END MODULE Temperature
