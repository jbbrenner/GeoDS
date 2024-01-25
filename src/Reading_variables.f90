MODULE Reading_Variables
  
  !-----------------------------------------------------------------------!
  USE ncio, ONLY: nc_read
  !-----------------------------------------------------------------------! 
  IMPLICIT NONE

  !-----------------------------------------------------------------------!
 
  !-----------------------------------------------------------------------!

CONTAINS


  SUBROUTINE Lecture(n,elev)

    IMPLICIT NONE

    REAL, DIMENSION(256,156)  :: elev
    INTEGER,INTENT(OUT) :: n

    CALL nc_read("/home/jbrenner/These/Cartes/S.nc","orog",eleva)
    
    n=SIZE(elev)
    PRINT*,"le tableau a une taille de",n


  END SUBROUTINE Lecture




END MODULE Reading_Variables
