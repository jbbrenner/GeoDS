PROGRAM main
  !----------------------------------------------------------!
  !Import modules
  !----------------------------------------------------------!
  USE Parametrization
  USE Temperature
  USE ncio, ONLY: nc_read
  !USE Reading_Variables
  !----------------------------------------------------------!
  !
  !----------------------------------------------------------!
  IMPLICIT NONE
  !----------------------------------------------------------!
  !Declare variables
  !----------------------------------------------------------!
  REAL :: T
  CHARACTER(LEN=10) :: interpol

  REAL, DIMENSION(256,156)  :: eleva
  INTEGER :: n
  

  !----------------------------------------------------------!
  ! Call external functions and subroutines
  !----------------------------------------------------------!
  CALL Test(3.0,T)
  CALL nc_read("/home/jbrenner/These/Cartes/S.nc","orog",eleva)

  !CALL Lecture(n,elev)


  !----------------------------------------------------------!
  ! Apply interpolation using CDO
  ! Bash script located in src
  !----------------------------------------------------------!
  !WRITE(*,*)"Do you want to call the Interpolation module ? yes or no"
  !READ(*,*)interpol
  !IF (interpol=="yes") THEN
     !CALL SYSTEM("pwd && bash src/Interpolation.sh && cd -")
 ! ELSE
     !WRITE(*,*)"Interpolation skipped"
 ! ENDIF
  !----------------------------------------------------------!

  
  WRITE(*,*)"end of the program"

END PROGRAM main
