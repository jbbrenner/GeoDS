PROGRAM main
  !----------------------------------------------------------!
  !Import modules
  !----------------------------------------------------------!
  USE Parametrization
  USE Temperature
  USE ncio, ONLY: nc_read
  !----------------------------------------------------------!
  !
  !----------------------------------------------------------!
  IMPLICIT NONE
  !----------------------------------------------------------!
  !Declare variables
  !----------------------------------------------------------!
  REAL :: T
  CHARACTER(LEN=10) :: interpol
  

  !----------------------------------------------------------!
  ! Call external functions and subroutines
  !----------------------------------------------------------!
  CALL Test(3.0,T)


  !----------------------------------------------------------!
  ! Apply interpolation using CDO
  ! Bash script located in src
  !----------------------------------------------------------!
  WRITE(*,*)"Do you want to call the Interpolation module ? yes or no"
  READ(*,*)interpol
  IF (interpol=="yes") THEN
     CALL SYSTEM("pwd && bash src/Interpolation.sh && cd -")
  ELSE
     write(*,*)"Interpolation skipped"
  ENDIF
  !----------------------------------------------------------!

  
  WRITE(*,*)"end of the program"

END PROGRAM main
