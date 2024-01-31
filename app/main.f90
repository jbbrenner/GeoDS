PROGRAM main
  !________________________________________________________________________________________!
  !Importing modules
  !________________________________________________________________________________________!

  USE Parametrization
  USE Reading_Variables
  USE Temperature
  USE ncio, ONLY: nc_read
  !________________________________________________________________________________________!

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !Declaring variables
  !________________________________________________________________________________________!  

  CHARACTER (LEN=100) :: LR_temperature_dataset_file
  CHARACTER (LEN=20) :: LR_surface_temperature_id
  REAL, DIMENSION(64,32,1200) :: LR_surface_temperature_data
  CHARACTER (LEN=100) :: HR_elevation_file
  CHARACTER (LEN=20) :: HR_surface_elevation_id
  REAL, DIMENSION(71,61) :: HR_surface_elevation_data


  
  REAL :: T
  CHARACTER(LEN=10) :: interpol

  !________________________________________________________________________________________!
  ! Calling external functions and subroutines
  !________________________________________________________________________________________!
  CALL Test(3.0,T)
  
  CALL inputs_temperature(LR_temperature_dataset_file, LR_surface_temperature_id, LR_surface_temperature_data)

    PRINT *,LR_surface_temperature_data(1,1,1)

  CALL inputs_topography(HR_elevation_file, HR_surface_elevation_id, HR_surface_elevation_data)

    PRINT *,HR_surface_elevation_data(1,1)


  
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

  !________________________________________________________________________________________!

END PROGRAM main
