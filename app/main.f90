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

  !Temperature related variables
  CHARACTER (LEN=100) :: LR_temperature_file
  CHARACTER (LEN=20) :: LR_surface_temperature_id
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: LR_surface_temperature_data
  INTEGER :: lrtemp_x_size
  INTEGER :: lrtemp_y_size
  INTEGER :: lrtemp_t_size


  !Topography related variables
  CHARACTER (LEN=100) :: HR_elevation_file
  CHARACTER (LEN=20) :: HR_surface_elevation_id
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: HR_surface_elevation_data
  INTEGER :: hrtopo_x_size
  INTEGER :: hrtopo_y_size
  INTEGER :: hrtopo_t_size


  
  REAL :: T
  CHARACTER(LEN=10) :: interpol

  !________________________________________________________________________________________!
  ! Calling external functions and subroutines
  !________________________________________________________________________________________!
  CALL Test(3.0,T)
  
  CALL inputs_temperature(LR_temperature_file, LR_surface_temperature_id,&
       LR_surface_temperature_data,lrtemp_x_size,&
       lrtemp_y_size,lrtemp_t_size)

  PRINT *,LR_surface_temperature_data(1,1,1)
  PRINT*,"____________________________"

  CALL inputs_topography(HR_elevation_file, HR_surface_elevation_id,&
       HR_surface_elevation_data,hrtopo_x_size,hrtopo_y_size,hrtopo_t_size)

  PRINT *,HR_surface_elevation_data(71,61,1)

  
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
