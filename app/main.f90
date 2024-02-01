PROGRAM main
  !________________________________________________________________________________________!
  !Importing modules
  !________________________________________________________________________________________!

  USE Parametrization
  USE Reading_Inputs
  USE Writing_Outputs
  USE Temperature
  USE ncio
  !________________________________________________________________________________________!

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !Declaring input variables
  !________________________________________________________________________________________!  

  !Temperature-related input variables
  CHARACTER (LEN=256) :: LR_temperature_file
  CHARACTER (LEN=20) :: LR_surface_temperature_id
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: LR_surface_temperature_data
  INTEGER :: lrtemp_x_size
  INTEGER :: lrtemp_y_size
  INTEGER :: lrtemp_t_size


  !Topography-related input variables
  CHARACTER (LEN=256) :: HR_elevation_file
  CHARACTER (LEN=20) :: HR_surface_elevation_id
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: HR_surface_elevation_data
  INTEGER :: hrtopo_x_size
  INTEGER :: hrtopo_y_size
  INTEGER :: hrtopo_t_size
  
  !________________________________________________________________________________________!
  !Declaring output variables
  !________________________________________________________________________________________!  

  !Temperature-related output variables
  CHARACTER (LEN=256) :: ds_temperature_file
 
  !________________________________________________________________________________________!
  !Reading netCDF input files 
  !________________________________________________________________________________________!
  
  CALL inputs_temperature(LR_temperature_file, LR_surface_temperature_id,&
       LR_surface_temperature_data,lrtemp_x_size,&
       lrtemp_y_size,lrtemp_t_size)

  PRINT *,"_______________________________"
  PRINT *,LR_surface_temperature_data(1,1,1)

  CALL Test(LR_surface_temperature_data)

  PRINT *,LR_surface_temperature_data(1,1,1)

  PRINT *,"______________________________"
  
  CALL inputs_topography(HR_elevation_file, HR_surface_elevation_id,&
       HR_surface_elevation_data,hrtopo_x_size,hrtopo_y_size,hrtopo_t_size)

  PRINT *,HR_surface_elevation_data(71,61,1)

  !________________________________________________________________________________________!
  !Writing netCDF output files 
  !________________________________________________________________________________________!

  CALL downscaled_outputs_writing(ds_temperature_file)
  
  !________________________________________________________________________________________!
  !Deallocating all arrays after writing outputs in netCDF files
  !________________________________________________________________________________________!
  
  DEALLOCATE(LR_surface_temperature_data,HR_surface_elevation_data)

  !________________________________________________________________________________________!
  
  WRITE(*,*)"Closing program"
  !________________________________________________________________________________________!

END PROGRAM main







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
