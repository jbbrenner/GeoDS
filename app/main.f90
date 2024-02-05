PROGRAM main
  !________________________________________________________________________________________!
  !Importing modules
  !________________________________________________________________________________________!

  USE Parametrization
  USE Support_functions
  USE Reading_Inputs
  USE Writing_Outputs
  USE Temperature
  USE ncio
  !________________________________________________________________________________________!

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !Declaring support variables
  !________________________________________________________________________________________!

  CHARACTER(LEN=100) :: config_namelist_blockname
  INTEGER :: ios, fu
  
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

  CHARACTER (LEN=256) :: downscaled_climate_data_file
  DOUBLE PRECISION, ALLOCATABLE :: x_ds_grid(:), y_ds_grid(:)
  INTEGER :: x_max, y_max
 
  !________________________________________________________________________________________!
  !Reading netCDF input files 
  !________________________________________________________________________________________!
  
  CALL inputs_temperature(LR_surface_temperature_data)

  PRINT *,"_______________________________"
  PRINT *,LR_surface_temperature_data(1,1,1)

  CALL Test(LR_surface_temperature_data)

  PRINT *,LR_surface_temperature_data(1,1,1)

  PRINT *,"______________________________"
  
  CALL inputs_topography(HR_surface_elevation_data)

  PRINT *,HR_surface_elevation_data(71,61,1)

  !________________________________________________________________________________________!
  !Writing netCDF output files 
  !________________________________________________________________________________________!

  CALL downscaled_outputs_grid_init(x_ds_grid,y_ds_grid)

  !________________________________________________________________________________________!
  !Closing configuration file
  !________________________________________________________________________________________!
  CLOSE (fu)
  !________________________________________________________________________________________!
  !Deallocating all arrays after writing outputs in netCDF files
  !________________________________________________________________________________________!

  DEALLOCATE(LR_surface_temperature_data,HR_surface_elevation_data,x_ds_grid,y_ds_grid)

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
