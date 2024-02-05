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
  !Reading netCDF input files 
  !________________________________________________________________________________________!
  
  CALL inputs_temperature(LR_surface_temperature_data, config_namelist_blockname,ios, fu)

  PRINT *,"_______________________________"
  PRINT *,LR_surface_temperature_data(1,1,1)

  CALL Test(LR_surface_temperature_data)

  PRINT *,LR_surface_temperature_data(1,1,1)

  PRINT *,"______________________________"
  
  CALL inputs_topography(HR_surface_elevation_data, config_namelist_blockname, ios, fu)

  PRINT *,HR_surface_elevation_data(71,61,1)

  !________________________________________________________________________________________!
  !Writing netCDF output files 
  !________________________________________________________________________________________!

  CALL downscaled_outputs_grid_init(x_ds_grid,y_ds_grid, config_namelist_blockname, ios, fu)

  !________________________________________________________________________________________!
  !Closing configuration file
  !________________________________________________________________________________________!
  !CLOSE (fu)
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
