PROGRAM main
  !________________________________________________________________________________________!
  !Importing modules
  !________________________________________________________________________________________!

  USE Parametrization
  USE Reading_Inputs, only: reading_temperature_inputs, reading_topography_inputs
  USE Writing_Outputs, only: initializing_downscaled_outputs_grid
  USE Temperature, only: Test
  !________________________________________________________________________________________!

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !Reading netCDF input files 
  !________________________________________________________________________________________!
  
  CALL reading_temperature_inputs(lr_surface_temperature_data, config_namelist_blockname, ios, fu)

  PRINT *,"_______________________________"
  PRINT *,lr_surface_temperature_data(1,1,1)

  CALL Test(lr_surface_temperature_data)

  PRINT *,lr_surface_temperature_data(1,1,1)

  PRINT *,"______________________________"
  
  CALL reading_topography_inputs(hr_surface_elevation_data, config_namelist_blockname, ios, fu)

  PRINT *,hr_surface_elevation_data(71,61,1)

  !________________________________________________________________________________________!
  !Writing netCDF output files 
  !________________________________________________________________________________________!

  CALL initializing_downscaled_outputs_grid(ds_x_grid,ds_y_grid,ds_monthly_t_grid,&
       ds_annual_t_grid, config_namelist_blockname, ios, fu)

  !________________________________________________________________________________________!
  !Deallocating all arrays after writing outputs in netCDF files
  !________________________________________________________________________________________!

  DEALLOCATE(lr_surface_temperature_data,hr_surface_elevation_data, ds_x_grid, ds_y_grid)
  
  IF (lr_monthly_climate_data_availibility .EQV. .TRUE.) THEN
     DEALLOCATE(ds_monthly_t_grid)
     IF (ds_annual_data_generation .EQV. .TRUE.) THEN
        DEALLOCATE(ds_annual_t_grid)
     ENDIF
  ELSE
     DEALLOCATE(ds_annual_t_grid)
  ENDIF
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
