PROGRAM main
  !________________________________________________________________________________________!
  !Importing modules
  !________________________________________________________________________________________!

  USE Parametrization
  USE Reading_Inputs, only: reading_temperature_inputs, reading_topography_inputs
  USE Writing_Outputs, only: initializing_downscaled_outputs_grid, writing_downscaled_data_outputs
  !________________________________________________________________________________________!

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !Reading netCDF input files 
  !________________________________________________________________________________________!
  
  CALL reading_temperature_inputs(lr_surface_temperature_data, config_namelist_blockname, ios, fu)

  PRINT *,"_______________________________"
  PRINT *,lr_surface_temperature_data(1,1,1)
  PRINT *,"______________________________"
  
  CALL reading_topography_inputs(lr_surface_elevation_data, hr_surface_elevation_data, &
       config_namelist_blockname, ios, fu)

  PRINT *,hr_surface_elevation_data(71,61,1)
  !________________________________________________________________________________________!
  !Writing netCDF output files 
  !________________________________________________________________________________________!

  CALL initializing_downscaled_outputs_grid(ds_x_grid,ds_y_grid,ds_monthly_t_grid,&
       ds_annual_t_grid, config_namelist_blockname, ios, fu)

  CALL writing_downscaled_data_outputs(ds_monthly_climate_data_file, ds_annual_climate_data_file,&
       lr_surface_temperature_data, ds_monthly_climate_data, ds_annual_climate_data)

  PRINT*, "lr_temperature", (sum(lr_surface_temperature_data))/(hr_topo_x_size*hr_topo_y_size*lr_climate_data_t_size)
  PRINT*, "max_lr_temp", (MAXVAL(lr_surface_temperature_data))
  PRINT*, "hr_temperature", (sum(hr_surface_temperature_data))
  PRINT*, "max_hr_temperature",(MAXVAL(hr_surface_temperature_data))
  PRINT*, "hr_temperature", (sum(hr_surface_temperature_data))/(hr_topo_x_size*hr_topo_y_size*lr_climate_data_t_size)
  PRINT*, "mois : ", sum(ds_monthly_climate_data)
  PRINT*,"annees : ", sum(ds_annual_climate_data)*12

  !________________________________________________________________________________________!
  !Deallocating all arrays after writing outputs in netCDF files
  !________________________________________________________________________________________!

  DEALLOCATE(lr_surface_temperature_data, lr_surface_elevation_data, hr_surface_elevation_data, &
       elevation_anomalies_data, hr_surface_temperature_data, ds_x_grid, ds_y_grid)
  
  IF (lr_monthly_climate_data_availibility .EQV. .TRUE.) THEN
     DEALLOCATE(ds_monthly_t_grid, ds_monthly_climate_data)
     IF (ds_annual_data_generation .EQV. .TRUE.) THEN
        DEALLOCATE(ds_annual_t_grid, ds_annual_climate_data)
     ENDIF
  ELSE
     DEALLOCATE(ds_annual_t_grid, ds_annual_climate_data)
  ENDIF
  !________________________________________________________________________________________!
  
  WRITE(*,*)"Closing program"
  !________________________________________________________________________________________!

END PROGRAM main
