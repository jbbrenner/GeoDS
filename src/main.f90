PROGRAM main
  !________________________________________________________________________________________!
  !Importing modules
  !________________________________________________________________________________________!

  USE Parametrization
  USE Inputs_reading, ONLY: reading_temperature_inputs, reading_topography_inputs
  USE Outputs_writing, ONLY: initializing_downscaled_outputs_grid, writing_downscaled_data_outputs
  !________________________________________________________________________________________!

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !Reading netCDF input files 
  !________________________________________________________________________________________!

  CALL reading_temperature_inputs(lr_surface_temperature_data, config_namelist_blockname, t_extent, ios, fu)

  PRINT *,"_______________________________"
  !CALL reading_precipitation_inputs(lr_precipitation_data, config_namelist_blockname, ios, fu)

  PRINT *,"_______________________________"

  CALL reading_topography_inputs(lr_surface_elevation_data, hr_surface_elevation_data, lr_topographic_insolation_data, &
       hr_topographic_insolation_data, config_namelist_blockname, ios, fu)

  !________________________________________________________________________________________!
  !Writing netCDF output files 
  !________________________________________________________________________________________!

  CALL initializing_downscaled_outputs_grid(ds_x_grid,ds_y_grid,ds_monthly_t_grid,&
       ds_annual_t_grid, config_namelist_blockname, ios, fu)

  CALL writing_downscaled_data_outputs(ds_monthly_climate_data_file, ds_annual_climate_data_file,&
       lr_surface_temperature_data, ds_monthly_climate_data, ds_annual_climate_data)

  PRINT*, "Low resolution mean temperature :",&
       (sum(lr_surface_temperature_data))/(hr_topo_x_size*hr_topo_y_size*lr_climate_data_t_size)&
       - T_conv
  PRINT*, "Low resolution maximum temperature :", (MAXVAL(lr_surface_temperature_data)) - T_conv
  PRINT*, "Low resolution maximum temperature :",(MINVAL(lr_surface_temperature_data)) - T_conv
  PRINT*, "High resolution mean temperature :",&
       (sum(hr_surface_temperature_data))/(hr_topo_x_size*hr_topo_y_size*lr_climate_data_t_size)&
       - T_conv
  PRINT*, "High resolution maximum temperature :",(MAXVAL(hr_surface_temperature_data)) - T_conv
  PRINT*, "High resolution minimum temperature :",(MINVAL(hr_surface_temperature_data)) - T_conv
  !PRINT*, MAXLOC(hr_surface_temperature_data)
  !PRINT*, "July HR max elevdif temperature :", hr_surface_temperature_data(586,60,7)
  !PRINT*, "July LR max elevdif temperature :", lr_surface_temperature_data(586,60,7)
  PRINT*,"_______________________________"
  !________________________________________________________________________________________!
  !Deallocating all arrays after writing outputs in netCDF files
  !________________________________________________________________________________________!

  DEALLOCATE(lr_hr_surface_temperature_difference, &
       lr_surface_temperature_data, hr_surface_temperature_data, lr_surface_elevation_data, &
       hr_surface_elevation_data, elevation_anomalies_data, lr_topographic_insolation_data, hr_topographic_insolation_data, &
       topographic_insolation_anomalies_data, ds_x_grid, ds_y_grid)
  
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
