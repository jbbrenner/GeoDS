PROGRAM main
  !________________________________________________________________________________________!
  !Importing modules
  !________________________________________________________________________________________!

  USE Parametrization
  USE Inputs_reading, ONLY: reading_temperature_inputs, reading_precipitation_inputs, reading_wind_inputs, &
       reading_topography_inputs
  USE Topographic_parameters_computation, ONLY: computing_WL_exposure_indexes
  USE Outputs_writing, ONLY: writing_wdir_gridpoints_patterns,initializing_downscaled_outputs_grid, &
       writing_downscaled_data_outputs
  USE Wind_directions_ordering
  !________________________________________________________________________________________!

  IMPLICIT NONE

  INTEGER :: m
  !________________________________________________________________________________________!
  !Reading netCDF input files 
  !________________________________________________________________________________________!

  PRINT*, ""
  PRINT*, "___________INPUTS______________"

  CALL reading_temperature_inputs(lr_surface_temperature_data, config_namelist_blockname, t_extent, ios, fu)

  CALL reading_precipitation_inputs(lr_precipitation_data, config_namelist_blockname, ios, fu)

  CALL reading_wind_inputs(lr_uwind_data, lr_vwind_data, config_namelist_blockname, ios, fu)

  CALL reading_topography_inputs(lr_surface_elevation_data, hr_surface_elevation_data, lr_topographic_insolation_data, &
       hr_topographic_insolation_data, config_namelist_blockname, ios, fu)

  !________________________________________________________________________________________!
  !TEI tests
  !________________________________________________________________________________________!
  
  PRINT*, "_______________________________"
  PRINT*, "TEI computation"
  CALL computing_WL_exposure_indexes(TEI_pointers_array, config_namelist_blockname, ios, fu)
  PRINT*, "End TEI computation"
  PRINT*, "_______________________________"

  !________________________________________________________________________________________!
  !Writing output files 
  !________________________________________________________________________________________!

  CALL writing_wdir_gridpoints_patterns()

  CALL computing_wind_directions(sorted_wind_directions_data)

  CALL initializing_downscaled_outputs_grid(ds_x_grid,ds_y_grid, tei_wdir_grid, ds_monthly_t_grid,&
       ds_annual_t_grid, config_namelist_blockname, ios, fu)

  CALL writing_downscaled_data_outputs(ds_monthly_temperature_data_file, ds_annual_temperature_data_file,&
       ds_monthly_precipitation_data_file, ds_annual_precipitation_data_file, topographic_exposure_indexes_file, &
       lr_surface_temperature_data, ds_annual_temperature_data, &
       ds_annual_precipitation_data, topographic_exposure_indexes_data, sorted_wind_directions_data)

  PRINT*, hr_topo_x_size, hr_topo_y_size, lr_climate_data_t_size, sum(lr_surface_temperature_data)
  PRINT*, "Low resolution mean temperature :",&
       sum(lr_surface_temperature_data)/(hr_topo_x_size*hr_topo_y_size*lr_climate_data_t_size) - T_conv
  PRINT*, "Low resolution maximum temperature :", (MAXVAL(lr_surface_temperature_data)) - T_conv
  PRINT*, "Low resolution maximum temperature :",(MINVAL(lr_surface_temperature_data)) - T_conv
  PRINT*, "High resolution mean temperature :",&
       (sum(hr_surface_temperature_data))/(hr_topo_x_size*hr_topo_y_size*lr_climate_data_t_size) &
       - T_conv
  PRINT*, "High resolution maximum temperature :",(MAXVAL(hr_surface_temperature_data)) - T_conv
  PRINT*, "High resolution minimum temperature :",(MINVAL(hr_surface_temperature_data)) - T_conv
  PRINT*,"_______________________________"
  !________________________________________________________________________________________!
  !Deallocating all arrays after writing outputs in netCDF files
  !________________________________________________________________________________________!
  DO m=1, nbr_wdir
     DEALLOCATE(WL_pattern_pointers_array(m)%wl_arr_ptr)
     DEALLOCATE(TEI_pointers_array(m)%tei_arr_ptr)
  END DO
        
  DEALLOCATE(WL_pattern_pointers_array)
  DEALLOCATE(TEI_pointers_array)
  DEALLOCATE(wdir_angle_boundaries)
        
  DEALLOCATE(hr_precipitation_data, lr_hr_precipitation_anomalies, &
       lr_hr_surface_temperature_anomalies, &
       lr_surface_temperature_data, hr_surface_temperature_data, lr_uwind_data, lr_vwind_data, lr_surface_elevation_data, &
       hr_surface_elevation_data, elevation_anomalies_data, lr_topographic_insolation_data, hr_topographic_insolation_data, &
       topographic_insolation_anomalies_data, ds_x_grid, ds_y_grid, tei_wdir_grid, topographic_exposure_indexes_data, &
       sorted_wind_directions_data)
  
  IF (lr_monthly_climate_data_availibility .EQV. .TRUE.) THEN
     DEALLOCATE(ds_monthly_t_grid)
     IF (ds_annual_data_generation .EQV. .TRUE.) THEN
        DEALLOCATE(ds_annual_t_grid, ds_annual_temperature_data)
     ENDIF
  ELSE
     DEALLOCATE(ds_annual_t_grid, ds_annual_temperature_data)
  ENDIF
  !________________________________________________________________________________________!
 
  WRITE(*,*)"Closing program"
  !________________________________________________________________________________________!

END PROGRAM main
