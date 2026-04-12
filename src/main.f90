PROGRAM main
  !________________________________________________________________________________________!
  !Importing modules
  !________________________________________________________________________________________!

  USE Parametrization
  USE Support_functions
  USE Inputs_reading, ONLY: reading_temperature_inputs, reading_precipitation_inputs, reading_wind_inputs, &
       reading_topography_inputs
  USE wl_gridpoints_selection
  USE topographic_parameters_computation
  USE Wind_directions_ordering
  USE temperature_downscaling
  USE precipitation_downscaling
  USE Outputs_writing, ONLY: writing_wdir_gridpoints_patterns,initializing_downscaled_outputs_grid, &
       writing_downscaled_data_outputs
  !________________________________________________________________________________________!

  IMPLICIT NONE

  INTEGER :: p

  PRINT*, "____________________________________________"
  PRINT*, "Reading Inputs"

  CALL reading_temperature_inputs(lr_surface_temperature_data, config_namelist_blockname, t_extent, ios, fu)

  CALL reading_precipitation_inputs(lr_precipitation_data, config_namelist_blockname, ios, fu)

  CALL reading_wind_inputs(lr_uwind_data, lr_vwind_data, config_namelist_blockname, ios, fu)

  CALL reading_topography_inputs(lr_surface_elevation_data, hr_surface_elevation_data, lr_topographic_insolation_data, &
       hr_topographic_insolation_data, config_namelist_blockname, ios, fu)

  PRINT*, "____________________________________________"
  PRINT*, "Computing topography-based parameters"
  PRINT*, ""
  CALL filling_WL_patterns_arrays(WL_pattern_pointers_array,wdir_angle_boundaries)
 
  CALL computing_elevation_anomalies(lr_surface_elevation_data, hr_surface_elevation_data, elevation_anomalies_data)
  
  CALL computing_insolation_anomalies(lr_topographic_insolation_data, hr_topographic_insolation_data, &
       topographic_insolation_anomalies_data)
  
  CALL computing_WL_exposure_indexes(TEI_pointers_array, TEI_drying_effect_correction)
  PRINT*, "TEST JB", TEI_drying_effect_correction(1)%tei_arr_ptr(100,100)
  CALL writing_wdir_gridpoints_patterns()

  CALL computing_wind_directions(sorted_wind_directions_data)

  PRINT*, "____________________________________________"
  PRINT*, "Downscaling temperature"
  
  CALL applying_lapse_rate_correction(lr_surface_temperature_data,elevation_anomalies_data, &
       hr_surface_temperature_data, hr_lr_surface_temperature_anomalies)

  PRINT*, "____________________________________________"
  PRINT*, "Downscaling precipitation"
  
  CALL downscaling_precipitation(lr_precipitation_data, hr_precipitation_data, &
       hr_lr_precipitation_ratio, hr_lr_precipitation_anomalies)

  PRINT*, "____________________________________________"
  PRINT*, "Writing Outputs"
   
  CALL initializing_downscaled_outputs_grid(ds_x_grid,ds_y_grid, tei_wdir_grid, ds_monthly_t_grid,&
       ds_annual_t_grid, config_namelist_blockname, ios, fu)

  CALL writing_downscaled_data_outputs(ds_monthly_temperature_data_file, ds_annual_temperature_data_file,&
       ds_monthly_precipitation_data_file, ds_annual_precipitation_data_file, topographic_exposure_indexes_file, &
       ds_annual_temperature_data, ds_annual_precipitation_data, topographic_exposure_indexes_data, &
       sorted_wind_directions_data)

  !________________________________________________________________________________________!
  !Deallocating all arrays after writing outputs in netCDF files
  !________________________________________________________________________________________!
 
 DO p=1, nbr_wdir
     DEALLOCATE(WL_pattern_pointers_array(p)%wl_arr_ptr)
     DEALLOCATE(TEI_pointers_array(p)%tei_arr_ptr)
  END DO
        
  DEALLOCATE(WL_pattern_pointers_array)
  DEALLOCATE(TEI_pointers_array)
  IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
     DEALLOCATE(TEI_drying_effect_correction)
  END IF
  DEALLOCATE(wdir_angle_boundaries)
        
  DEALLOCATE(hr_precipitation_data, hr_lr_precipitation_ratio, hr_lr_precipitation_anomalies, &
       hr_lr_surface_temperature_anomalies, &
       lr_surface_temperature_data, hr_surface_temperature_data, lr_uwind_data, lr_vwind_data, lr_surface_elevation_data, &
       hr_surface_elevation_data, elevation_anomalies_data, lr_topographic_insolation_data, hr_topographic_insolation_data, &
       topographic_insolation_anomalies_data, ds_x_grid, ds_y_grid, tei_wdir_grid, topographic_exposure_indexes_data, &
       sorted_wind_directions_data)
  
  IF (lr_monthly_climate_data_availability .EQV. .TRUE.) THEN
     DEALLOCATE(ds_monthly_t_grid)
     IF (ds_annual_data_generation .EQV. .TRUE.) THEN
        DEALLOCATE(ds_annual_t_grid, ds_annual_temperature_data)
     ENDIF
  ELSE
     DEALLOCATE(ds_annual_t_grid, ds_annual_temperature_data)
  ENDIF
  !________________________________________________________________________________________!
  PRINT*, "____________________________________________"
  WRITE(*,*)"Closing program"
  !________________________________________________________________________________________!

END PROGRAM main
