MODULE Inputs_reading
  
  !________________________________________________________________________________________!

  USE Parametrization
  USE ncio, ONLY: nc_read
  USE Support_functions, only: accessing_config_file
  
  IMPLICIT NONE
 
CONTAINS

  !____________________________________________________________________________________________________________________________!
  !Subroutines required to read netCDF datasets and store related data in arrays using the ncio library. All the subroutines
  !open the same configuration file containing paths and other parametrization variables (namelist), but each one is associated
  !with a different category of data (i.e : data related to temperature, precipitation, topography...)
  !____________________________________________________________________________________________________________________________!
  
  SUBROUTINE reading_temperature_inputs(lr_surface_temperature_data, config_namelist_blockname, t_extent, ios, fu)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_surface_temperature_data
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: t_extent, ios, fu

    !_____________________________________________________________________________________!
    !Reading temperature-related input variables in the configuration file
    !_____________________________________________________________________________________!
    config_namelist_blockname="Global_parametrization"
    CALL accessing_config_file(ios, fu)
    
    t_extent = t_end - t_start + 1
    PRINT*, "t_extent = ", t_extent
    config_namelist_blockname="Inputs_climate_variables"
    CALL accessing_config_file(ios, fu)
    !Sizing data array with dimensions stored in the configuration file
    ALLOCATE (lr_surface_temperature_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
   
    !Storing temperature data from LR netCDF file in array
    CALL nc_read(lr_climate_data_file, lr_surface_temperature_id, lr_surface_temperature_data, &
            [1,1,t_start], [lr_climate_data_x_size, lr_climate_data_y_size, t_extent])
 
  END SUBROUTINE reading_temperature_inputs
  
  !___________________________________________________________________________________________________________________________!
  !___________________________________________________________________________________________________________________________!
  
  SUBROUTINE reading_precipitation_inputs(lr_precipitation_data, config_namelist_blockname, ios, fu)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_precipitation_data
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu

    !_____________________________________________________________________________________!
    !Reading temperature-related input variables in the configuration file
    !_____________________________________________________________________________________!
    config_namelist_blockname="Global_inputs_variables"
    CALL accessing_config_file(ios, fu)

    !Sizing data array with dimensions stored in the configuration file
    ALLOCATE (lr_precipitation_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))

    config_namelist_blockname="Precipitation"
    CALL accessing_config_file(ios, fu)

    !Storing temperature data from LR netCDF file in array
    CALL nc_read(lr_climate_data_file, lr_precipitation_id, lr_precipitation_data)


  END SUBROUTINE reading_precipitation_inputs

  !___________________________________________________________________________________________________________________________!
  !___________________________________________________________________________________________________________________________

  SUBROUTINE reading_wind_inputs(lr_uwind_data, lr_vwind_data, config_namelist_blockname, ios, fu)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_uwind_data, lr_vwind_data
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu

    !_____________________________________________________________________________________!
    !Reading temperature-related input variables in the configuration file
    !_____________________________________________________________________________________!

    config_namelist_blockname="Inputs_climate_variables"
    CALL accessing_config_file(ios, fu)
    !Sizing data array with dimensions stored in the configuration file
    ALLOCATE (lr_uwind_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))
    ALLOCATE (lr_vwind_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))

   
    !Storing temperature data from LR netCDF file in array
    CALL nc_read(lr_UVwind_file, lr_uwind_id, lr_uwind_data, [1,1,t_start], &
         [lr_climate_data_x_size, lr_climate_data_y_size, t_extent])
    CALL nc_read(lr_UVwind_file, lr_vwind_id, lr_vwind_data, [1,1,t_start], &
         [lr_climate_data_x_size, lr_climate_data_y_size, t_extent])
 
  END SUBROUTINE reading_wind_inputs

  !___________________________________________________________________________________________________________________________!
  !___________________________________________________________________________________________________________________________!

  SUBROUTINE reading_topography_inputs(lr_surface_elevation_data, hr_surface_elevation_data, lr_topographic_insolation_data, &
       hr_topographic_insolation_data,  config_namelist_blockname, ios, fu)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_surface_elevation_data, &
         hr_surface_elevation_data, lr_topographic_insolation_data, hr_topographic_insolation_data
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu

    !______________________________________________________________________________________!
    !Reading topography-related input variables in the configuration file
    !______________________________________________________________________________________!
        
    config_namelist_blockname="Topography"
    CALL accessing_config_file(ios, fu)
    !Sizing data array using dimensions stored in the configuration file

    ALLOCATE (lr_surface_elevation_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))
    ALLOCATE (hr_surface_elevation_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))
    ALLOCATE (lr_topographic_insolation_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))
    ALLOCATE (hr_topographic_insolation_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))


    IF (hr_topo_t_size .EQ. 1) THEN
       CALL nc_read(lr_topographic_parameters, lr_surface_elevation_id, lr_surface_elevation_data(:,:,1))
       CALL nc_read(hr_topographic_parameters, hr_surface_elevation_id, hr_surface_elevation_data(:,:,1))
       CALL nc_read(lr_topographic_parameters, lr_topographic_insolation_id, lr_topographic_insolation_data(:,:,1))
       CALL nc_read(hr_topographic_parameters, hr_topographic_insolation_id, hr_topographic_insolation_data(:,:,1))
    ELSE
       !Storing temperature data from LR netCDF file in array
       CALL nc_read(lr_topographic_parameters, lr_surface_elevation_id, lr_surface_elevation_data)
       CALL nc_read(hr_topographic_parameters, hr_surface_elevation_id, hr_surface_elevation_data)
       CALL nc_read(lr_topographic_parameters, lr_topographic_insolation_id, lr_topographic_insolation_data)
       CALL nc_read(hr_topographic_parameters, hr_topographic_insolation_id, hr_topographic_insolation_data)
    ENDIF

    !Closing configuration file
   CLOSE(fu)

  END SUBROUTINE reading_topography_inputs

  !__________________________________________________________________________________________________________________________!
  !__________________________________________________________________________________________________________________________!
  
END MODULE Inputs_reading
