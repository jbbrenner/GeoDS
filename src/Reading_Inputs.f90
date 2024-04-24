MODULE Reading_Inputs
  
  !________________________________________________________________________________________!

  USE Parametrization
  USE ncio, ONLY: nc_read
  USE Support_functions, only: accessing_config_file
  
  IMPLICIT NONE
 
CONTAINS

  !________________________________________________________________________________________!
  !Subroutines required to read netCDF datasets and store related data in arrays using the ncio library. All the subroutines
  !open the same configuration file containing paths and other parametrization variables (namelist), but each one is associated
  !with a different category of data (i.e : data related to temperature, precipitation, topography...)
  !________________________________________________________________________________________!
  
  SUBROUTINE reading_temperature_inputs(lr_surface_temperature_data, config_namelist_blockname, ios, fu)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_surface_temperature_data
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu

    !_____________________________________________________________________________________!
    !Reading temperature-related input variables in the configuration file
    !_____________________________________________________________________________________!
    config_namelist_blockname="Global_inputs_variables"
    CALL accessing_config_file(ios, fu)

    !Sizing data array with dimensions stored in the configuration file
    ALLOCATE (lr_surface_temperature_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:lr_climate_data_t_size))

    config_namelist_blockname="Temperature"
    CALL accessing_config_file(ios, fu)
   
    !Storing temperature data from LR netCDF file in array
    CALL nc_read(lr_climate_data_file, lr_surface_temperature_id, lr_surface_temperature_data)
    

 !PRINT*, "________________ERA_____________________________"
 !print*, lr_surface_temperature_data(1,1,1)
 !print*, lr_surface_temperature_data(1,1,2)
 !print*, '_________________________________________________'
    
  END SUBROUTINE reading_temperature_inputs
  
  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

  SUBROUTINE reading_topography_inputs(lr_surface_elevation_data, hr_surface_elevation_data, lr_topographic_insolation_data, &
       hr_topographic_insolation_data,  config_namelist_blockname, ios, fu)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_surface_elevation_data, &
         hr_surface_elevation_data, lr_topographic_insolation_data, hr_topographic_insolation_data
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu

    !_____________________________________________________________________________________!
    !Reading topography-related input variables in the configuration file
    !_____________________________________________________________________________________!
        
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

   ! PRINT*, "_______TEST_______"
   ! PRINT*, lr_topographic_insolation_data(1,1,1)
   ! PRINT*, lr_topographic_insolation_data(1,1,2)
   ! PRINT*, lr_topographic_insolation_data(1,1,7)
   ! Print*, lr_surface_elevation_data(1,1,1)
   ! print*, lr_surface_elevation_data(1,1,7)
   ! print*, hr_topographic_insolation_data(1,1,1)
   ! print*, hr_topographic_insolation_data(1,1,7)
    !Closing configuration file
   CLOSE(fu)

  END SUBROUTINE reading_topography_inputs

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!
  
END MODULE Reading_Inputs
