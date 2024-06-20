MODULE Support_functions

  !This module contains subsidiary subroutines used to automate certain repetitive and heavy
  !writing tasks.   
  !________________________________________________________________________________________!

  USE Parametrization

  IMPLICIT NONE
  
  !Declaring local variables
  LOGICAL, PRIVATE :: input_checking
  !________________________________________________________________________________________!
  
CONTAINS

  !________________________________________________________________________________________!
  !Subroutine used to automate access to configuration file
  !________________________________________________________________________________________!

  SUBROUTINE accessing_config_file(ios, fu)

    IMPLICIT NONE
     
    INTEGER,INTENT(INOUT) :: ios,fu

    NAMELIST/Global_inputs_variables/lr_climate_data_file, lr_monthly_climate_data_availibility, lr_climate_data_x_size,&
         lr_climate_data_y_size, lr_climate_data_t_size, x_dim_name, y_dim_name, xy_unit, t_start, t_end, lambda, alpha
    NAMELIST/Temperature/lr_surface_temperature_id
    NAMELIST/Precipitation/lr_precipitation_id
    NAMELIST/Topography/lr_topographic_parameters, lr_surface_elevation_id, lr_topographic_insolation_id, &
    hr_topographic_parameters, hr_surface_elevation_id, hr_topographic_insolation_id, hr_topo_x_size,&
       hr_topo_y_size, hr_topo_t_size
    NAMELIST/Downscaled_outputs/ds_monthly_climate_data_file, ds_annual_climate_data_file,&
         ds_x_grid_lower_bound, ds_y_grid_lower_bound,&
         spatial_resolution, ds_annual_data_generation


    !Checking whether the configuration file exists or not
    INQUIRE (file = Configuration_file, EXIST = input_checking)
    IF (input_checking .EQV. (.FALSE.)) THEN
       WRITE (*, *)"Error: input file", Configuration_file, "does not exist"
    END IF

    !Opening configuration file and reading variables depending on the block explored
    !by the calling subroutine
    SELECT CASE(config_namelist_blockname)
       
    CASE ("Global_inputs_variables")
       OPEN (NEWUNIT = fu, ACTION = 'READ', FILE = Configuration_file, IOSTAT = ios)
       IF (ios /= 0) THEN
            WRITE (*, *)"Error:", Configuration_file, "could not be opened"
       END IF
       READ (UNIT = fu, NML = Global_inputs_variables, IOSTAT = ios)
       PRINT*, "Configuration file : accessing general inputs variables"
    CASE ("Temperature")
       OPEN (NEWUNIT = fu, ACTION = 'READ', FILE = Configuration_file, IOSTAT = ios)
       IF (ios /= 0) THEN
            WRITE (*, *)"Error:", Configuration_file, "could not be opened"
       END IF
       READ (UNIT = fu, NML = Temperature, IOSTAT = ios)
       PRINT*, "Configuration file : accessing temperature-related variables"
    CASE ("Precipitation")
       OPEN (NEWUNIT = fu, ACTION = 'READ', FILE = Configuration_file, IOSTAT = ios)
       IF (ios /= 0) THEN
            WRITE (*, *)"Error:", Configuration_file, "could not be opened"
       END IF
       READ (UNIT = fu, NML = Precipitation, IOSTAT = ios)
       PRINT*, "Configuration file : accessing precipitation-related variables"
    CASE ("Topography")
       OPEN (NEWUNIT = fu, ACTION = 'READ', FILE = Configuration_file, IOSTAT = ios)
       IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file, "could not be opened"
       END IF
       READ (UNIT = fu, NML = Topography, IOSTAT = ios)
       PRINT*, "Configuration file : accessing topography-related variables"
       
    CASE ("Downscaled_outputs")
       OPEN (NEWUNIT = fu, ACTION = 'READ', FILE = Configuration_file, IOSTAT = ios)
       IF (ios /= 0) THEN
            WRITE (*, *)"Error:", Configuration_file, "could not be opened"
       END IF
       READ (UNIT = fu, NML = Downscaled_outputs, IOSTAT = ios)
       PRINT*, "Configuration file : accessing outputs-related variables"

    CASE DEFAULT
       Print *,"No namelist selected"
       
    END SELECT
           
  END SUBROUTINE accessing_config_file

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!
  
END MODULE Support_functions
