MODULE Reading_Inputs
  
  !________________________________________________________________________________________!

  USE Parametrization
  USE ncio, ONLY: nc_read
  USE Support_functions, only: config_file_access
  
  IMPLICIT NONE
 
CONTAINS

  !________________________________________________________________________________________!
  !Subroutines required to read netCDF datasets and store related data in arrays using the ncio library. All the subroutines
  !open the same configuration file containing paths and other parametrization variables (namelist), but each one is associated
  !with a different category of data (i.e : data related to temperature, precipitation, topography...)
  !________________________________________________________________________________________!
  
  SUBROUTINE inputs_temperature(LR_surface_temperature_data, config_namelist_blockname, ios, fu)

    IMPLICIT NONE

    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: LR_surface_temperature_data
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu
    

    !_____________________________________________________________________________________!
    !Reading temperature-related input variables in the configuration file
    !_____________________________________________________________________________________!
    config_namelist_blockname="Temperature"

    CALL config_file_access(ios, fu)

    !Sizing data array with dimensions stored in the configuration file
    ALLOCATE (LR_surface_temperature_data(1:lrtemp_x_size,&
         1:lrtemp_y_size,1:lrtemp_t_size))

    !Storing temperature data from LR netCDF file in array
    CALL nc_read(LR_temperature_file, LR_surface_temperature_id, LR_surface_temperature_data)

  END SUBROUTINE inputs_temperature
  
  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

  SUBROUTINE inputs_topography(HR_surface_elevation_data,config_namelist_blockname, ios, fu)

    IMPLICIT NONE

    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: HR_surface_elevation_data
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu

    !_____________________________________________________________________________________!
    !Reading topography-related input variables in the configuration file
    !_____________________________________________________________________________________!
        
    config_namelist_blockname="Topography"
    
    CALL config_file_access(ios, fu)
    
    !Sizing data array using dimensions stored in the configuration file
    
    ALLOCATE (HR_surface_elevation_data(1:hrtopo_x_size,&
         1:hrtopo_y_size,1:hrtopo_t_size))


    IF (hrtopo_t_size .EQ. 1) THEN       
     CALL nc_read(HR_elevation_file, HR_surface_elevation_id, HR_surface_elevation_data(:,:,1))
   ELSE
    !Storing temperature data from LR netCDF file in array
     CALL nc_read(HR_elevation_file, HR_surface_elevation_id, HR_surface_elevation_data)
   ENDIF
    !Closing configuration file
   CLOSE(fu)

  END SUBROUTINE inputs_topography

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!
  
END MODULE Reading_Inputs
