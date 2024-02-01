MODULE Reading_Inputs
  
  !________________________________________________________________________________________!

  USE Parametrization, only: Configuration_file
  USE ncio, ONLY: nc_read
  USE Support_functions, only: config_file_access
  
  !________________________________________________________________________________________! 

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

CONTAINS

  !Subroutines required to read netCDF datasets and store related data in arrays using the ncio library. All the subroutines
  !open the same configuration file containing paths and other parametrization variables (namelist), but each one is associated
  !with a different category of data (i.e : data related to temperature, precipitation, topography...)
  
  SUBROUTINE inputs_temperature(LR_surface_temperature_data)

    IMPLICIT NONE

    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: LR_surface_temperature_data

    CHARACTER(LEN=100) :: config_namelist_blockname
    INTEGER :: ios, fu

    CHARACTER (LEN=256) :: LR_temperature_file
    CHARACTER (LEN=20) :: LR_surface_temperature_id
    INTEGER :: lrtemp_x_size 
    INTEGER :: lrtemp_y_size
    INTEGER :: lrtemp_t_size

    CHARACTER (LEN=256) :: HR_elevation_file
    CHARACTER (LEN=20) :: HR_surface_elevation_id
    INTEGER :: hrtopo_x_size 
    INTEGER :: hrtopo_y_size
    INTEGER :: hrtopo_t_size
    
    CHARACTER (LEN=256) :: ds_temperature_file

    config_namelist_blockname="Temperature"

    CALL config_file_access(config_namelist_blockname, LR_temperature_file, LR_surface_temperature_id,lrtemp_x_size,&
         lrtemp_y_size,lrtemp_t_size, HR_elevation_file, HR_surface_elevation_id,hrtopo_x_size,&
         hrtopo_y_size,hrtopo_t_size, ds_temperature_file, ios, fu)

    !Sizing data array with dimensions stored in the configuration file
    ALLOCATE (LR_surface_temperature_data(1:lrtemp_x_size,&
         1:lrtemp_y_size,1:lrtemp_t_size))

    !Storing temperature data from LR netCDF file in array
    CALL nc_read(LR_temperature_file, LR_surface_temperature_id, LR_surface_temperature_data)

  END SUBROUTINE inputs_temperature

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

  SUBROUTINE inputs_topography(HR_surface_elevation_data)

    IMPLICIT NONE

    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: HR_surface_elevation_data

    CHARACTER(LEN=100) :: config_namelist_blockname
    INTEGER :: ios, fu
    
    CHARACTER (LEN=256) :: LR_temperature_file
    CHARACTER (LEN=20) :: LR_surface_temperature_id
    INTEGER :: lrtemp_x_size 
    INTEGER :: lrtemp_y_size
    INTEGER :: lrtemp_t_size

    CHARACTER (LEN=256) :: HR_elevation_file
    CHARACTER (LEN=20) :: HR_surface_elevation_id
    INTEGER :: hrtopo_x_size 
    INTEGER :: hrtopo_y_size
    INTEGER :: hrtopo_t_size
    
    CHARACTER (LEN=256) :: ds_temperature_file
    
    config_namelist_blockname="Topography"
    
    CALL config_file_access(config_namelist_blockname, LR_temperature_file, LR_surface_temperature_id,lrtemp_x_size,&
         lrtemp_y_size,lrtemp_t_size, HR_elevation_file, HR_surface_elevation_id,hrtopo_x_size,&
         hrtopo_y_size,hrtopo_t_size, ds_temperature_file, ios, fu)
    
    !Sizing data array using dimensions stored in the configuration file
    ALLOCATE (HR_surface_elevation_data(1:hrtopo_x_size,&
         1:hrtopo_y_size,1:hrtopo_t_size))

    !Storing temperature data from LR netCDF file in array
    CALL nc_read(HR_elevation_file, HR_surface_elevation_id, HR_surface_elevation_data)

    !Closing configuration file
   ! CLOSE(fu)

  END SUBROUTINE inputs_topography

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!
  
END MODULE Reading_Inputs
