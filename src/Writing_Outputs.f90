MODULE Writing_Outputs
  !________________________________________________________________________________________!

  USE Parametrization, only: Configuration_file
  USE ncio, only: nc_create
  USE Support_functions, only: config_file_access

  !________________________________________________________________________________________! 

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

CONTAINS

  SUBROUTINE downscaled_outputs_writing()

    IMPLICIT NONE
    
    
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

    !_________________________________________________________________________________!
    !Reading outputs variables in the configuration file
    !_________________________________________________________________________________!
    config_namelist_blockname="Downscaled_outputs"
    
    CALL config_file_access(config_namelist_blockname, LR_temperature_file, LR_surface_temperature_id,lrtemp_x_size,&
         lrtemp_y_size,lrtemp_t_size, HR_elevation_file, HR_surface_elevation_id,hrtopo_x_size,&
         hrtopo_y_size,hrtopo_t_size, ds_temperature_file, ios, fu)

    !_________________________________________________________________________________!
       
    CALL nc_create(ds_temperature_file, overwrite=.TRUE.,netcdf4=.TRUE.)    
    
  END SUBROUTINE downscaled_outputs_writing

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

END MODULE Writing_Outputs
