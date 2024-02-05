MODULE Parametrization

  !________________________________________________________________________________________!  

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !Support variables
  !________________________________________________________________________________________!
  
  CHARACTER (LEN=100), PUBLIC :: Configuration_file="/home/jbrenner/GeoDS/Configuration_File.nml"
  CHARACTER(LEN=100), PUBLIC :: config_namelist_blockname !String storing a blockname of the configuration file's namelist
  INTEGER, PUBLIC :: ios, fu !Test variables
  REAL, PUBLIC :: lapse_rate=3
  !________________________________________________________________________________________!
  !Input variables
  !________________________________________________________________________________________!

  !Temperature-related variables
  CHARACTER (LEN=256), PUBLIC :: LR_temperature_file
  CHARACTER (LEN=20), PUBLIC :: LR_surface_temperature_id
  INTEGER, PUBLIC :: lrtemp_x_size 
  INTEGER, PUBLIC :: lrtemp_y_size
  INTEGER, PUBLIC :: lrtemp_t_size
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: LR_surface_temperature_data

  !Topography-related input variables
  CHARACTER (LEN=256), PUBLIC :: HR_elevation_file
  CHARACTER (LEN=20), PUBLIC :: HR_surface_elevation_id
  INTEGER, PUBLIC :: hrtopo_x_size 
  INTEGER, PUBLIC :: hrtopo_y_size
  INTEGER, PUBLIC :: hrtopo_t_size
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: HR_surface_elevation_data
  !________________________________________________________________________________________!
  !Output variables
  !________________________________________________________________________________________!

  CHARACTER (LEN=256), PUBLIC :: downscaled_climate_data_file
  DOUBLE PRECISION, ALLOCATABLE, PUBLIC :: x_ds_grid(:), y_ds_grid(:)
  INTEGER, PUBLIC :: xdim_ds_grid, ydim_ds_grid
  !________________________________________________________________________________________!
  
END MODULE Parametrization
