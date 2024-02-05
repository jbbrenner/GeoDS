MODULE Parametrization

  !________________________________________________________________________________________!  

  IMPLICIT NONE
  PUBLIC

  !________________________________________________________________________________________!
  !Support variables
  !________________________________________________________________________________________!

  INTEGER, PARAMETER :: str_len = 256
  CHARACTER (LEN=str_len) :: Configuration_file="/home/jbrenner/GeoDS/Configuration_File.nml"
  CHARACTER(LEN=str_len) :: config_namelist_blockname !String storing a blockname of the configuration file's namelist
  INTEGER :: ios, fu !Test variables
  REAL :: lapse_rate=3
  !________________________________________________________________________________________!
  !Input variables
  !________________________________________________________________________________________!

  !Temperature-related variables
  CHARACTER (LEN=str_len) :: LR_temperature_file
  CHARACTER (LEN=str_len) :: LR_surface_temperature_id
  INTEGER :: lrtemp_x_size 
  INTEGER :: lrtemp_y_size
  INTEGER :: lrtemp_t_size
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: LR_surface_temperature_data

  !Topography-related input variables
  CHARACTER (LEN=str_len) :: HR_elevation_file
  CHARACTER (LEN=str_len) :: HR_surface_elevation_id
  INTEGER :: hrtopo_x_size 
  INTEGER :: hrtopo_y_size
  INTEGER :: hrtopo_t_size
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: HR_surface_elevation_data
  !________________________________________________________________________________________!
  !Output variables
  !________________________________________________________________________________________!

  CHARACTER (LEN=str_len) :: downscaled_climate_data_file
  DOUBLE PRECISION, ALLOCATABLE :: x_ds_grid(:), y_ds_grid(:)
  INTEGER :: xdim_ds_grid, ydim_ds_grid
  !________________________________________________________________________________________!
  
END MODULE Parametrization
