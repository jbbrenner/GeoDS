MODULE Parametrization

  !________________________________________________________________________________________!  

  
  IMPLICIT NONE
  PUBLIC

  !________________________________________________________________________________________!
  !Support variables
  !________________________________________________________________________________________!

  INTEGER, PARAMETER :: str_len = 256
  INTEGER, PARAMETER :: months_nbr = 12
  CHARACTER (LEN=str_len) :: Configuration_file="/home/jbrenner/GeoDS/Configuration_File.nml"
  CHARACTER(LEN=str_len) :: config_namelist_blockname !String storing a blockname of the configuration file's namelist
  INTEGER :: ios, fu !Test variables
  REAL :: lapse_rate=3
  !________________________________________________________________________________________!
  !Input variables
  !________________________________________________________________________________________!
  !Global inputs variables
  LOGICAL :: lr_monthly_climate_data_availibility
  CHARACTER (LEN=str_len) :: lr_climate_data_file
  INTEGER :: lr_climate_data_x_size 
  INTEGER :: lr_climate_data_y_size
  INTEGER :: lr_climate_data_t_size

  !Temperature-related variables
  CHARACTER (LEN=str_len) :: lr_surface_temperature_id
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: lr_surface_temperature_data

  !Topography-related input variables
  CHARACTER (LEN=str_len) :: hr_elevation_file
  CHARACTER (LEN=str_len) :: hr_surface_elevation_id
  INTEGER :: hr_topo_x_size 
  INTEGER :: hr_topo_y_size
  INTEGER :: hr_topo_t_size
  REAL, DIMENSION(:,:,:), ALLOCATABLE :: hr_surface_elevation_data
  !________________________________________________________________________________________!
  !Output variables
  !________________________________________________________________________________________!

  CHARACTER (LEN=str_len) :: ds_monthly_climate_data_file
  CHARACTER (LEN=str_len) :: ds_annual_climate_data_file
  DOUBLE PRECISION, ALLOCATABLE :: ds_x_grid(:), ds_y_grid(:), ds_monthly_t_grid(:), ds_annual_t_grid(:)
  DOUBLE PRECISION :: ds_x_grid_lower_bound, ds_y_grid_lower_bound, spatial_resolution
  LOGICAL :: ds_annual_data_generation
  !________________________________________________________________________________________!
  
END MODULE Parametrization
