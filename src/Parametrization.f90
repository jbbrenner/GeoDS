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
  DOUBLE PRECISION, PARAMETER :: lapse_rate = 0.006 !°C.m⁻¹
  !________________________________________________________________________________________!
  !Input variables
  !________________________________________________________________________________________!
  !Global inputs variables
  LOGICAL :: lr_monthly_climate_data_availibility
  CHARACTER (LEN=str_len) :: lr_climate_data_file
  INTEGER :: lr_climate_data_x_size, lr_climate_data_y_size, lr_climate_data_t_size

  !Temperature-related variables
  CHARACTER (LEN=str_len) :: lr_surface_temperature_id
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: lr_surface_temperature_data

  !Topography-related input variables
  CHARACTER (LEN=str_len) :: lr_topographic_parameters, lr_surface_elevation_id, lr_topographic_insolation_id, &
       hr_topographic_parameters, hr_surface_elevation_id, hr_topographic_insolation_id
  INTEGER :: hr_topo_x_size, hr_topo_y_size, hr_topo_t_size
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: lr_surface_elevation_data, hr_surface_elevation_data, &
       elevation_anomalies_data, lr_topographic_insolation_data, hr_topographic_insolation_data, &
       topographic_insolation_anomalies_data
  !________________________________________________________________________________________!
  !Output variables
  !________________________________________________________________________________________!

  CHARACTER (LEN=str_len) :: ds_monthly_climate_data_file, ds_annual_climate_data_file
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ds_x_grid, ds_y_grid, ds_monthly_t_grid, ds_annual_t_grid
  DOUBLE PRECISION :: ds_x_grid_lower_bound, ds_y_grid_lower_bound, spatial_resolution
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: hr_surface_temperature_data
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: ds_monthly_climate_data, ds_annual_climate_data
  LOGICAL :: ds_annual_data_generation
  !________________________________________________________________________________________!
  
END MODULE Parametrization
