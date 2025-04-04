MODULE Parametrization

  !________________________________________________________________________________________!  

  USE Derived_types
  
  IMPLICIT NONE
  PUBLIC

  !________________________________________________________________________________________!
  !Support variables
  !________________________________________________________________________________________!

  INTEGER, PARAMETER :: str_len = 256
  INTEGER, PARAMETER :: months_nbr = 12
  CHARACTER (LEN=str_len) :: Configuration_file = "/home/jbrenner/GeoDS/Configuration_File.nml"
  CHARACTER(LEN=str_len) :: config_namelist_blockname !String storing a blockname of the configuration file's namelist
  INTEGER :: ios, fu !Test variables
  DOUBLE PRECISION, PARAMETER :: T_conv = 273.15
  DOUBLE PRECISION, PARAMETER :: pi = ACOS(-1.0)
  DOUBLE PRECISION, PARAMETER :: lapse_rate = 0.0065
  !________________________________________________________________________________________!
  !Input variables
  !________________________________________________________________________________________!
  
  !Global inputs variables
  LOGICAL :: lr_monthly_climate_data_availability, wdir_grids_generation
  INTEGER :: lr_climate_data_x_size, lr_climate_data_y_size, lr_climate_data_t_size
  CHARACTER (LEN=str_len) :: x_dim_name, y_dim_name, xy_unit
  DOUBLE PRECISION :: missing_data_error_code
  INTEGER :: t_start, t_end, t_extent
  REAL :: lambda, alpha, beta, delta

  !Climate-related variables
  CHARACTER (LEN=str_len) :: lr_climate_data_file
  CHARACTER (LEN=str_len) :: lr_UVwind_file
  CHARACTER (LEN=str_len) :: lr_surface_temperature_id
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: lr_surface_temperature_data
  CHARACTER (LEN=str_len) :: lr_precipitation_id
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: lr_precipitation_data
  CHARACTER (LEN=str_len) :: lr_uwind_id
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: lr_uwind_data
  CHARACTER (LEN=str_len) :: lr_vwind_id
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: lr_vwind_data

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


  !Topography-related outputs variables
  TYPE(wl_pattern_arr), DIMENSION(:), ALLOCATABLE :: WL_pattern_pointers_array !Dynamical array storing the nbr_wdir pointers to the wind exposure arrays associated to the nbr_wdir directions of wind 
  TYPE(tei_arr), DIMENSION(:), ALLOCATABLE :: TEI_pointers_array, TEI_drying_effect_correction !Dynamical array storing the nbr_wdir pointers to topographic
 !exposure indexes arrays, i.e. the arrays containing the index for each point for every nbr_wdir wind directions

  !Climate-related outputs variables
  CHARACTER (LEN=str_len/2) :: wdir_patterns_file_path
  CHARACTER (LEN=str_len) :: ds_monthly_temperature_data_file, ds_annual_temperature_data_file, & 
        ds_monthly_precipitation_data_file, ds_annual_precipitation_data_file, topographic_exposure_indexes_file, &
        sorted_wind_directions_file
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: ds_x_grid, ds_y_grid, tei_wdir_grid, ds_monthly_t_grid, ds_annual_t_grid
  DOUBLE PRECISION :: ds_x_grid_lower_bound, ds_y_grid_lower_bound, spatial_resolution
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: hr_surface_temperature_data
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: hr_lr_surface_temperature_anomalies
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: hr_precipitation_data
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: hr_lr_precipitation_ratio
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: hr_lr_precipitation_anomalies 
  DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE :: ds_annual_temperature_data, &
          ds_annual_precipitation_data, topographic_exposure_indexes_data, &
          sorted_wind_directions_data
  LOGICAL :: ds_annual_data_generation
  INTEGER :: nbr_wdir
  DOUBLE PRECISION :: TEI_windward_searching_dist
  DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE :: wdir_angle_boundaries
  DOUBLE PRECISION :: max_precipitation_increase_factor
  LOGICAL :: broad_mountain_range_drying_effect_activator
  DOUBLE PRECISION :: drying_effect_windward_searching_dist 
  !________________________________________________________________________________________!
 
  
END MODULE Parametrization
