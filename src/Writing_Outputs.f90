MODULE Writing_Outputs
  !________________________________________________________________________________________!

  USE Parametrization
  USE Support_functions, only: accessing_config_file
  USE ncio, only: nc_create, nc_write_attr, nc_write_dim, nc_write
  USE Temperature_downscaling, only: applying_lapse_rate_correction
  USE Precipitation_downscaling, only: downscaling_precipitation

  IMPLICIT NONE

  !Declaring local variables
  INTEGER, PRIVATE :: i, j
  INTEGER, PRIVATE :: k = 0

CONTAINS

  !________________________________________________________________________________________!
  !Subroutine generating a new empty netCDF file with high-resolution grid 
  !________________________________________________________________________________________!
  
  SUBROUTINE initializing_downscaled_outputs_grid(ds_x_grid, ds_y_grid, ds_monthly_t_grid,&
       ds_annual_t_grid, config_namelist_blockname,ios, fu)

    IMPLICIT NONE
    
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: ds_x_grid, ds_y_grid, ds_monthly_t_grid, ds_annual_t_grid
    CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu

    config_namelist_blockname="Global_inputs_variables"
    CALL accessing_config_file(ios, fu)
    
    CLOSE(fu)
    
    !Reading outputs variables in the configuration file
    config_namelist_blockname="Downscaled_outputs"
    CALL accessing_config_file(ios, fu)
   
    ALLOCATE (ds_x_grid(hr_topo_x_size))
    ALLOCATE (ds_y_grid(hr_topo_y_size))

    ds_x_grid(:) = 0
    ds_y_grid(:) = 0
    
    ds_x_grid(1) = ds_x_grid_lower_bound
    DO i = 2,hr_topo_x_size
       ds_x_grid(i) = ds_x_grid(i-1) + spatial_resolution
    ENDDO

    ds_y_grid(1) = ds_y_grid_lower_bound
    DO i = 2, hr_topo_y_size
       ds_y_grid(i) = ds_y_grid(i-1) + spatial_resolution
    ENDDO
    !If monthly low resolution climate data are available, the algorithm generates monthly
    !downscaled data as well. The user can choose in the configuration file whether annual
    !dataset are wanted or not (= monthly data average over a year).
    !If only annual low resolution climate data are available, the algorithm generates annual
    !downscaled data
    IF (lr_monthly_climate_data_availibility .EQV. .TRUE.) THEN
       ALLOCATE (ds_monthly_t_grid(t_extent))
       CALL nc_create(ds_monthly_climate_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
       CALL nc_write_attr(ds_monthly_climate_data_file,"Title","High-resolution climate data grid")
       CALL nc_write_attr(ds_monthly_climate_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")

       CALL nc_write_dim(ds_monthly_climate_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
       CALL nc_write_dim(ds_monthly_climate_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
       CALL nc_write_dim(ds_monthly_climate_data_file,"time",x=ds_monthly_t_grid, &
            units="months",calendar="360_day", unlimited=.TRUE.)
            !units="hours since 1800-01-01 00:00:00.0", calendar="gregorian", unlimited=.TRUE.)
       
       IF (ds_annual_data_generation .EQV. .TRUE.) THEN
          ALLOCATE (ds_annual_t_grid(t_extent/months_nbr))
          CALL nc_create(ds_annual_climate_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
          CALL nc_write_attr(ds_annual_climate_data_file,"Title","High-resolution climate data grid")
          CALL nc_write_attr(ds_annual_climate_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")

          CALL nc_write_dim(ds_annual_climate_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_climate_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_climate_data_file,"time",x=ds_annual_t_grid, &
               units="years",calendar="360_day", unlimited=.TRUE.)
       ENDIF
       
     ELSE
          ALLOCATE (ds_annual_t_grid(t_extent))
          CALL nc_create(ds_annual_climate_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
          CALL nc_write_attr(ds_annual_climate_data_file,"Title","High-resolution climate data grid")
          CALL nc_write_attr(ds_annual_climate_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")

          CALL nc_write_dim(ds_annual_climate_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_climate_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_climate_data_file,"time",x=ds_annual_t_grid, &
               units="years",calendar="360_day", unlimited=.TRUE.)
     ENDIF
        
     CLOSE(fu)
   

  END SUBROUTINE initializing_downscaled_outputs_grid

  !________________________________________________________________________________________!
  !Subroutine writing downscaled climate data in the netCDF file generated by the downscaled_outputs_grid_init subroutine
  !________________________________________________________________________________________!
  SUBROUTINE writing_downscaled_data_outputs(ds_monthly_climate_data_file, ds_annual_climate_data_file,&
       lr_surface_temperature_data, ds_monthly_climate_data, ds_annual_climate_data)

    IMPLICIT NONE
    
    CHARACTER (LEN=str_len), INTENT(IN) :: ds_monthly_climate_data_file, ds_annual_climate_data_file
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_surface_temperature_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: ds_monthly_climate_data,&
         ds_annual_climate_data


    !Downscaled climate data arrays are sized to fit the high resolution Digital Elevation Model dataset structure (target grid)
    !If monthly low resolution climate data are available, the annual downscaled climate dataset is built as the average of the
    !monthly downscaled climate dataset on 12 months time span
    IF (lr_monthly_climate_data_availibility .EQV. .TRUE.) THEN
        
      ALLOCATE (ds_monthly_climate_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:t_extent))
      ds_monthly_climate_data(:,:,:) = 0
      CALL applying_lapse_rate_correction(lr_surface_temperature_data, elevation_anomalies_data, &
           hr_surface_temperature_data, lr_hr_surface_temperature_difference)
    
      CALL downscaling_precipitation(lr_precipitation_data, hr_precipitation_data, &
       lr_hr_precipitation_data)

      ds_monthly_climate_data(:,:,:) = hr_surface_temperature_data(:,:,:)
      CALL nc_write(ds_monthly_climate_data_file, "ts", ds_monthly_climate_data(:,:,:),&
           dim1=x_dim_name, dim2=y_dim_name, dim3="time")
      
      ds_monthly_climate_data(:,:,:) = 0
      ds_monthly_climate_data(:,:,:) = lr_hr_surface_temperature_difference(:,:,:)
      CALL nc_write(ds_monthly_climate_data_file, "ts_anomalies", ds_monthly_climate_data(:,:,:), &
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")

      ds_monthly_climate_data(:,:,:) = 0
      ds_monthly_climate_data(:,:,:) = hr_precipitation_data(:,:,:)
      CALL nc_write(ds_monthly_climate_data_file, "PP", ds_monthly_climate_data(:,:,:), & 
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")


      IF (ds_annual_data_generation .EQV. .TRUE.) THEN
         ALLOCATE (ds_annual_climate_data(1:hr_topo_x_size, 1:hr_topo_y_size,&
              1:t_extent/months_nbr))
         ds_annual_climate_data(:,:,:) = 0
         DO WHILE (k<t_extent/months_nbr)
            DO j=1, months_nbr
               ds_annual_climate_data(:,:,k+1) = ds_annual_climate_data(:,:,k+1) + &
                    hr_surface_temperature_data(:,:,k*months_nbr + j)
               
            ENDDO
            ds_annual_climate_data(:,:,k+1) = ds_annual_climate_data(:,:,k+1)/months_nbr
            k=k+1
         ENDDO
         CALL nc_write(ds_annual_climate_data_file, "ts", ds_annual_climate_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")
         ds_annual_climate_data(:,:,:) = 0
         DO WHILE (k<t_extent/months_nbr)
            DO j=1, months_nbr
               ds_annual_climate_data(:,:,k+1) = ds_annual_climate_data(:,:,k+1) + &
                    lr_hr_surface_temperature_difference(:,:,k*months_nbr + j)
               
            ENDDO
            ds_annual_climate_data(:,:,k+1) = ds_annual_climate_data(:,:,k+1)/months_nbr
            k=k+1
         ENDDO
         CALL nc_write(ds_annual_climate_data_file, "ts_anomalies", ds_annual_climate_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")
         
      ENDIF
       
    ELSE
         ALLOCATE (ds_annual_climate_data(1:hr_topo_x_size, 1:hr_topo_y_size,&
              1:t_extent))
         ds_annual_climate_data(:,:,:) = 0
         CALL applying_lapse_rate_correction(lr_surface_temperature_data, elevation_anomalies_data, &
              hr_surface_temperature_data, lr_hr_surface_temperature_difference)
         ds_monthly_climate_data(:,:,:) = hr_surface_temperature_data
         CALL nc_write(ds_annual_climate_data_file, "ts", ds_annual_climate_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")
        ds_monthly_climate_data(:,:,:) = lr_hr_surface_temperature_difference
         CALL nc_write(ds_annual_climate_data_file, "ts_anomalies", ds_annual_climate_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time") 
        ds_monthly_climate_data(:,:,:) = hr_precipitation_data
         CALL nc_write(ds_annual_climate_data_file, "PP", ds_annual_climate_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")

    ENDIF
   
  END SUBROUTINE writing_downscaled_data_outputs
  !________________________________________________________________________________________________________!

  
END MODULE Writing_Outputs
