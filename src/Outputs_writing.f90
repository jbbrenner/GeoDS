MODULE Outputs_writing
  !________________________________________________________________________________________!

  USE Parametrization
  USE Support_functions, only: accessing_config_file
  USE ncio, only: nc_create, nc_write_attr, nc_write_dim, nc_write
  USE Temperature_downscaling, only: applying_lapse_rate_correction
  USE Precipitation_downscaling, only: downscaling_precipitation

  IMPLICIT NONE

  !Declaring local variables
  INTEGER, PRIVATE :: i, j, m
  INTEGER, PRIVATE :: k = 0
  CHARACTER (LEN=str_len), PRIVATE :: txt_file_name, temporary_name
  CHARACTER (LEN=3), PRIVATE :: str

CONTAINS

  !________________________________________________________________________________________!
  !Subroutine generating a txt file to study specifically wind direction grids. One txt file is created
  !for each of the nbr_wdir wind directions, containing the relative ix, iy and horizontal_distance of 
  !the influence points in the windward direction of the point the TEI is being calculated
  !________________________________________________________________________________________!

  SUBROUTINE writing_wdir_gridpoints_patterns()

    IMPLICIT NONE
    IF (wdir_grids_generation .EQV. .TRUE.) THEN
    DO m=1, nbr_wdir
       WRITE(str,'(I2)') m
       txt_file_name = wdir_patterns_file_path // "/Wdir_grids/Wdir_grid" // str // ".txt"
       temporary_name=''
       j=1
       DO i=1, LEN(txt_file_name)
          IF (txt_file_name(i:i) .NE. " ") THEN
             temporary_name(j:j) = txt_file_name(i:i)
             j = j + 1
          END IF
       END DO
       txt_file_name = temporary_name
       !PRINT*, "file's name : ", txt_file_name
       OPEN(unit=11, file=txt_file_name)
       WRITE(11, '(A1,A1,A1,A1,A10)') "X",";","Y",";","horiz_dist"
       DO k=1, SIZE(WL_pattern_pointers_array(m)%wl_arr_ptr)
          WRITE(11, '(I5,A1,I5,A1,F8.2)') WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative,";",&
            WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative,";",&
            WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist
            !IF (m .EQ. 1) THEN
             !       PRINT*, WL_pattern_pointers_array(m)%wl_arr_ptr(k)%ix_relative,";",&
              !      WL_pattern_pointers_array(m)%wl_arr_ptr(k)%jy_relative,";",&
               !     WL_pattern_pointers_array(m)%wl_arr_ptr(k)%horizontal_dist
                !    PRINT*, '__'
           ! END IF
       END DO
       CLOSE(11)
      END DO
   END IF
   
  END SUBROUTINE writing_wdir_gridpoints_patterns
  
  !________________________________________________________________________________________!
  !Subroutine generating a new empty netCDF file with high-resolution grid 
  !________________________________________________________________________________________!
  
  SUBROUTINE initializing_downscaled_outputs_grid(ds_x_grid, ds_y_grid, tei_wdir_grid, ds_monthly_t_grid,&
       ds_annual_t_grid, config_namelist_blockname,ios, fu)

    IMPLICIT NONE
    
    DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: ds_x_grid, ds_y_grid, tei_wdir_grid, &
            ds_monthly_t_grid, ds_annual_t_grid
    CHARACTER(LEN=str_len), INTENT(INOUT) :: config_namelist_blockname
    INTEGER, INTENT(INOUT) :: ios, fu

    config_namelist_blockname="Global_parametrization"
    CALL accessing_config_file(ios, fu)
    
    CLOSE(fu)
    
    !Reading outputs variables in the configuration file
    config_namelist_blockname="Downscaled_outputs"
    CALL accessing_config_file(ios, fu)
   
    ALLOCATE (ds_x_grid(hr_topo_x_size))
    ALLOCATE (ds_y_grid(hr_topo_y_size))
    ALLOCATE (tei_wdir_grid(nbr_wdir))

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

    !TEI output writing___________________________________________________________________________________
    CALL nc_create(topographic_exposure_indexes_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
    CALL nc_write_attr(topographic_exposure_indexes_file,"Title","High-resolution climate data grid")
    CALL nc_write_attr(topographic_exposure_indexes_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
    CALL nc_write_dim(topographic_exposure_indexes_file,x_dim_name,x=ds_x_grid,units=xy_unit)
    CALL nc_write_dim(topographic_exposure_indexes_file,y_dim_name,x=ds_y_grid,units=xy_unit)
    CALL nc_write_dim(topographic_exposure_indexes_file,"wdir",x=tei_wdir_grid,units="no_unit")
    
    !If monthly low resolution climate data are available, the algorithm generates monthly
    !downscaled data as well. The user can choose in the configuration file whether annual
    !dataset are wanted or not (= monthly data average over a year).
    !If only annual low resolution climate data are available, the algorithm generates annual
    !downscaled data
    IF (lr_monthly_climate_data_availability .EQV. .TRUE.) THEN
       ALLOCATE (ds_monthly_t_grid(t_extent))
       ds_monthly_t_grid(:) = 0


       !Sorted wind directions output writing________________________________________________________________

       CALL nc_create(sorted_wind_directions_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
       CALL nc_write_attr(sorted_wind_directions_file,"Title","High-resolution climate data grid")
       CALL nc_write_attr(sorted_wind_directions_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
       CALL nc_write_dim(sorted_wind_directions_file,x_dim_name,x=ds_x_grid,units=xy_unit)
       CALL nc_write_dim(sorted_wind_directions_file,y_dim_name,x=ds_y_grid,units=xy_unit)
       CALL nc_write_dim(sorted_wind_directions_file,"time",x=ds_monthly_t_grid,&
            units="months",calendar="360_day", unlimited=.TRUE.)

       !Downscaled output climate data writing______________________________________________________________

       ds_monthly_t_grid(:) = 0
       CALL nc_create(ds_monthly_temperature_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
       CALL nc_write_attr(ds_monthly_temperature_data_file,"Title","High-resolution climate data grid")
       CALL nc_write_attr(ds_monthly_temperature_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
       CALL nc_write_dim(ds_monthly_temperature_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
       CALL nc_write_dim(ds_monthly_temperature_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
       CALL nc_write_dim(ds_monthly_temperature_data_file,"time",x=ds_monthly_t_grid, &
            units="months",calendar="360_day", unlimited=.TRUE.)
            !units="hours since 1800-01-01 00:00:00.0", calendar="gregorian", unlimited=.TRUE.)
 

       ds_monthly_t_grid(:) = 0
       CALL nc_create(ds_monthly_precipitation_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
       CALL nc_write_attr(ds_monthly_precipitation_data_file,"Title","High-resolution climate data grid")
       CALL nc_write_attr(ds_monthly_precipitation_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
       CALL nc_write_dim(ds_monthly_precipitation_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
       CALL nc_write_dim(ds_monthly_precipitation_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
       CALL nc_write_dim(ds_monthly_precipitation_data_file,"time",x=ds_monthly_t_grid, &
            units="months",calendar="360_day", unlimited=.TRUE.)


       IF (ds_annual_data_generation .EQV. .TRUE.) THEN
          ALLOCATE (ds_annual_t_grid(t_extent/months_nbr))

          CALL nc_create(ds_annual_temperature_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
          CALL nc_write_attr(ds_annual_temperature_data_file,"Title","High-resolution climate data grid")
          CALL nc_write_attr(ds_annual_temperature_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
          CALL nc_write_dim(ds_annual_temperature_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_temperature_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_temperature_data_file,"time",x=ds_annual_t_grid, &
               units="years",calendar="360_day", unlimited=.TRUE.)
        

          CALL nc_create(ds_annual_precipitation_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
          CALL nc_write_attr(ds_annual_precipitation_data_file,"Title","High-resolution climate data grid")
          CALL nc_write_attr(ds_annual_precipitation_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
          CALL nc_write_dim(ds_annual_precipitation_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_precipitation_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_precipitation_data_file,"time",x=ds_annual_t_grid, &
               units="years",calendar="360_day", unlimited=.TRUE.)


       ENDIF
       
     ELSE
          ALLOCATE (ds_annual_t_grid(t_extent))

          CALL nc_create(sorted_wind_directions_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
          CALL nc_write_attr(sorted_wind_directions_file,"Title","High-resolution climate data grid")
          CALL nc_write_attr(sorted_wind_directions_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
          CALL nc_write_dim(sorted_wind_directions_file,x_dim_name,x=ds_x_grid,units=xy_unit)
          CALL nc_write_dim(sorted_wind_directions_file,y_dim_name,x=ds_y_grid,units=xy_unit)
          CALL nc_write_dim(sorted_wind_directions_file,"time",x=ds_annual_t_grid,&
            units="years",calendar="360_day", unlimited=.TRUE.)


          CALL nc_create(ds_annual_temperature_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
          CALL nc_write_attr(ds_annual_temperature_data_file,"Title","High-resolution climate data grid")
          CALL nc_write_attr(ds_annual_temperature_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
          CALL nc_write_dim(ds_annual_temperature_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_temperature_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_temperature_data_file,"time",x=ds_annual_t_grid, &
               units="years",calendar="360_day", unlimited=.TRUE.)


          CALL nc_create(ds_annual_precipitation_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)
          CALL nc_write_attr(ds_annual_precipitation_data_file,"Title","High-resolution climate data grid")
          CALL nc_write_attr(ds_annual_precipitation_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")
          CALL nc_write_dim(ds_annual_precipitation_data_file,x_dim_name,x=ds_x_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_precipitation_data_file,y_dim_name,x=ds_y_grid,units=xy_unit)
          CALL nc_write_dim(ds_annual_precipitation_data_file,"time",x=ds_annual_t_grid, &
               units="years",calendar="360_day", unlimited=.TRUE.)

     ENDIF
        
     CLOSE(fu)
   

  END SUBROUTINE initializing_downscaled_outputs_grid

  !________________________________________________________________________________________!
  !Subroutine writing downscaled climate data in the netCDF file generated by the downscaled_outputs_grid_init subroutine
  !________________________________________________________________________________________!
  SUBROUTINE writing_downscaled_data_outputs(ds_monthly_temperature_data_file, ds_annual_temperature_data_file,&
       ds_monthly_precipitation_data_file, ds_annual_precipitation_data_file, topographic_exposure_indexes_file, &
       lr_surface_temperature_data, ds_annual_temperature_data, ds_annual_precipitation_data, &
       topographic_exposure_indexes_data, sorted_wind_directions_data)

    IMPLICIT NONE
    
    CHARACTER (LEN=str_len), INTENT(IN) :: ds_monthly_temperature_data_file, ds_annual_temperature_data_file, &
            ds_monthly_precipitation_data_file, ds_annual_precipitation_data_file, topographic_exposure_indexes_file
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: lr_surface_temperature_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: ds_annual_temperature_data, &
         ds_annual_precipitation_data, topographic_exposure_indexes_data, sorted_wind_directions_data


    ALLOCATE(topographic_exposure_indexes_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:nbr_wdir))
   
     
    DO m=1, nbr_wdir
       DO j=1, hr_topo_y_size
          DO i=1, hr_topo_x_size
             topographic_exposure_indexes_data(i, j, m) = TEI_pointers_array(m)%tei_arr_ptr(i, j)
          END DO
       END DO
    END DO
   
    !Downscaled climate data arrays are sized to fit the high resolution Digital Elevation Model dataset structure (target grid)
    !If monthly low resolution climate data are available, the annual downscaled climate dataset is built as the average of the
    !monthly downscaled climate dataset on 12 months time span
    IF (lr_monthly_climate_data_availability .EQV. .TRUE.) THEN
        
      CALL applying_lapse_rate_correction(lr_surface_temperature_data, elevation_anomalies_data, &
           hr_surface_temperature_data, hr_lr_surface_temperature_anomalies)
    
      CALL downscaling_precipitation(lr_precipitation_data, hr_precipitation_data, &
       hr_lr_precipitation_ratio, hr_lr_precipitation_anomalies)

      CALL nc_write(topographic_exposure_indexes_file, "topographic_exposure_index", &
           topographic_exposure_indexes_data(:,:,:), dim1=x_dim_name, dim2=y_dim_name, dim3="wdir")

      CALL nc_write(sorted_wind_directions_file, "sorted_wind_directions", &
           sorted_wind_directions_data(:,:,:), dim1=x_dim_name, dim2=y_dim_name, dim3="time")

      CALL nc_write(ds_monthly_temperature_data_file, "ts", hr_surface_temperature_data(:,:,:),&
           dim1=x_dim_name, dim2=y_dim_name, dim3="time")

      CALL nc_write(ds_monthly_temperature_data_file, "ts_anomalies", hr_lr_surface_temperature_anomalies(:,:,:), &
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")

      CALL nc_write(ds_monthly_precipitation_data_file, "PP", hr_precipitation_data(:,:,:), & 
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")
      
      CALL nc_write(ds_monthly_precipitation_data_file, "PP_ratio", hr_lr_precipitation_ratio(:,:,:), &
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")

      CALL nc_write(ds_monthly_precipitation_data_file, "PP_anomalies", hr_lr_precipitation_anomalies(:,:,:), &
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")



      IF (ds_annual_data_generation .EQV. .TRUE.) THEN
         ALLOCATE (ds_annual_temperature_data(1:hr_topo_x_size, 1:hr_topo_y_size,&
              1:t_extent/months_nbr))
         ds_annual_temperature_data(:,:,:) = 0
         k = 0
         DO WHILE (k<t_extent/months_nbr)
            DO j=1, months_nbr
               ds_annual_temperature_data(:,:,k+1) = ds_annual_temperature_data(:,:,k+1) + &
                    hr_surface_temperature_data(:,:,k*months_nbr + j)              
            ENDDO
            ds_annual_temperature_data(:,:,k+1) = ds_annual_temperature_data(:,:,k+1)/months_nbr
            k=k+1
         ENDDO
         CALL nc_write(ds_annual_temperature_data_file, "ts", ds_annual_temperature_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")
         ds_annual_temperature_data(:,:,:) = 0

         k = 0
         DO WHILE (k<t_extent/months_nbr)
            DO j=1, months_nbr
               ds_annual_temperature_data(:,:,k+1) = ds_annual_temperature_data(:,:,k+1) + &
                    hr_lr_surface_temperature_anomalies(:,:,k*months_nbr + j)
               
            ENDDO
            ds_annual_temperature_data(:,:,k+1) = ds_annual_temperature_data(:,:,k+1)/months_nbr
            k=k+1
         ENDDO
         CALL nc_write(ds_annual_temperature_data_file, "ts_anomalies", ds_annual_temperature_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")


        ALLOCATE (ds_annual_precipitation_data(1:hr_topo_x_size, 1:hr_topo_y_size,&
              1:t_extent/months_nbr))
        ds_annual_precipitation_data(:,:,:) = 0
        
        k = 0
         DO WHILE (k<t_extent/months_nbr)
            DO j=1, months_nbr
               ds_annual_precipitation_data(:,:,k+1) = ds_annual_precipitation_data(:,:,k+1) + &
                    hr_precipitation_data(:,:,k*months_nbr + j)
            ENDDO
            ds_annual_precipitation_data(:,:,k+1) = ds_annual_precipitation_data(:,:,k+1)/months_nbr
            k=k+1
         ENDDO
         CALL nc_write(ds_annual_precipitation_data_file, "PP", ds_annual_precipitation_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")
         ds_annual_precipitation_data(:,:,:) = 0

         k = 0
         DO WHILE (k<t_extent/months_nbr)
            DO j=1, months_nbr
               ds_annual_precipitation_data(:,:,k+1) = ds_annual_precipitation_data(:,:,k+1) + &
                    hr_lr_precipitation_ratio(:,:,k*months_nbr + j)
            ENDDO
            ds_annual_precipitation_data(:,:,k+1) = ds_annual_precipitation_data(:,:,k+1)/months_nbr
            k=k+1
         ENDDO
         CALL nc_write(ds_annual_precipitation_data_file, "PP_ratio", ds_annual_precipitation_data(:,:,:),& 
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")
         ds_annual_precipitation_data(:,:,:) = 0

         k = 0
         DO WHILE (k<t_extent/months_nbr)
            DO j=1, months_nbr
               ds_annual_precipitation_data(:,:,k+1) = ds_annual_precipitation_data(:,:,k+1) + &
                    hr_lr_precipitation_anomalies(:,:,k*months_nbr + j)
            ENDDO
            ds_annual_precipitation_data(:,:,k+1) = ds_annual_precipitation_data(:,:,k+1)/months_nbr
            k=k+1
         ENDDO
         CALL nc_write(ds_annual_precipitation_data_file, "PP_anomalies", ds_annual_precipitation_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")

      ENDIF
       
    ELSE
         ALLOCATE (ds_annual_temperature_data(1:hr_topo_x_size, 1:hr_topo_y_size,&
              1:t_extent))
         ALLOCATE (ds_annual_precipitation_data(1:hr_topo_x_size, 1:hr_topo_y_size,&
              1:t_extent))
         ds_annual_temperature_data(:,:,:) = 0
         ds_annual_precipitation_data(:,:,:) = 0
         CALL applying_lapse_rate_correction(lr_surface_temperature_data, elevation_anomalies_data, &
              hr_surface_temperature_data, hr_lr_surface_temperature_anomalies)
         ds_annual_temperature_data(:,:,:) = hr_surface_temperature_data
         CALL nc_write(sorted_wind_directions_file, "sorted_wind_directions", &
           sorted_wind_directions_data(:,:,:), dim1=x_dim_name, dim2=y_dim_name, dim3="time")
         CALL nc_write(ds_annual_temperature_data_file, "ts", ds_annual_temperature_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")
        ds_annual_temperature_data(:,:,:) = hr_lr_surface_temperature_anomalies
         CALL nc_write(ds_annual_temperature_data_file, "ts_anomalies", ds_annual_temperature_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time") 
        ds_annual_precipitation_data(:,:,:) = hr_precipitation_data
         CALL nc_write(ds_annual_precipitation_data_file, "PP", ds_annual_precipitation_data(:,:,:),&
              dim1=x_dim_name, dim2=y_dim_name, dim3="time")

    ENDIF
   
  END SUBROUTINE writing_downscaled_data_outputs
  !________________________________________________________________________________________________________!

  
END MODULE Outputs_writing
