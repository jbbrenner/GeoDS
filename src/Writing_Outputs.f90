MODULE Writing_Outputs
  !________________________________________________________________________________________!

  USE Parametrization, only: Configuration_file
  USE ncio, only: nc_create, nc_write_attr, nc_write_dim
  USE Support_functions, only: config_file_access

  !________________________________________________________________________________________! 

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!
  !Declaring local variables
  INTEGER, PRIVATE :: i

  
CONTAINS

  !________________________________________________________________________________________!
  !Subroutine generating a new empty netCDF file with high-resolution grid 
  !________________________________________________________________________________________!
  
  SUBROUTINE downscaled_outputs_grid_init(x_ds_grid,y_ds_grid)

    IMPLICIT NONE
    
    DOUBLE PRECISION, ALLOCATABLE, INTENT(INOUT) :: x_ds_grid(:), y_ds_grid(:)
    
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
    
    CHARACTER (LEN=256) :: downscaled_climate_data_file

    

    !_________________________________________________________________________________!
    !Reading outputs variables in the configuration file
    !_________________________________________________________________________________!
    config_namelist_blockname="Downscaled_outputs"
    
    CALL config_file_access(config_namelist_blockname, LR_temperature_file, LR_surface_temperature_id,lrtemp_x_size,&
         lrtemp_y_size,lrtemp_t_size, HR_elevation_file, HR_surface_elevation_id,hrtopo_x_size,&
         hrtopo_y_size,hrtopo_t_size, downscaled_climate_data_file, ios, fu)

    !_________________________________________________________________________________!
    ALLOCATE (x_ds_grid(921))
    ALLOCATE (y_ds_grid(521))

    x_ds_grid(:)=0
    y_ds_grid(:)=0

    
    x_ds_grid(1)=-443750.d0 
    DO i=2,901
       x_ds_grid(i)=x_ds_grid(i-1)+1000.d0
    ENDDO

    y_ds_grid(1)=-251600.d0
    DO i=2,521
       y_ds_grid(i)=y_ds_grid(i-1)+1000.d0
    ENDDO
  
       
    CALL nc_create(downscaled_climate_data_file, OVERWRITE=.TRUE.,NETCDF4=.TRUE.)

    CALL nc_write_attr(downscaled_climate_data_file,"Title","High-resolution climate data grid")
    CALL nc_write_attr(downscaled_climate_data_file,"Institution", &
                       "Laboratoire de Sciences du Climat et de l'Environnement, GeoDS project")

    CALL nc_write_dim(downscaled_climate_data_file,"x",x=x_ds_grid,units="m")
    CALL nc_write_dim(downscaled_climate_data_file,"y",x=y_ds_grid,units="m")
    call nc_write_dim(downscaled_climate_data_file,"time",x=1.0, &
         units="years",calendar="360_day", unlimited=.TRUE.)
    
  END SUBROUTINE downscaled_outputs_grid_init

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

END MODULE Writing_Outputs
