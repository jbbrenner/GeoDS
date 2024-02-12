MODULE Topographic_parameters_computation

  USE Parametrization

  IMPLICIT NONE

CONTAINS

  !_______________________________________________________!
  !Simple subroutine used to calculate elevation anomalies between low resolution (LR)
  !DEM and high resolution (HR) DEM. Note that the LR grid must be first interpolated
  !on the HR one
  SUBROUTINE computing_elevation_anomalies(lr_surface_elevation_data, hr_surface_elevation_data, elevation_anomalies_data)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(IN) :: lr_surface_elevation_data, &
         hr_surface_elevation_data
    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: elevation_anomalies_data

    ALLOCATE (elevation_anomalies_data(1:hr_topo_x_size, 1:hr_topo_y_size, 1:hr_topo_t_size))

    elevation_anomalies_data(:,:,:) = lr_surface_elevation_data(:,:,:) - hr_surface_elevation_data(:,:,:)

    PRINT*, "anomalies", (sum(elevation_anomalies_data))/(hr_topo_x_size*hr_topo_y_size*hr_topo_t_size)
    
  END SUBROUTINE computing_elevation_anomalies
  
  
END MODULE Topographic_parameters_computation
