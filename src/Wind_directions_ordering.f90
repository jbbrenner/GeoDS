MODULE Wind_directions_ordering

    !__________________________________________________________________________________________________________________________!
  !Module used to associate each direction with one of the wdir direction angle intervals
  !__________________________________________________________________________________________________________________________!


  USE Parametrization

  IMPLICIT NONE

  DOUBLE PRECISION, PRIVATE :: wind_direction
  INTEGER, PRIVATE :: m, i, j, t

CONTAINS

  SUBROUTINE computing_wind_directions(sorted_wind_directions_data)

    IMPLICIT NONE

    DOUBLE PRECISION, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: sorted_wind_directions_data

   ALLOCATE(sorted_wind_directions_data(1:lr_climate_data_x_size, 1:lr_climate_data_y_size, 1:t_extent))

    sorted_wind_directions_data(:,:,:) = 0

    DO t=1, t_extent
       DO j=1, hr_topo_y_size
          DO i=1, hr_topo_x_size
             wind_direction = ATAN2(lr_vwind_data(i,j,t), lr_uwind_data(i,j,t))
             IF (wind_direction .LT. 0.d0) THEN
                     wind_direction = wind_direction + pi
             ELSE
                     wind_direction = wind_direction - pi
             END IF
             DO m=1, nbr_wdir
                IF ((wind_direction .GE. wdir_angle_boundaries(m)) & 
                     .AND. (wind_direction .LT. &
                     wdir_angle_boundaries(m+1))) THEN
                   sorted_wind_directions_data(i,j,t) = m
                END IF
             END DO
          END DO
       END DO
    END DO
    
  END SUBROUTINE computing_wind_directions

  END MODULE Wind_directions_ordering
