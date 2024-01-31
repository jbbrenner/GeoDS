MODULE Reading_Variables
  
  !________________________________________________________________________________________!

  USE Parametrization
  USE ncio, ONLY: nc_read

  !________________________________________________________________________________________! 

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

CONTAINS

  !Subroutines required to read netCDF datasets and store related data in arrays using the ncio library. All the subroutines
  !open the same configuration file containing paths and other parametrization variables (namelist), but each one is associated
  !with a different category of data (i.e : data related to temperature, precipitation, topography...)
  
  SUBROUTINE inputs_temperature(LR_temperature_file, LR_surface_temperature_id,&
       LR_surface_temperature_data,lrtemp_x_size,&
       lrtemp_y_size,lrtemp_t_size)

    IMPLICIT NONE

    CHARACTER (LEN=100), INTENT(INOUT) :: LR_temperature_file
    CHARACTER (LEN=20), INTENT(INOUT) :: LR_surface_temperature_id
    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: LR_surface_temperature_data
    INTEGER, INTENT(INOUT) :: lrtemp_x_size 
    INTEGER, INTENT(INOUT) :: lrtemp_y_size
    INTEGER, INTENT(INOUT) :: lrtemp_t_size
    
    INTEGER :: ios,fu
    LOGICAL :: input_checking

    NAMELIST/Temperature/LR_temperature_file, LR_surface_temperature_id,lrtemp_x_size,&
         lrtemp_y_size,lrtemp_t_size

    !Checking whether the configuration file exists or not
    INQUIRE (file=Configuration_file, EXIST=input_checking)

        IF (input_checking .eqv. (.FALSE.)) THEN
            WRITE (*, *)"Error: input file", Configuration_file, "does not exist"
         END IF

    !Opening configuration file to access netCDF files paths (temperature data)
    OPEN (ACTION='READ', FILE=Configuration_file, IOSTAT=ios, NEWUNIT=fu)
        IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file,"could not be opened"
         END IF
    READ (NML=Temperature, IOSTAT=ios, UNIT=fu)

    !Sizing data array using dimensions stored in the configuration file
    ALLOCATE (LR_surface_temperature_data(1:lrtemp_x_size,&
         1:lrtemp_y_size,1:lrtemp_t_size))

    !Storing temperature data from LR netCDF file in array
    CALL nc_read(LR_temperature_file, LR_surface_temperature_id, LR_surface_temperature_data)

    !Closing configuration file
    CLOSE(fu)

  END SUBROUTINE inputs_temperature

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

  SUBROUTINE inputs_topography(HR_elevation_file, HR_surface_elevation_id,&
       HR_surface_elevation_data,hrtopo_x_size,&
       hrtopo_y_size,hrtopo_t_size)

    IMPLICIT NONE

    CHARACTER (LEN=100), INTENT(INOUT) :: HR_elevation_file
    CHARACTER (LEN=20), INTENT(INOUT) :: HR_surface_elevation_id
    REAL, DIMENSION(:,:,:), ALLOCATABLE, INTENT(INOUT) :: HR_surface_elevation_data
    INTEGER, INTENT(INOUT) :: hrtopo_x_size 
    INTEGER, INTENT(INOUT) :: hrtopo_y_size
    INTEGER, INTENT(INOUT) :: hrtopo_t_size
    
    INTEGER :: ios,fu
    LOGICAL :: input_checking

    NAMELIST/Topography/HR_elevation_file, HR_surface_elevation_id,hrtopo_x_size,&
         hrtopo_y_size, hrtopo_t_size

    !Checking whether the configuration file exists or not
    INQUIRE (file=Configuration_file, EXIST=input_checking)

        IF (input_checking .eqv. (.FALSE.)) THEN
            WRITE (*, *)"Error: input file", Configuration_file, "does not exist"
         END IF

    !Opening configuration file to access netCDF files paths (elevation data)
    OPEN (ACTION='READ', FILE=Configuration_file, IOSTAT=ios, NEWUNIT=fu)
        IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file,"could not be opened"
         END IF
    READ (NML=Topography, IOSTAT=ios, UNIT=fu)

    !Sizing data array using dimensions stored in the configuration file
    ALLOCATE (HR_surface_elevation_data(1:hrtopo_x_size,&
         1:hrtopo_y_size,1:hrtopo_t_size))

    !Storing temperature data from LR netCDF file in array
    CALL nc_read(HR_elevation_file, HR_surface_elevation_id, HR_surface_elevation_data)

    !Closing configuration file
    CLOSE(fu)

  END SUBROUTINE inputs_topography

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!
  
END MODULE Reading_Variables
