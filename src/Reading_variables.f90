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
  
  SUBROUTINE inputs_temperature(LR_temperature_dataset_file, LR_surface_temperature_id,LR_surface_temperature_data)

    IMPLICIT NONE

    CHARACTER (LEN=100), INTENT(INOUT) :: LR_temperature_dataset_file
    CHARACTER (LEN=20), INTENT(INOUT) :: LR_surface_temperature_id
    REAL, DIMENSION(64,32,1200), INTENT(INOUT) :: LR_surface_temperature_data
    INTEGER :: ios,fu
    LOGICAL :: input_checking

    NAMELIST/Temperature/LR_temperature_dataset_file, LR_surface_temperature_id


    !Checking whether the configuration file exists or not
    INQUIRE (file=Configuration_file, EXIST=input_checking)

        IF (input_checking .eqv. (.FALSE.)) THEN
            WRITE (*, *)"Error: input file ", Configuration_file, " does not exist"
         END IF

    !Opening configuration file to access netCDF files paths (temperature data)
    OPEN (ACTION='READ', FILE=Configuration_file, IOSTAT=ios, NEWUNIT=fu)
        IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file,"could not be opened"
         END IF
    READ (NML=Temperature, IOSTAT=ios, UNIT=fu)

    !Storing temperature data from LR netCDF file in array
    CALL nc_read(LR_temperature_dataset_file, LR_surface_temperature_id, LR_surface_temperature_data)

    !Closing configuration file
    CLOSE(fu)

  END SUBROUTINE inputs_temperature

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

  SUBROUTINE inputs_topography(HR_elevation_file, HR_surface_elevation_id,HR_surface_elevation_data)

    IMPLICIT NONE

    CHARACTER (LEN=100), INTENT(INOUT) :: HR_elevation_file
    CHARACTER (LEN=20), INTENT(INOUT) :: HR_surface_elevation_id
    REAL, DIMENSION(71,61), INTENT(INOUT) :: HR_surface_elevation_data
    INTEGER :: ios,fu
    LOGICAL :: input_checking

    NAMELIST/Topography/HR_elevation_file, HR_surface_elevation_id


    !Checking whether the configuration file exists or not
    INQUIRE (file=Configuration_file, EXIST=input_checking)

        IF (input_checking .eqv. (.FALSE.)) THEN
            WRITE (*, *)"Error: input file ", Configuration_file, " does not exist"
         END IF

    !Opening configuration file to access netCDF files paths (temperature data)
    OPEN (ACTION='READ', FILE=Configuration_file, IOSTAT=ios, NEWUNIT=fu)
        IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file,"could not be opened"
         END IF
    READ (NML=Topography, IOSTAT=ios, UNIT=fu)

    !Storing temperature data from LR netCDF file in array
    CALL nc_read(HR_elevation_file, HR_surface_elevation_id, HR_surface_elevation_data)

    !Closing configuration file
    CLOSE(fu)

  END SUBROUTINE inputs_topography

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!
  
END MODULE Reading_Variables
