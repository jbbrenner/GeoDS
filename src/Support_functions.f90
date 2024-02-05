MODULE Support_functions

  !This module contains subsidiary subroutines used to automate certain repetitive and heavy
  !writing tasks.   
  !________________________________________________________________________________________!

  USE Parametrization, only: Configuration_file

  !________________________________________________________________________________________! 

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

CONTAINS

  !________________________________________________________________________________________!
  !Subroutine used to automate access to configuration file. For each new blockname or variable written in the file,
  !functions arguments have to be updated
  !________________________________________________________________________________________!

  SUBROUTINE config_file_access(config_namelist_blockname, LR_temperature_file, LR_surface_temperature_id,lrtemp_x_size,&
         lrtemp_y_size,lrtemp_t_size, HR_elevation_file, HR_surface_elevation_id,hrtopo_x_size,&
       hrtopo_y_size,hrtopo_t_size, downscaled_climate_data_file, ios, fu)

    IMPLICIT NONE

    CHARACTER(LEN=100), INTENT(INOUT) :: config_namelist_blockname
    
    CHARACTER (LEN=256), INTENT(INOUT) :: LR_temperature_file
    CHARACTER (LEN=20), INTENT(INOUT) :: LR_surface_temperature_id
    INTEGER, INTENT(INOUT) :: lrtemp_x_size 
    INTEGER, INTENT(INOUT) :: lrtemp_y_size
    INTEGER, INTENT(INOUT) :: lrtemp_t_size

    CHARACTER (LEN=256), INTENT(INOUT) :: HR_elevation_file
    CHARACTER (LEN=20), INTENT(INOUT) :: HR_surface_elevation_id
    INTEGER, INTENT(INOUT) :: hrtopo_x_size 
    INTEGER, INTENT(INOUT) :: hrtopo_y_size
    INTEGER, INTENT(INOUT) :: hrtopo_t_size
    
    CHARACTER (LEN=256), INTENT(INOUT) :: downscaled_climate_data_file

    
    INTEGER,INTENT(INOUT) :: ios,fu
    
    LOGICAL :: input_checking

    NAMELIST/Temperature/LR_temperature_file, LR_surface_temperature_id,lrtemp_x_size,&
         lrtemp_y_size,lrtemp_t_size
    NAMELIST/Topography/HR_elevation_file, HR_surface_elevation_id,hrtopo_x_size,&
       hrtopo_y_size,hrtopo_t_size
    NAMELIST/Downscaled_outputs/downscaled_climate_data_file


    !Checking whether the configuration file exists or not
    INQUIRE (file=Configuration_file, EXIST=input_checking)
    IF (input_checking .eqv. (.FALSE.)) THEN
       WRITE (*, *)"Error: input file", Configuration_file, "does not exist"
    END IF

    !Opening configuration file and reading variables depending on the block explored
    !by the calling subroutine
    SELECT CASE(config_namelist_blockname)
    CASE ("Temperature")
       OPEN (NEWUNIT=fu, ACTION='READ', FILE=Configuration_file, IOSTAT=ios)
       IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file,"could not be opened"
       END IF
       READ (UNIT=fu, NML=Temperature, IOSTAT=ios)
       PRINT*, "Configuration file : accessing temperature-related variables"
       
    CASE ("Topography")
       OPEN (NEWUNIT=fu, ACTION='READ', FILE=Configuration_file, IOSTAT=ios)
       IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file,"could not be opened"
       END IF
       READ (UNIT=fu, NML=Topography, IOSTAT=ios)
       PRINT*, "Configuration file : accessing topography-related variables"
       
    CASE ("Downscaled_outputs")
       OPEN (NEWUNIT=fu, ACTION='READ', FILE=Configuration_file, IOSTAT=ios)
       IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file,"could not be opened"
       END IF
       READ (UNIT=fu, NML=Downscaled_outputs, IOSTAT=ios)
       PRINT*, "Configuration file : accessing outputs-related variables"

    CASE DEFAULT
       Print *,"No namelist selected"
       
    END SELECT
           
  END SUBROUTINE config_file_access

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!
  
END MODULE Support_functions
