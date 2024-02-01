MODULE Writing_Outputs
  !________________________________________________________________________________________!

  USE Parametrization, only: Configuration_file
  USE ncio, only: nc_create

  !________________________________________________________________________________________! 

  IMPLICIT NONE

  !________________________________________________________________________________________!
  !________________________________________________________________________________________!

CONTAINS

  SUBROUTINE downscaled_outputs_writing(ds_temperature_file)

    IMPLICIT NONE
    
    
    CHARACTER (LEN=256), INTENT(INOUT) :: ds_temperature_file
    
    INTEGER :: ios,fu
    LOGICAL :: input_checking
    
    NAMELIST/Downscaled_outputs/ds_temperature_file

    OPEN (NEWUNIT=fu, ACTION='READ', FILE=Configuration_file, IOSTAT=ios)
        IF (ios /= 0) THEN
            WRITE (*, *)"Error:",Configuration_file,"could not be opened"
         END IF
    READ (UNIT=fu, NML=Downscaled_outputs, IOSTAT=ios)
      
    CALL nc_create(ds_temperature_file, overwrite=.TRUE.,netcdf4=.TRUE.)

    CLOSE(fu)
    
    
  END SUBROUTINE downscaled_outputs_writing

END MODULE Writing_Outputs
