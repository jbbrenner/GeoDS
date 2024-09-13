MODULE WL_gridpoints_selection
  
  USE Parametrization

  IMPLICIT NONE

CONTAINS

    SUBROUTINE filling_WL_patterns_arrays(WL_pointers_array)

      IMPLICIT NONE

      TYPE(wlarr), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: WL_pointers_array


    END SUBROUTINE filling_WL_patterns_arrays

END MODULE WL_gridpoints_selection
