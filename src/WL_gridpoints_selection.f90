MODULE WL_gridpoints_selection
  !Module creating the needed number of exposure arrays (one for each wind direction chosen by the user). The steps are 1) allocation of the arrays depending on the
  !Parametrization file specifications 2) filling of the arrays with wlelem items, each one associated with a gridpoints of influence for the given wind direction
  !
  
  USE Parametrization
  USE Support_functions, only: accessing_config_file
  IMPLICIT NONE

  INTEGER, PRIVATE :: i, j, k, m
  DOUBLE PRECISION, PRIVATE :: max_size !maximum size of the wind exposure arrays 
  INTEGER, DIMENSION(:), ALLOCATABLE, PRIVATE :: counter

CONTAINS

    SUBROUTINE filling_WL_patterns_arrays(WL_pointers_array, wdir_angle_boundaries, config_namelist_blockname, ios, fu)

      IMPLICIT NONE

      TYPE(wlarr), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: WL_pointers_array
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: wdir_angle_boundaries
      CHARACTER(LEN=str_len), INTENT(OUT) :: config_namelist_blockname
      INTEGER, INTENT(INOUT) :: ios, fu

      !__________________________________________________________________!
      !Reading TEI related information stored in the Configuration file
      !__________________________________________________________________!
      PRINT*, "____________________________________"
      PRINT*, "Topographic exposure indexes computation"
      config_namelist_blockname="Downscaled_outputs"
      CALL accessing_config_file(ios, fu)

      !__________________________________________________________________!
      !Allocation and initialization of arrays
      !__________________________________________________________________!
      
      ALLOCATE(counter(nbr_wdir))  !This counter is used to fill the different WL_pointers_arrays : it stores the index of the last added element to one of 
                                   !the nbr_wdir pointers arrays, and is necessary to add the next element of the same array using the correct index

      ALLOCATE(wdir_angle_boundaries(nbr_wdir + 1)) !Array containing the bounds of each angle interval associated with the nbr_wdir wind directions
     
      PRINT*, "nbr_wdir =", nbr_wdir
      PRINT*, 'd_wsearch =', d_wsearch
      max_size=(d_wsearch/spatial_resolution)**2
      PRINT*, 'max_size = ', max_size
 
      ALLOCATE(WL_pointers_array(nbr_wdir)) !Creation of nbr_wdir arrays, whose pointers are stored in WL_pointers_arrays : allows a dynamical declaration of variables
      DO k=1, nbr_wdir
         ALLOCATE(WL_pointers_array(k)%ptr(max_size)) !Allocation of each of the pointed array using the max_size parameter : if nbr_wdir > 4, each array is sized to 
      END DO                                          !be able to store a whole quadrant of grid points, in the extrem case where all of them influence the point the TEI
                                                      !is being calculated
      DO k=1, nbr_wdir
         DO i=1, CEILING(max_size)
            WL_pointers_array(k)%ptr(i)%ix = 1
            WL_pointers_array(k)%ptr(i)%jy = 2
         END DO
      END DO

      !___________________________________________________________________!
      !Filling of arrays
      !___________________________________________________________________!

      counter(:) = 0
      !1/filling the wdir_angle_boundaries array : division of 2pi radians in nbr_wdir equal angle intervals. Each lower limit is an item of the array
      DO m=1, nbr_wdir + 1
         wdir_angle_boundaries(m) = (m-1) * (2*pi/nbr_wdir)
         !PRINT*, wdir_angle_boundaries(m)
      END DO

      !2/filling the arrays    
      

      !___________________________________________________________________!
      !Deallocation of the arrays : main file
      !___________________________________________________________________!

      
      DO k=1, nbr_wdir
         DEALLOCATE(WL_pointers_array(k)%ptr)
      END DO
      DEALLOCATE(WL_pointers_array)
      DEALLOCATE(counter)
      DEALLOCATE(wdir_angle_boundaries)
        
      PRINT*, "___________________________________"
    END SUBROUTINE filling_WL_patterns_arrays

END MODULE WL_gridpoints_selection
