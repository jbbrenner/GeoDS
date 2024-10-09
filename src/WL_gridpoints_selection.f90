MODULE WL_gridpoints_selection
  !Module creating the needed number of exposure arrays (one for each wind direction chosen by the user). The steps are 1) allocation of the arrays depending on the
  !Parametrization file specifications 2) filling of the arrays with wlelem items, each one associated with a gridpoints of influence for the given wind direction
  !
  
  USE Parametrization
  USE Support_functions, only: accessing_config_file

  IMPLICIT NONE

  INTEGER, PRIVATE :: ir, jr, m
  DOUBLE PRECISION, PRIVATE :: max_size_real !maximum size of the wind exposure arrays, built as a REAL variable
  INTEGER, PRIVATE :: max_size !maximum size of the wind exposure arrays, converted in INTEGER
  INTEGER, DIMENSION(:), ALLOCATABLE, PRIVATE :: counter

CONTAINS

    SUBROUTINE filling_WL_patterns_arrays(WL_pattern_pointers_array, wdir_angle_boundaries)

      IMPLICIT NONE

      TYPE(wl_pattern_arr), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: WL_pattern_pointers_array
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: wdir_angle_boundaries

      !__________________________________________________________________!
      !Allocation and initialization of arrays
      !__________________________________________________________________!
      
      ALLOCATE(counter(nbr_wdir))  !This counter is used to fill the different WL_pattern_pointers_arrays : it stores the index of the last added element to one of 
                                   !the nbr_wdir pointers arrays, and is necessary to add the next element of the same array using the correct index

      ALLOCATE(wdir_angle_boundaries(nbr_wdir + 1)) !Array containing the bounds of each angle interval associated with the nbr_wdir wind directions
     
      PRINT*, "nbr_wdir =", nbr_wdir
      PRINT*, 'd_wsearch =', d_wsearch
      max_size_real=CEILING(2*(d_wsearch/spatial_resolution + 2)**2)
      max_size=INT(max_size_real)
      PRINT*, 'max_size = ', max_size
 
      ALLOCATE(WL_pattern_pointers_array(nbr_wdir)) !Creation of nbr_wdir arrays, whose pointers are stored in WL_pattern_pointers_arrays : allows a dynamical declaration of variables
      DO m=1, nbr_wdir
         ALLOCATE(WL_pattern_pointers_array(m)%wl_arr_ptr(max_size)) !Allocation of each of the pointed array using the max_size parameter : if nbr_wdir > 4, each array is sized to 
      END DO                                          !be able to store a whole quadrant of grid points, in the extrem case where all of them influence the point the TEI
                                                      !is being calculated
      !___________________________________________________________________!
      !Filling of arrays
      !___________________________________________________________________!

      counter(:) = 1
      !1/filling the wdir_angle_boundaries array : division of 2pi radians in nbr_wdir equal angle intervals. Each lower limit is an item of the array
      DO m=1, nbr_wdir + 1
         wdir_angle_boundaries(m) = (m-1) * (2*pi/nbr_wdir) - pi
         PRINT*, wdir_angle_boundaries(m)
      END DO

      DO m=1, nbr_wdir
         WL_pattern_pointers_array(m)%wl_arr_ptr(:)%ix_relative=-9999
         WL_pattern_pointers_array(m)%wl_arr_ptr(:)%jy_relative=-9999
         WL_pattern_pointers_array(m)%wl_arr_ptr(:)%horizontal_dist=-9999
      END DO

      !2/filling the arrays    
      !Using a double loop, the algorithm checks every gridpoints within a square box of 2*max_size size, and verifies several
      !conditions to associate the given point to the correct wind direction array. Note that a same point can be in two different
      !arrays, just not with the same weight

      DO ir=FLOOR(-d_wsearch/spatial_resolution), CEILING(d_wsearch/spatial_resolution), 1 
         DO jr=FLOOR(-d_wsearch/spatial_resolution), CEILING(d_wsearch/spatial_resolution), 1
           IF (SQRT((ir*spatial_resolution)**2 + (jr*spatial_resolution)**2) .LE. d_wsearch) THEN !Checking only the points within the maximum research distance chosen by the user
              DO m=1, nbr_wdir
                 IF (ATAN2(jr*spatial_resolution,ir*spatial_resolution) .GE. wdir_angle_boundaries(m) & !If the tested gridpoint is close enough, the angle between it and the center of the scheme is being tested, in order to associate the gridpoint to the correct wind direction array. The multiplication of i and j by spatial_resolution allows to have real arguments for !the ATAN2 function, which do not work with integers
                      .AND. ATAN2(jr*spatial_resolution,ir*spatial_resolution) .LT. wdir_angle_boundaries(m+1)) THEN !Once the gridpoint is associated to the right array, the necessary information to compute the TEI are stored :
                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%ix_relative = ir !the relative coordinates of the gridpoint i.e. the x-increment 
                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%jy_relative = jr !and y-increment
                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%horizontal_dist = SQRT((ir*spatial_resolution)**2 + &
                      (jr*spatial_resolution)**2) !the relative distance between the point the TEI is beeing calculated and the given gridpoint
                    !WL_pattern_pointers_array(m)%ptr(counter(m))%wdir_dist = SQRT((i*spatial_resolution)**2 + (j*spatial_resolution)**2) &
                     ! * COS(ATAN2(j*spatial_resolution,i*spatial_resolution) - wdir_angle_boundaries(m))
                     !IF (m .EQ. 1) THEN
                      !       PRINT *, "1st condition", ir, jr, ATAN2(jr*spatial_resolution,ir*spatial_resolution), m
                     !END IF
                    counter(m) = counter(m) + 1
!                 ELSE IF (ATAN2(jr*spatial_resolution,ir*spatial_resolution) .LT. wdir_angle_boundaries(m) &
!                     .AND. ABS(ATAN2(jr*spatial_resolution,ir*spatial_resolution)-wdir_angle_boundaries(m)) .LT. pi/4 &
!                      .AND. ABS((SQRT((ir*spatial_resolution)**2 + (jr*spatial_resolution)**2) &
!                      * SIN(wdir_angle_boundaries(m) - ATAN2(jr*spatial_resolution,ir*spatial_resolution)))) &
!                      .LT. spatial_resolution) THEN
!                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%ix_relative = ir 
!                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%jy_relative = jr
!                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%horizontal_dist = SQRT((ir*spatial_resolution)**2 + &
!                      (jr*spatial_resolution)**2) 
!                    counter(m) = counter(m) + 1
!                 !   !PRINT *, "2nd condition", i, j, ATAN2(j*spatial_resolution,i*spatial_resolution), m
!                 ELSE IF (ATAN2(jr*spatial_resolution,ir*spatial_resolution) .GE. wdir_angle_boundaries(m+1) &
!                      .AND. ABS(ATAN2(jr*spatial_resolution,ir*spatial_resolution)-wdir_angle_boundaries(m+1)) .LT. pi/4 &
!                      .AND. ABS((SQRT((ir*spatial_resolution)**2 + (jr*spatial_resolution)**2) &
!                      * SIN(ATAN2(jr*spatial_resolution,ir*spatial_resolution) - wdir_angle_boundaries(m+1)))) &
!                      .LT. spatial_resolution) THEN
!                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%ix_relative = ir 
!                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%jy_relative = jr
!                    WL_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%horizontal_dist = SQRT((ir*spatial_resolution)**2 + &
!                      (jr*spatial_resolution)**2) 
!                    counter(m) = counter(m) + 1
                    !PRINT *, "3rd condition", i, j, ATAN2(j*spatial_resolution,i*spatial_resolution), m
                 END IF
              END DO
           ENDIF
        END DO
     END DO
     !___________________________________________________________________!
     !The following loop is used to correct the previous conditions for the specific case of gridpoints located on the left portion of the x-absciss
     !ir=0
     !DO jr=FLOOR(-d_wsearch/spatial_resolution), -1
      !  WL_pattern_pointers_array(1)%wl_arr_ptr(counter(1))%ix_relative = ir 
       ! WL_pattern_pointers_array(1)%wl_arr_ptr(counter(1))%jy_relative = jr
        !WL_pattern_pointers_array(1)%wl_arr_ptr(counter(1))%horizontal_dist = SQRT((ir*spatial_resolution)**2 + &
       !             (jr*spatial_resolution)**2) 
        !counter(1) = counter(1) + 1
     !END DO
     !____________________________________________________________________!
    
      !___________________________________________________________________!
      !Deallocation of the private arrays
      !___________________________________________________________________!
      DEALLOCATE(counter)


     ! DO jr=1, SIZE(WL_pattern_pointers_array(1)%wl_arr_ptr)
      !             PRINT*, WL_pattern_pointers_array(1)%wl_arr_ptr(jr)%ix_relative,";",&
       !           WL_pattern_pointers_array(1)%wl_arr_ptr(jr)%jy_relative,";",&
        !          WL_pattern_pointers_array(1)%wl_arr_ptr(jr)%horizontal_dist
       !END DO


    END SUBROUTINE filling_WL_patterns_arrays

END MODULE WL_gridpoints_selection
