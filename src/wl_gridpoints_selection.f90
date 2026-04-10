MODULE WL_gridpoints_selection
  !Module creating the needed number of exposure arrays (one for each wind direction chosen by the user). The steps are 1) allocation of the arrays depending on the
  !Parametrization file specifications 2) filling of the arrays with wlelem items, each one associated with a gridpoints of influence for the given wind direction
  !
  
  USE Parametrization
  USE Support_functions, only: accessing_config_file

  IMPLICIT NONE

  INTEGER, PRIVATE :: ir, jr, m, p
  DOUBLE PRECISION, PRIVATE :: max_size_real !maximum size of the wind exposure arrays, built as a REAL variable
  INTEGER, PRIVATE :: max_size !maximum size of the wind exposure arrays, converted in INTEGER
  INTEGER, DIMENSION(:), ALLOCATABLE, PRIVATE :: counter

CONTAINS

    SUBROUTINE filling_WL_patterns_arrays(locvar__wl_pattern_pointers_array, locvar__wdir_angle_boundaries_array)

      IMPLICIT NONE

      TYPE(wl_pattern_arr), DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: locvar__wl_pattern_pointers_array
      DOUBLE PRECISION, DIMENSION(:), ALLOCATABLE, INTENT(INOUT) :: locvar__wdir_angle_boundaries_array

      !__________________________________________________________________!
      !Allocation and initialization of arrays
      !__________________________________________________________________!
      config_namelist_blockname="Downscaled_outputs"
      CALL accessing_config_file(ios, fu)

      ALLOCATE(TEI_pointers_array(nbr_wdir))
      IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
        ALLOCATE(TEI_drying_effect_correction(nbr_wdir))
      END IF
      DO p=1, nbr_wdir
        ALLOCATE(TEI_pointers_array(p)%tei_arr_ptr(1:hr_topo_x_size,1:hr_topo_y_size))
        IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
          ALLOCATE(TEI_drying_effect_correction(p)%tei_arr_ptr(1:hr_topo_x_size,1:hr_topo_y_size))
        END IF
      END DO

      DO p=1, nbr_wdir
        TEI_pointers_array(p)%tei_arr_ptr(:,:) = 0d0
        IF (broad_mountain_range_drying_effect_activator .EQV. .TRUE.) THEN
          TEI_drying_effect_correction(p)%tei_arr_ptr(:,:) = 0d0
        END IF
      END DO      

      ALLOCATE(counter(nbr_wdir))  !This counter is used to fill the different locvar__wl_pattern_pointers_arrays : it stores the index of the last added element to one of 
                                   !the nbr_wdir pointers arrays, and is necessary to add the next element of the same array using the correct index

      ALLOCATE(locvar__wdir_angle_boundaries_array(nbr_wdir + 1)) !Array containing the bounds of each angle interval associated with the nbr_wdir wind directions
     
      PRINT*, "nbr_wdir =", nbr_wdir
      PRINT*, 'TEI_windward_searching_dist =', TEI_windward_searching_dist
      PRINT*, 'drying_effect_windward_searching_dist =', drying_effect_windward_searching_dist
      max_size_real=CEILING(2*(drying_effect_windward_searching_dist/spatial_resolution + 2)**2)
      max_size=INT(max_size_real)
      PRINT*, 'max_size = ', max_size
 
      ALLOCATE(locvar__wl_pattern_pointers_array(nbr_wdir))                  !Creation of nbr_wdir arrays, whose pointers are stored in point_arrays : allows a dynamical declaration of variables
      DO m=1, nbr_wdir
         ALLOCATE(locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(max_size)) !Allocation of each of the pointed array using the max_size parameter : if nbr_wdir > 4, each array is sized to 
      END DO                                                         !be able to store a whole quadrant of grid points, in the extrem case where all of them influence the point the TEI
                                                                     !is being calculated
      !___________________________________________________________________!
      !Filling of arrays
      !___________________________________________________________________!

      counter(:) = 1

      !1/filling the locvar__wdir_angle_boundaries_array array : division of 2pi radians in nbr_wdir equal angle intervals. Each lower limit is an item of the array
      DO m=1, nbr_wdir + 1
         locvar__wdir_angle_boundaries_array(m) = (m-1) * (2*pi/nbr_wdir) - pi
         PRINT*, locvar__wdir_angle_boundaries_array(m)
      END DO

      DO m=1, nbr_wdir
         locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(:)%ix_relative=-999
         locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(:)%jy_relative=-999
         locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(:)%horizontal_dist=-999
      END DO

      !2/filling the arrays    
      !Using a double loop, the algorithm checks every gridpoints within a square box of 2*max_size size, and verifies several
      !conditions to associate the given point to the correct wind direction array. Note that a same point can be in two different
      !arrays, just not with the same weight

      DO ir=FLOOR(-drying_effect_windward_searching_dist/spatial_resolution), &
              CEILING(drying_effect_windward_searching_dist/spatial_resolution), 1 
         DO jr=FLOOR(-drying_effect_windward_searching_dist/spatial_resolution), &
                 CEILING(drying_effect_windward_searching_dist/spatial_resolution), 1

           !Checking only the points within the maximum research distance chosen by the user
           IF (SQRT((ir*spatial_resolution)**2 + (jr*spatial_resolution)**2) .LE. drying_effect_windward_searching_dist) THEN 
              DO m=1, nbr_wdir

                 !If the tested gridpoint is close enough, the angle between it and the center of the scheme is being tested,
                 !in order to associate the gridpoint to the correct wind direction array. The multiplication of i and j by 
                 !spatial_resolution allows to have real arguments for !the ATAN2 function, which do not work with integers
                 IF (ATAN2(jr*spatial_resolution,ir*spatial_resolution) .GE. locvar__wdir_angle_boundaries_array(m) & 
                      .AND. ATAN2(jr*spatial_resolution,ir*spatial_resolution) .LT. locvar__wdir_angle_boundaries_array(m+1)) THEN 
                    
                    !Once the gridpoint is associated to the right array, the necessary information to compute the TEI are stored :
                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%ix_relative = ir !the relative coordinates of the gridpoint i.e. the x-increment 
                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%jy_relative = jr !and y-increment
                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%horizontal_dist = SQRT((ir*spatial_resolution)**2 + &
                      (jr*spatial_resolution)**2) !the relative distance between the point the TEI is beeing calculated and the given gridpoint
                    !locvar__wl_pattern_pointers_array(m)%ptr(counter(m))%wdir_dist = SQRT((i*spatial_resolution)**2 + (j*spatial_resolution)**2) &
                     ! * COS(ATAN2(j*spatial_resolution,i*spatial_resolution) - locvar__wdir_angle_boundaries_array(m))
                     !IF (m .EQ. 1) THEN
                      !       PRINT *, "1st condition", ir, jr, ATAN2(jr*spatial_resolution,ir*spatial_resolution), m
                     !END IF
                    counter(m) = counter(m) + 1
!                 ELSE IF (ATAN2(jr*spatial_resolution,ir*spatial_resolution) .LT. locvar__wdir_angle_boundaries_array(m) &
!                     .AND. ABS(ATAN2(jr*spatial_resolution,ir*spatial_resolution)-locvar__wdir_angle_boundaries_array(m)) .LT. pi/4 &
!                      .AND. ABS((SQRT((ir*spatial_resolution)**2 + (jr*spatial_resolution)**2) &
!                      * SIN(locvar__wdir_angle_boundaries_array(m) - ATAN2(jr*spatial_resolution,ir*spatial_resolution)))) &
!                      .LT. spatial_resolution) THEN
!                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%ix_relative = ir 
!                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%jy_relative = jr
!                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%horizontal_dist = SQRT((ir*spatial_resolution)**2 + &
!                      (jr*spatial_resolution)**2) 
!                    counter(m) = counter(m) + 1
!                 !   !PRINT *, "2nd condition", i, j, ATAN2(j*spatial_resolution,i*spatial_resolution), m
!                 ELSE IF (ATAN2(jr*spatial_resolution,ir*spatial_resolution) .GE. locvar__wdir_angle_boundaries_array(m+1) &
!                      .AND. ABS(ATAN2(jr*spatial_resolution,ir*spatial_resolution)-locvar__wdir_angle_boundaries_array(m+1)) .LT. pi/4 &
!                      .AND. ABS((SQRT((ir*spatial_resolution)**2 + (jr*spatial_resolution)**2) &
!                      * SIN(ATAN2(jr*spatial_resolution,ir*spatial_resolution) - locvar__wdir_angle_boundaries_array(m+1)))) &
!                      .LT. spatial_resolution) THEN
!                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%ix_relative = ir 
!                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%jy_relative = jr
!                    locvar__wl_pattern_pointers_array(m)%wl_arr_ptr(counter(m))%horizontal_dist = SQRT((ir*spatial_resolution)**2 + &
!                      (jr*spatial_resolution)**2) 
!                    counter(m) = counter(m) + 1
                    !PRINT *, "3rd condition", i, j, ATAN2(j*spatial_resolution,i*spatial_resolution), m
                 END IF
              END DO
           ENDIF
        END DO
     END DO


!PRINT*, locvar__wl_pattern_pointers_array(1)%wl_arr_ptr
     !___________________________________________________________________!
     !The following loop is used to correct the previous conditions for the specific case of gridpoints located on the left portion of the x-absciss
     !ir=0
     !DO jr=FLOOR(-TEI_windward_searching_dist/spatial_resolution), -1
      !  locvar__wl_pattern_pointers_array(1)%wl_arr_ptr(counter(1))%ix_relative = ir 
       ! locvar__wl_pattern_pointers_array(1)%wl_arr_ptr(counter(1))%jy_relative = jr
        !locvar__wl_pattern_pointers_array(1)%wl_arr_ptr(counter(1))%horizontal_dist = SQRT((ir*spatial_resolution)**2 + &
       !             (jr*spatial_resolution)**2) 
        !counter(1) = counter(1) + 1
     !END DO
     !____________________________________________________________________!
    
      !___________________________________________________________________!
      !Deallocation of the private arrays
      !___________________________________________________________________!
      DEALLOCATE(counter)

    END SUBROUTINE filling_WL_patterns_arrays

END MODULE WL_gridpoints_selection
