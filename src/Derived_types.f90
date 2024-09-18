MODULE Derived_types

  !___________________________________________________________________!

  IMPLICIT NONE
  PUBLIC
  
  !___________________________________________________________________!
  !Module containing the different derived types used in the algorithm
  !___________________________________________________________________!

  TYPE wl_pattern_elem
     INTEGER :: ix !Relative x-coordinate of a given element stored in the associated k-wind exposure array
     INTEGER :: jy !Relative y-coordinate of a given element stored in the associated k-wind exposure array
     REAL :: horizontal_dist !Distance between the given k-wind exposure array's element and the grid point whose WL index is being calculated
  END TYPE wl_pattern_elem


  TYPE wl_pattern_arr
     TYPE(wl_pattern_elem), DIMENSION(:), POINTER :: wl_arr_ptr !Pointer associated with an 1D-array containing WL_elements, i.e. an array containing the general
  END TYPE wl_pattern_arr                                       !pattern to find each cells influencing the WL_index of an undifined grid points, for a given k
                                                       !wind direction.


  TYPE tei_arr
     DOUBLE PRECISION, DIMENSION(:,:), POINTER :: tei_arr_ptr
  END TYPE tei_arr
  !___________________________________________________________________!
  
END MODULE Derived_types
