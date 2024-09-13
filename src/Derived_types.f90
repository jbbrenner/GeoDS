MODULE Derived_types

  !___________________________________________________________________!

  IMPLICIT NONE
  PUBLIC
  
  !___________________________________________________________________!
  !Module containing the different derived types used in the algorithm
  !___________________________________________________________________!

  TYPE wlelem
     INTEGER :: ix !Relative x-coordinate of a given element stored in the associated k-wind exposure array
     INTEGER :: jy !Relative y-coordinate of a given element stored in the associated k-wind exposure array
     REAL :: hdist !Distance between the given k-wind exposure array's element and the grid point whose WL index is being calculated
  END TYPE wlelem


  TYPE wlarr
     TYPE(wlelem), DIMENSION(:), POINTER :: ptr !Pointer associated with an 1D-array containing WL_elements, i.e. an array containing the general
  END TYPE wlarr                                 !pattern to find each cells influencing the WL_index of an undifined grid points, for a given k
                                                    !wind direction.

  !___________________________________________________________________!
  
END MODULE Derived_types
