all: GeoDS.x

DIR = src

include make.macros

ifneq ("$(wildcard sources/mod_iloveclim_setup.F)","")
    BIGF_FILES = 1
endif

# Suffix-rules:  Begin by throwing away all old suffix-
# rules, and then create new ones for compiling
# *.f90-files.
.SUFFIXES:
.SUFFIXES: .f90 .o
.SUFFIXES: .f08 .o
.SUFFIXES: .f .o
.SUFFIXES: .F .o

# Compilation rules
$(DIR)/%.o: $(DIR)/%.f90
	$(FC) $(F_FLAGS_EXTD) $(FFLAGS) ${INCLUDES} -c $(DIR)/$*.f90 -o $(DIR)/$*.o

$(DIR)/%.o: $(DIR)/%.f08
	$(FC) $(F_STRCT_EIGHT) $(F_FLAGS_EXTD) $(FFLAGS) ${INCLUDES} -c ${F_TREAT_FORTR} $(DIR)/$*.f08 -o $(DIR)/$*.o

$(DIR)/%.o: $(DIR)/%.f
	$(FC) $(F_FLAGS_CARD) $(FFLAGS) ${INCLUDES} -c $(DIR)/$*.f -o $(DIR)/$*.o

$(DIR)/%.o: $(DIR)/%.F
	$(FC) $(F_FLAGS_CARD) $(FFLAGS) ${INCLUDES} -c $(DIR)/$*.F -o $(DIR)/$*.o

# Include the dependency-list created by makedepf90 below
include .depend

# target 'clean' for deleting object- *.mod- and other
# unwanted files

clean:
	rm -f *.mod $(DIR)/*.o *genmod* GeoDS.x .depend

strict: clean GeoDS.x

# Create a dependency list using makedepf90.  All files
# that needs to be compiled to build the program,
# i.e all source files except include files, should
# be given on the command line to makedepf90.
#
# The argument to the '-o' option will be the name of the
# resulting program when running 'make'

depend .depend:
		$(FMAKEDEPEND) -o GeoDS.x $(DIR)/*.f* > .depend

# The compilation line of makedepf90 is by default:
#   3 emic.x: $(FOBJ)
#   4 →       $(FC) -o $@ $(FFLAGS) $(LDFLAGS) $(FOBJ) $(LIBS)

# The End of All Things (op. cit.)
