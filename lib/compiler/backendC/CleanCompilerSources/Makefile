# This is for Unix
CC = gcc
CFLAGS = -D_SUN_ -DGNU_C -O -fomit-frame-pointer
AR = ar
RANLIB = ranlib

OBJECTS = \
	backend.o backendsupport.o buildtree.o checker_2.o checksupport.o \
	cocl.o codegen1.o codegen2.o codegen3.o codegen.o comparser_2.o \
	compiler.o comsupport.o dbprint.o instructions.o optimisations.o \
	pattern_match_2.o result_state_database.o sa.o scanner_2.o \
	set_scope_numbers.o settings.o unix_io.o statesgen.o tcsupport_2.o \
	typeconv_2.o version.o

backend.a: $(OBJECTS)
	$(AR) cur backend.a $(OBJECTS)
	$(RANLIB) backend.a
