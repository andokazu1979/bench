# FC: Fortran90 compiler command
FC=mpif90

# FFLAGS: Compiler options
FFLAGS=

# EXEC: Executable
EXEC=pingpong

# OBJS: object files to link
OBJS=pingpong.o

$(EXEC): $(OBJS)
	$(FC) -o $@ $(FFLAGS) $^

%.o: %.f90
	$(FC) -c $(FFLAGS) $^

clean:
	rm -f *.o $(EXEC)
