#mpifort  NETCDF_UTILITIES.F90 pnetcdf_utils.f90 Decomp1D.F90 read_netcdf.f90 main.f90 -I/home/ec2-user/env_pnetcdf/include -lpnetcdf -lnetcdf -lnetcdff

# Define your Fortran compiler
FC = mpifort 

# Define the source file(s)
SRCS = NETCDF_UTILITIES.F90 pnetcdf_utils.f90 Decomp1D.F90 netcdf_utils.f90 read_netcdf.f90 main.f90 

# Define the executable name
EXEC = a.out 

# Define NetCDF library and include paths (replace with your actual paths)
INC = -I/home/ec2-user/env_pnetcdf/include  # Path to NetCDF Fortran module files
LIB = -L/home/ec2-user/env_pnetcdf/lib -lpnetcdf -lnetcdf -lnetcdff # Path to NetCDF libraries

# Define general compiler and linker flags
FFLAGS = -O2 -g  # Optimization and debugging flags
LDFLAGS =
#DFLAGS = -g -Wall -Wextra -pedantic -fimplicit-none -fbacktrace
# Default target: build the executable
all: $(EXEC)

$(EXEC): $(SRCS)
	$(FC) $(DFLAGS) $(FFLAGS) $(INC) $(SRCS) $(LIB) $(LDFLAGS) -o $(EXEC)

# Clean target: remove executable and object files
clean:
	rm -f $(EXEC) *.o *.mod a.out
