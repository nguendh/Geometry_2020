# CMAKE generated file: DO NOT EDIT!
# Generated by "MinGW Makefiles" Generator, CMake Version 3.20

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Disable VCS-based implicit rules.
% : %,v

# Disable VCS-based implicit rules.
% : RCS/%

# Disable VCS-based implicit rules.
% : RCS/%,v

# Disable VCS-based implicit rules.
% : SCCS/s.%

# Disable VCS-based implicit rules.
% : s.%

.SUFFIXES: .hpux_make_needs_suffix_list

# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

#Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

SHELL = cmd.exe

# The CMake executable.
CMAKE_COMMAND = "C:\Program Files\JetBrains\CLion 2021.2.3\bin\cmake\win\bin\cmake.exe"

# The command to remove a file.
RM = "C:\Program Files\JetBrains\CLion 2021.2.3\bin\cmake\win\bin\cmake.exe" -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = D:\Windows.old\Users\pc\CLionProjects\geometry

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = D:\Windows.old\Users\pc\CLionProjects\geometry\cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/geometry.dir/depend.make
# Include the progress variables for this target.
include CMakeFiles/geometry.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/geometry.dir/flags.make

CMakeFiles/geometry.dir/main.cpp.obj: CMakeFiles/geometry.dir/flags.make
CMakeFiles/geometry.dir/main.cpp.obj: ../main.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=D:\Windows.old\Users\pc\CLionProjects\geometry\cmake-build-debug\CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/geometry.dir/main.cpp.obj"
	C:\MinGW\bin\g++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles\geometry.dir\main.cpp.obj -c D:\Windows.old\Users\pc\CLionProjects\geometry\main.cpp

CMakeFiles/geometry.dir/main.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/geometry.dir/main.cpp.i"
	C:\MinGW\bin\g++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E D:\Windows.old\Users\pc\CLionProjects\geometry\main.cpp > CMakeFiles\geometry.dir\main.cpp.i

CMakeFiles/geometry.dir/main.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/geometry.dir/main.cpp.s"
	C:\MinGW\bin\g++.exe $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S D:\Windows.old\Users\pc\CLionProjects\geometry\main.cpp -o CMakeFiles\geometry.dir\main.cpp.s

# Object files for target geometry
geometry_OBJECTS = \
"CMakeFiles/geometry.dir/main.cpp.obj"

# External object files for target geometry
geometry_EXTERNAL_OBJECTS =

geometry.exe: CMakeFiles/geometry.dir/main.cpp.obj
geometry.exe: CMakeFiles/geometry.dir/build.make
geometry.exe: CMakeFiles/geometry.dir/linklibs.rsp
geometry.exe: CMakeFiles/geometry.dir/objects1.rsp
geometry.exe: CMakeFiles/geometry.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=D:\Windows.old\Users\pc\CLionProjects\geometry\cmake-build-debug\CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable geometry.exe"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles\geometry.dir\link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/geometry.dir/build: geometry.exe
.PHONY : CMakeFiles/geometry.dir/build

CMakeFiles/geometry.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles\geometry.dir\cmake_clean.cmake
.PHONY : CMakeFiles/geometry.dir/clean

CMakeFiles/geometry.dir/depend:
	$(CMAKE_COMMAND) -E cmake_depends "MinGW Makefiles" D:\Windows.old\Users\pc\CLionProjects\geometry D:\Windows.old\Users\pc\CLionProjects\geometry D:\Windows.old\Users\pc\CLionProjects\geometry\cmake-build-debug D:\Windows.old\Users\pc\CLionProjects\geometry\cmake-build-debug D:\Windows.old\Users\pc\CLionProjects\geometry\cmake-build-debug\CMakeFiles\geometry.dir\DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/geometry.dir/depend

