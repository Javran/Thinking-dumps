include_directories(${CMAKE_CURRENT_SOURCE_DIR}/..)

# If pkg-config is not installed on the system, then the
# CHECK_INSTALL_DIR variable must be set to the install
# location of Check. For example, on Windows, this may
# be: C:/Program Files/check
# set(CHECK_INSTALL_DIR "C:/Program Files/check")

find_package(Check REQUIRED)
include_directories(${CHECK_INCLUDE_DIRS})

file(GLOB run_checks_SRC "*.h" "*.c")
add_executable(run_checks ${run_checks_SRC})

target_link_libraries(run_checks ${CHECK_LIBRARIES})
