cmake_minimum_required(VERSION 2.8)

project(CWang)
enable_language(Fortran)

set(CMAKE_BUILD_TYPE RelWithDebInfo)
set(CMAKE_Fortran_COMPILER mpif90)
set(CMAKE_C_COMPILER mpicc)
set(CMAKE_Fortran_FLAGS "-cpp -O3 -g -DBAD_ZDU=1 -DDRAW_SHOW=1 -DDRAW_EVINCE=0")

file(GLOB SRC_FILE "source/*/source/*")

add_library(pack ${SRC_FILE})

