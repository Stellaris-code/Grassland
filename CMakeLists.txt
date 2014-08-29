project(Grassland)
cmake_minimum_required(VERSION 2.8)
aux_source_directory(. SRC_LIST)

include_directories(./include)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_modules" ${CMAKE_MODULE_PATH})


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

file(GLOB_RECURSE CPP_FILES ./src/*.cpp)
file(GLOB_RECURSE HPP_FILES ./include/*.hpp)

find_package(SFML REQUIRED system window graphics)

add_executable(${PROJECT_NAME} ${CPP_LIST} ${HPP_LIST} src/main.cpp)


target_link_libraries(${PROJECT_NAME} ${SFML_LIBRARIES})

