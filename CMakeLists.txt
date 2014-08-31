project(Grassland)
cmake_minimum_required(VERSION 2.8)
aux_source_directory(. SRC_LIST)

include_directories("./include")

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake_modules" ${CMAKE_MODULE_PATH})


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++1y")

file(GLOB_RECURSE CPP_FILES ./src/*.cpp)
file(GLOB_RECURSE HPP_FILES ./include/*.hpp)

find_package(SFML REQUIRED system window graphics audio)
find_package(Thor REQUIRED)
find_package(BOX2D REQUIRED)


include_directories("/home/jim/sfml-tmxloader/include")

link_directories("/home/jim/sfml-tmxloader")

add_executable(${PROJECT_NAME} ${CPP_LIST} ${HPP_LIST} src/main.cpp)



target_link_libraries(${PROJECT_NAME} tmx-loader)
target_link_libraries(${PROJECT_NAME} pugi)


target_link_libraries(${PROJECT_NAME} ${SFML_LIBRARIES})


target_link_libraries(${PROJECT_NAME} ${BOX2D_LIBRARIES})

target_link_libraries(${PROJECT_NAME} ${THOR_LIBRARY})



