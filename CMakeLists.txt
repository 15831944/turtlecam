CMAKE_MINIMUM_REQUIRED(VERSION 2.6)
PROJECT(turtlecam)

SET(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -Wextra -std=c++11")

ADD_EXECUTABLE(turtlecam turtlecam.cpp)
