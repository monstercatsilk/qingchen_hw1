cmake_minimum_required(VERSION 3.5)
project(MatrixCalculator)

add_executable(matrix_calculator
    src/main.c
    src/algebra.c
)

target_include_directories(matrix_calculator PRIVATE inc)

target_link_libraries(matrix_calculator m)