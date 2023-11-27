add_library(cpp_folder_tree STATIC cpp_folder_tree.cpp)
add_library(find_last_addr STATIC find_last_addr.cpp)
add_library(enable_gdb_attach STATIC enable_gdb_attach.cpp)

target_link_libraries(ALaDyn PRIVATE cpp_folder_tree find_last_addr
                                     enable_gdb_attach)

option(USE_GDB_ATTACH "Enable GDB attach to debug while running" OFF)

if(NOT WIN32 AND USE_GDB_ATTACH)
    target_compile_definitions(enable_gdb_attach PUBLIC USE_GDB_ATTACH)
endif()

if(NOT WIN32)
    find_package(Boost COMPONENTS system filesystem)
    if(Boost_FOUND)
        target_compile_definitions(cpp_folder_tree PUBLIC USE_BOOST)
        target_include_directories(cpp_folder_tree PUBLIC ${Boost_INCLUDE_DIR})
        target_link_libraries(cpp_folder_tree PUBLIC ${Boost_LIBRARIES})
    else()
        target_compile_definitions(cpp_folder_tree PUBLIC USE_FILESYSTEM)
    endif()
endif()
