function(add_mocked_library libraryName)
    get_target_property(sourceFiles ${libraryName} SOURCES)
    get_target_property(linkLibs ${libraryName} LINK_LIBRARIES)
    get_target_property(includeDirs ${libraryName} INCLUDE_DIRECTORIES)
    get_target_property(compileDefinitions ${libraryName} COMPILE_DEFINITIONS)
    get_target_property(compileOptions ${libraryName} COMPILE_OPTIONS)

    add_library(${libraryName}_COVERAGE
        ${sourceFiles}
    )

    target_include_directories(${libraryName}_COVERAGE PUBLIC
        ${includeDirs}
    )

    target_link_libraries(${libraryName}_COVERAGE PUBLIC
        ${linkLibs}
    )

    if (compileOptions)
        target_compile_options(${libraryName}_COVERAGE PRIVATE
            ${compileOptions}
        )
    endif()

    if (compileDefinitions)
        target_compile_definitions(${libraryName}_COVERAGE PRIVATE
            ${compileDefinitions}
        )
    endif()

    target_compile_options(${libraryName}_COVERAGE PRIVATE
        -coverage
        -O0
    )
endfunction()
