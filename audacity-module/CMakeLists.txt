cmake_minimum_required(VERSION 3.16)

project(mod-cloud-ai)

# Set C++ standard
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

# Find wxWidgets (Audacity's UI framework)
find_package(wxWidgets REQUIRED COMPONENTS core base net)
include(${wxWidgets_USE_FILE})

# Module source files
set(SOURCES
    ModCloudAI.cpp
    CloudAIEffect.cpp
    TtsDialog.cpp
    MusicDialog.cpp
    SoundEffectsDialog.cpp
    VoiceIsolationDialog.cpp
    TranscriptionDialog.cpp
    PythonBridge.cpp
)

set(HEADERS
    ModCloudAI.h
    CloudAIEffect.h
    TtsDialog.h
    MusicDialog.h
    SoundEffectsDialog.h
    VoiceIsolationDialog.h
    TranscriptionDialog.h
    PythonBridge.h
)

# Create the module library
add_library(mod-cloud-ai MODULE ${SOURCES} ${HEADERS})

# Link libraries
target_link_libraries(mod-cloud-ai
    ${wxWidgets_LIBRARIES}
    audacity-lib
)

# Include directories
target_include_directories(mod-cloud-ai PRIVATE
    ${CMAKE_SOURCE_DIR}/include
    ${wxWidgets_INCLUDE_DIRS}
)

# Platform-specific settings
if(WIN32)
    target_compile_definitions(mod-cloud-ai PRIVATE _UNICODE UNICODE)
    set_target_properties(mod-cloud-ai PROPERTIES SUFFIX ".dll")
elseif(APPLE)
    set_target_properties(mod-cloud-ai PROPERTIES SUFFIX ".dylib")
else()
    set_target_properties(mod-cloud-ai PROPERTIES SUFFIX ".so")
endif()

# Install target
install(TARGETS mod-cloud-ai
    LIBRARY DESTINATION ${CMAKE_INSTALL_PREFIX}/modules
    RUNTIME DESTINATION ${CMAKE_INSTALL_PREFIX}/modules
)
