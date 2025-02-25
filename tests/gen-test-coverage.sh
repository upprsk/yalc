#/usr/bin/env bash
#
# Should be run from root of repo

cd build/

# run the tests
cmake --build . -t test

# create report
lcov --directory . --capture --output-file coverage.info --filter brace
# remove exteral libraries
lcov --remove coverage.info '/usr/*' 'catch2*' 'fmt*' --output-file coverage.info

# generate html report
genhtml --demangle-cpp -o coverage coverage.info
