#!/bin/bash

# Script to run plenary.nvim tests
# Usage: ./run_tests.sh [test_file_pattern]

set -e

# Find nvim executable
NVIM_CMD="${NVIM_CMD:-nvim}"

# Test file pattern (default: all tests)
TEST_PATTERN="${1:-tests}"

# Run plenary tests
$NVIM_CMD --headless -c "PlenaryBustedDirectory $TEST_PATTERN { minimal_init = 'tests/minimal_init.lua' }" -c "qa"
