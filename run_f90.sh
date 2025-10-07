#!/bin/bash

# Find all .f90 files recursively
mapfile -t files < <(find . -type f -name "*.f90")

if [ ${#files[@]} -eq 0 ]; then
    echo "No .f90 files found."
    exit 1
fi

echo "Select a .f90 file:"
select file in "${files[@]}"; do
    if [[ -n "$file" ]]; then
        exe=$(mktemp)
        gfortran "$file" -o "$exe" && echo "Running $file..." && "$exe"
        rm -f "$exe"
        break
    else
        echo "Invalid choice."
    fi
done
