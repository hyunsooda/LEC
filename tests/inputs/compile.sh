#!/bin/bash

dirs_cmd=`ls -d */`
compile_flag="-Wno-everything -g -S -emit-llvm"

dirs=($dirs_cmd)
for dir in "${dirs[@]}"; do
    pushd $dir

    cFiles=`ls *.c`
    cc=($cFiles)
    cppFiles=`ls *.cpp`
    cpp=($cppFiles)

    for file in "${cc[@]}"; do
        clang $compile_flag $file
    done
    for file in "${cpp[@]}"; do
        clang++ $compile_flag $file
    done

    popd
done
