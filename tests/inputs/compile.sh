#!/bin/bash

dirs_cmd=`ls -d */`
dirs=($dirs_cmd)
for dir in "${dirs[@]}"; do
    pushd $dir

    cFiles=`ls *.c`
    cc=($cFiles)
    cppFiles=`ls *.cpp`
    cpp=($cppFiles)

    for file in "${cc[@]}"; do
        clang -g -S -emit-llvm $file
    done
    for file in "${cpp[@]}"; do
        clang++ -g -S -emit-llvm $file
    done

    popd
done
