#!/bin/bash

clang-15 -g -S -emit-llvm const.c
clang-15 -g -S -emit-llvm int.c
clang-15 -g -S -emit-llvm static-arr.c
clang-15 -g -S -emit-llvm static-arr2.c
