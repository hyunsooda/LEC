#!/bin/bash

/home/hyunsoo/haskell-tutorial/llvm/llvm-project/build/bin/clang-15 -g -S -emit-llvm const.c; mv const.ll ../
/home/hyunsoo/haskell-tutorial/llvm/llvm-project/build/bin/clang-15 -g -S -emit-llvm int.c; mv int.ll ../
