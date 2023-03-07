#!/bin/bash

# llc -filetype=obj output.ll -o output.o
# clang output.o && ./a.out
# rm output.o a.out
lli output.ll
