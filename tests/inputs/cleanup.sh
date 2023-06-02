#!/bin/bash

dirs_cmd=`ls -d */`
dirs=($dirs_cmd)
for dir in "${dirs[@]}"; do
    pushd $dir
    rm -f *.ll
    popd
done
