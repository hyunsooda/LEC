#!/bin/bash

set -e

input=$1
output=$2

function modify_lec_global_string() {
    LEC_DEFINED_ANSI_RED_GLOBAL_VAR='LEC_ANSI_RED_g767akzwihq04k3frbvijvx2l5gdn0sk'
    LEC_DEFINED_ANSI_WHITE_GLOBAL_VAR='LEC_ANSI_WHITE_g767akzwihq04k3frbvijvx2l5gdn0sk'

    ANSI_RED='1B[31m'
    ANSI_WHITE='1B[37m'

    # Ref: https://superuser.com/questions/590630/sed-how-to-replace-line-if-found-or-append-to-end-of-file-if-not-found
    RED_COLOR="unnamed_addr constant [6 x i8] c\""\\\\$ANSI_RED\\\\00"\""
    sed -i '/^@'$LEC_DEFINED_ANSI_RED_GLOBAL_VAR""'/{h;s/=.*/= '"$RED_COLOR"'/}' $output
    WHITE_COLOR="unnamed_addr constant [6 x i8] c\""\\\\$ANSI_WHITE\\\\00"\""
    sed -i '/^@'$LEC_DEFINED_ANSI_WHITE_GLOBAL_VAR""'/{h;s/=.*/= '"$WHITE_COLOR"'/}' $output
}

rm -rf $output
stack run -- lec -i $input -o $output
modify_lec_global_string
sed -i '/optnone/d' $output
opt -O3 -S -strip-debug $output -o $output
lli $output
