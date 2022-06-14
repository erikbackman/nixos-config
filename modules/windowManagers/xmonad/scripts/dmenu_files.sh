#!/usr/bin/env bash
shopt -s lastpipe
unset ARRAY;
declare -a ARRAY
ARRAY=()
find $1 -maxdepth 1 ! -path $1 -type f -exec basename {} \; | readarray -t ARRAY
LEN=${#ARRAY[@]}
CHOICE=$(printf "%s\n" "${ARRAY[@]}" | dmenu -l $LEN -i)
[ $? = 0 ] && zathura $(printf "%s/%s" $1 $CHOICE)
