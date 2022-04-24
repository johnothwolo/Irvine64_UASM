#!/bin/bash

if [ $# -lt 1 ]; then
    printf "Usage: asmCompile.sh <file.asm>"
    exit
fi

# change libIrvine.dylib whe linux support is added.
if [ -e uasm ] && [ -e objconv ] && [ -e libIrvine64.dylib ]; then
    files=
    for file in "$@"
    do
        # printf "${file}\n"
        eval ./uasm -elf64 -I./include -Fo"${file%.asm}.o" "${file}" || exit
        eval ./objconv -fmacho64 -nu "${file%.asm}.o" "${file%.asm}.macho" || exit
        files+="${file%.asm}.macho "
    done

    # eval g++ -O0 "${files}" -L"$PWD" -lIrvine64 -framework CoreFoundation -o "${file%.asm}" || exit
    # we're compiling with Irvine64.macho and floatio.macho because we want debug symbols.
    # i'll figure out how to preserve them later.
    eval g++ -O0 "${files}" Irvine64.macho floatio.macho -L"$PWD" -lkernel32 -framework CoreFoundation -o "${file%.asm}" || exit

    
else
    printf "run buildIrvine.sh first\n"
fi