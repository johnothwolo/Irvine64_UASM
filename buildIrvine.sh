#!/bin/bash

UNAME=$(uname -s)
uasm_makefile=

# linux isn't supported yet
if [ "${UNAME}" = Linux ]; then
    uasm_makefile="gccLinux64.mak"
elif [ "${UNAME}" = Darwin ]; then
    uasm_makefile="ClangOSX64.mak"
fi

eval xcodebuild -project kernel32/kernel32.xcodeproj -scheme kernel32 -derivedDataPath "$PWD"/kernel32/DerivedData|| exit
eval cp kernel32/DerivedData/Build/Products/Debug/libkernel32.a ./Irvine64_Library || exit
eval make -C ./UASM -f "${uasm_makefile}" CFLAGS+="-w" || exit
eval cp UASM/GccUnixR/uasm ./Irvine64_Library || exit
eval make -C objconv || exit
eval cp objconv/bin/objconv ./Irvine64_Library || exit

cd ./Irvine64_Library || exit

eval ./uasm -elf64 -I./include src/Irvine64.asm src/floatio.asm || exit
eval ./objconv -fmacho64 -nu Irvine64.o Irvine64.macho || exit
eval ./objconv -fmacho64 -nu floatio.o floatio.macho || exit
eval g++ -shared -O0 Irvine64.macho floatio.macho -L"$PWD" -lkernel32 -framework CoreFoundation -o libIrvine64.dylib