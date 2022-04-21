/Users/john/Downloads/UASM-master/GccUnixD/uasm -elf64 Irvine64.asm 
/Users/john/objconv/src/objconv -fmacho64 -nu Irvine64.o Irvine64.macho
/Users/john/objconv/src/objconv -fmacho64 -nu floatio.o floatio.macho
g++ -shared Irvine64.macho floatio.macho -L/Users/john/Library/Developer/Xcode/DerivedData/kernel32-dzkiqtfckdfzxgajajlygpoqfwca/Build/Products/Debug -lkernel32 -framework CoreFoundation -lutil -o libIrvine64.dylib