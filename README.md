Instructions:


Building Irvine: 
--------

run `build.sh`.

Building masm programs:
--------
1. `cd` into Irvine64_Library
2. run asmCompile.sh "/path/to/asm.asm"

Notes: 
--------

As of macOS Catalina OSX doesn't have the ability to run 64bit applications. As a result 32bit absolute addressing isn't supported. 
Instead of using `mov rax, OFFSET var`, use `lea rax, var`

Sources: 
--------

   - [Installing MASM SDK and masm script](https://reberhardt.com/blog/programming/2016/01/30/masm-on-mac-or-linux.html)
   - [Microsoft Visual Studio turotrials](https://code.visualstudio.com/docs/)
   - [Inspiration](https://github.com/janka102/MASM_OSX)
