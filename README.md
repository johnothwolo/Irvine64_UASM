Instructions:

###Install Wine.

Mac
---

[If you don't have, install Homebrew](https://docs.brew.sh/Installation.html).

```sh
brew install wine
```

Debian-based-Linux
------------------

```sh
sudo dpkg --add-architecture i386 
sudo add-apt-repository ppa:ubuntu-wine/ppa
sudo apt-get update
sudo apt-get install -y wine-stable
```

Arch-linux
----------

```sh
sudo pacman -Syu wine
```

###Install MASM SDK

```sh
wine /pathtofolder/masm_sdk.exe
```

###Install Registries
```sh
wine regedit /pathtofolder/registry.reg
```

###Open folder In Visual Studio Code
1. Open this folder in VS Code
2. Create new file
    - File > New File ⌘N
    - Save and name the file *.asm
3. Write some MASM code
    - There is a provided template that has the Irvine library `Include`d.
4. Build current file (⇧⌘B)
    - Make sure the file is the active tab
    - View > Command Palette... ⇧⌘P
    - Write task
    - Select Build current file
    - Alternatively you could use ⌘B
5. If you don't want to use VSCode, there is a masm compilation script included
   to make compiling easier with the script...
    - `mkdir ~/bin`
    - `cp /pathtofolder/masm ~/bin/` 
    - `export PATH=$PATH:~/bin`

```sh
Usage: masm <sourcefile>
```

##Still Working on

Ability to `include Irvine16.inc` files
Other enhancements


###Sources: 

    - [Installing MASM SDK and masm script](https://reberhardt.com/blog/programming/2016/01/30/masm-on-mac-or-linux.html)
    - [Microsoft Visual Studio turotrials](https://code.visualstudio.com/docs/)
    - [Inspiration](https://github.com/janka102/MASM_OSX)