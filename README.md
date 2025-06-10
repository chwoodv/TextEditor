# KILO

**kilo** is a lightweight, terminal-based text editor written in pure C. It aims to be minimal, fast, and easy to build, offering essential features for everyday text editing directly from the command line.

---

## Table of Contents

- [Features](#features)  
- [Usage](#usage)  
- [Configuration](#configuration)  
- [Keybindings](#keybindings)  
- [Dependencies](#dependencies)  
- [Build Instructions](#build-instructions)  
- [Roadmap](#roadmap)  
- [Contributing](#contributing)  
- [License](#license)

---

## Features

- Basic text editing (insert, delete, overwrite)
- Open and save text files
- Scrollable window
- Cursor navigation with arrow keys
- Find 
- Status bar showing file name and cursor position
- Minimal UI for distraction-free writing
- File type based colored syntax
- No external dependencies (only standard C libraries)

---

### Clone and Build

```sh
git clone https://github.com/chwoodv/TextEditor
cd TextEditor
make
```

## Usage
```sh
./kilo filename.txt
```
If the file exists, it will be loaded.

If the file doesn’t exist, an error will be returned.

To exit, press Ctrl + Q. Don’t forget to save with Ctrl + S first!

Or to create a new file,
```sh
./kilo
```
And save as after writing.

## Configuration

Kilo does not require any configuration to operate.

## Keybindings

Action          Keys
---
Save File           Ctrl + S
Quit Editor	        Ctrl + Q
Find                Ctrl + F
Move Cursor         Arrow Keys
Backspace/Delete    Backspace/Delete keys
Insert Character	Any key
Scroll	            Auto scroll

## Dependencies

Standard C libraries (stdlib.h, stdio.h, etc.)

POSIX terminal interface (termios.h)

No third-party libraries required

## Build Instructions  

Make sure you have a C compiler (like gcc):

```sh
make
```
To clean up object files:
```sh
make clean
```
