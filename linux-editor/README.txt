How to open .lisp files (on linux) via xdg-open and file managers.

1. Copy a file lw-editor to some location, i.e. ~/Applications/LispWorks
2. Copy an icon file lw.ico to same location, i.e. ~/Applications/LispWorks
3. Edit the lw-editor file specifying the LispWorks executable
4. Copy lweditor.desktop to ~/.local/share/applications folder. Make sure the file is executable (+x)
5. Edit this lweditor.desktop to make sure paths are correct.
6. Add the line:
text/x-common-lisp=lweditor.desktop;
to the file ~/.config/mimeapps.list [Added Associations] section.
