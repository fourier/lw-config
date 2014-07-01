;; -*- Mode: Lisp; -*-

;; Rebind prefix Ctrl-X to Ctrl-U for convinience in Dvorak
;; Based on /Applications/LispWorks\ Personal\ 6.1/Library/lib/6-1-0-0/config/mac-key-binds.lisp

(in-package "CL-USER")

;;; Key bindings for Mac emulation
;;; This file is loaded automatically the first time that Mac emulation is used. 

(flet ((mac-bind-key (command key &rest specific)
         (if specific
             (apply 'editor:bind-key command key specific)
           (editor:bind-key command key :global :mac))))



(mac-bind-key "Set Prefix Argument" "Control-x")

;; Emacs Control-x bindings
(mac-bind-key "What Cursor Position" #("Control-u" "=")) ;MJS 10Jul93: added emacs-like binding
(mac-bind-key "Previous Page" #("Control-u" "["))
(mac-bind-key "Next Page" #("Control-u" "]"))
(mac-bind-key "Mark Page" #("Control-u" "Control-p"))
(mac-bind-key "Count Lines Page" #("Control-u" "l"))
(mac-bind-key "Mark Whole Buffer" #("Control-u" "h"))
(mac-bind-key "Exchange Point and Mark" #("Control-u" "Control-u"))
(mac-bind-key "Point to Register" #("Control-u" "/"))
(mac-bind-key "Jump to Register" #("Control-u" "j"))
(mac-bind-key "Copy to Register" #("Control-u" "x"))
(mac-bind-key "Insert Register" #("Control-u" "g"))
(mac-bind-key "Uppercase Region" #("Control-u" "Control-u"))
(mac-bind-key "Lowercase Region" #("Control-u" "Control-l"))
(mac-bind-key "Delete Blank Lines" #("Control-u" "Control-o") :global)
(mac-bind-key "Forward Kill Sentence" #("Control-u" "Delete"))
(mac-bind-key "Backward Kill Sentence" #("Control-u" "Backspace"))
(mac-bind-key "Undo" #("Control-u" "u"))
(mac-bind-key "Transpose Lines" #("Control-u" "Control-t"))
(mac-bind-key "Indent Rigidly" #("Control-u" "Tab"))
(mac-bind-key "Indent Rigidly" #("Control-u" "Control-i"))
(mac-bind-key "Set Fill Prefix" #("Control-u" "."))
(mac-bind-key "Set Fill Column" #("Control-u" "f"))
(mac-bind-key "Next Search Match" #("Control-u" "`"))
(mac-bind-key "Wfind File" #("Control-u" "Control-f"))
(mac-bind-key "Find Alternate File" #("Control-u" "Control-v"))
(mac-bind-key "Write File" #("Control-u" "Control-w"))
(mac-bind-key "Save File" #("Control-u" "Control-s"))
(mac-bind-key "Save All Files" #("Control-u" "s"))
(mac-bind-key "Save All Files and Exit" #("Control-u" "Control-c"))
(mac-bind-key "Insert File" #("Control-u" "i"))
(mac-bind-key "Select Buffer" #("Control-u" "b"))
(mac-bind-key "List Buffers" #("Control-u" "Control-b"))
(mac-bind-key "Kill Buffer" #("Control-u" "k"))
(mac-bind-key "Check Buffer Modified" #("Control-u" "~"))
(mac-bind-key "Toggle Buffer Read-Only" #("Control-u" "Control-q"))
(mac-bind-key "New Window" #("Control-u" "2")) 
(mac-bind-key "Next Ordinary Window" #("Control-u" "o"))
(mac-bind-key "Delete Window" #("Control-u" "0")) ; added 13/2/89 (MSM)
(mac-bind-key "Delete Other Windows" #("Control-u" "1"))
(mac-bind-key "Add Mode Word Abbrev" #("Control-u" "Control-a"))
(mac-bind-key "Add Global Word Abbrev" #("Control-u" "+"))
(mac-bind-key "Inverse Add Mode Word Abbrev" #("Control-u" "Control-h"))
(mac-bind-key "Inverse Add Global Word Abbrev" #("Control-u" "-"))
(mac-bind-key "Set Comment Column" #("Control-u" ";"))
(mac-bind-key "Evaluate Last Form" #("Control-u" "Control-e"))
(mac-bind-key "Define Keyboard Macro" #("Control-u" "("))
(mac-bind-key "End Keyboard Macro" #("Control-u" ")"))
(mac-bind-key "Last Keyboard Macro" #("Control-u" "e"))
(mac-bind-key "Keyboard Macro Query" #("Control-u" "q"))
(mac-bind-key "Illegal" #("Control-u" "b") :mode "Pc Echo")
(mac-bind-key "Illegal" #("Control-u" "Control-w") :mode "Pc Echo")
(mac-bind-key "Illegal" #("Control-u" "i") :mode "Pc Echo")
(mac-bind-key "Illegal" #("Control-u" "Control-s") :mode "Pc Echo")
(mac-bind-key "Illegal" #("Control-u" "Control-v") :mode "Pc Echo")
(mac-bind-key "Illegal" #("Control-u" "k") :mode "Pc Echo")
(mac-bind-key "Select Buffer Other Window" #("Control-u" "4" "b"))
); flet
