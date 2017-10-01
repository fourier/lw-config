;; -*- Mode: lisp; -*-
(in-package "CL-USER")


;; Bind Ctrl-Up/Down to previous/next commands in Listener
(editor:bind-key "History Next" "Control-Down" :mode "Execute")
(editor:bind-key "History Previous" "Control-Up" :mode "Execute")
;; Same for Cmd-UP/Down 
(editor:bind-key "History Next" "Hyper-Down" :mode "Execute")
(editor:bind-key "History Previous" "Hyper-Up" :mode "Execute")

;; Enter will indent new line
(editor:bind-key "Indent New Line" #\Return :mode "Lisp")

;; per-word movements
(editor:bind-key "Forward Word" "Control-Right" :global)
(editor:bind-key "Backward Word" "Control-Left" :global)

(editor:bind-key "Forward Word" "Hyper-Right" :global)
(editor:bind-key "Backward Word" "Hyper-Left" :global)

;;(editor:bind-key "Function Documentation" "Hyper-F1" :global)
#+:cocoa (editor:bind-key "Search In Dash" "Hyper-F1" :global)
#-:cocoa (editor:bind-key "Search In Dash" "Ctrl-F1" :global)
(editor:bind-key "Show Documentation" "Meta-F1" :global)
;;(editor:bind-key "Function Arglist" "Meta-=" :global)
;;(editor:bind-key "Function Argument List" "Meta-A" :global)

;; selection keys to emulate Mac-emulation mode
;;(require "win-mode")
;;(require "selection-mode")
(editor:bind-key "Backward Character Extending Selection" "Shift-Left" :global)
(editor:bind-key "Forward Character Extending Selection"  "Shift-Right" :global)
(editor:bind-key "Previous Line Extending Selection"      "Shift-Up" :global)
(editor:bind-key "Next Line Extending Selection"          "Shift-Down" :global)

(editor:bind-key "Backward Word Extending Selection"      "Meta-Shift-Left" :global)
(editor:bind-key "Forward Word Extending Selection"       "Meta-Shift-Right" :global)

(editor:bind-key "Backward Word Extending Selection"      "Control-Shift-Left" :global)
(editor:bind-key "Forward Word Extending Selection"       "Control-Shift-Right" :global)

(editor:bind-key "Backward Form Extending Selection"      "Meta-Control-Shift-Left" :global)
(editor:bind-key "Forward Form Extending Selection"       "Meta-Control-Shift-Right" :global)
(editor:bind-key "End of Defun Extending Selection"       "Meta-Control-Shift-Next" :global)
(editor:bind-key "Beginning of Defun Extending Selection" "Meta-Control-Shift-Prior" :global)
(editor:bind-key "Backward Up List Extending Selection"   "Meta-Control-Shift-Up" :global)
(editor:bind-key "Down List Extending Selection"          "Meta-Control-Shift-Down" :global)

;; Control-Backspace as usual
(editor:bind-key "Kill Previous Word" "Control-Backspace" :mode "Lisp")
(editor:bind-key "Kill Previous Word" "Control-Backspace" :mode "Execute")
;; exeption for Echo Area
(editor:bind-key "Echo Area Kill Previous Word" "Control-Backspace" :mode "Echo Area")

;; emacs-like comment hotkey
(editor:bind-key "Comment Do What I Mean" "Meta-;" :global)

;; Ctrl-Up/Down move one paragraph up/down
(editor:bind-key "Forward Paragraph" "Control-Down" :global)
(editor:bind-key "Backward Paragraph" "Control-Up" :global)

;; Cmd-Up/Down move one sentence up/down
(editor:bind-key "Forward Sentence" "Hyper-Down" :global)
(editor:bind-key "Backward Sentence" "Hyper-Up" :global)

;; Goto Line
(editor:bind-key "Goto Line" #("Control-u" "g") :global)
;; Remove all spaces and join lines
(editor:bind-key "Delete Indentation" #("Control-u" "j") :global)

;; move forward-backward sexp
(editor:bind-key "Forward List" "Hyper-Right" :global)
(editor:bind-key "Backward List" "Hyper-Left" :global)
(editor:bind-key "Forward List" "Meta-Right" :global)
(editor:bind-key "Backward List" "Meta-Left" :global)

;; Stop debugger on Ctrl-C twice
(editor:bind-key "Debugger Abort" #("Control-c" "Control-c") :mode "Execute")


;; Start Symbol browser by F3
(editor:bind-key "Apropos" "F3" :global)
(editor:bind-key "List Callers" "Hyper-F3" :global)
(editor:bind-key "Function Arglist Displayer" "F1" :global)

;; SLIME-like bindings
(editor:bind-key "Compile And Load Buffer File" #("Control-c" "Control-k") :mode "Lisp")
(editor:bind-key "Save File and Compile Defun" #("Control-c" "Control-c") :mode "Lisp")

;; Revert buffer like in Emacs
(editor:bind-key "Revert Buffer" "F5" :mode "Lisp")

;; Meta-up jumps to the beginning of the function
;; Meta-down jumps to the end of the function
(editor:bind-key "Beginning of Defun" "Meta-Up" :mode "Lisp")
(editor:bind-key "End of Defun" "Meta-Down" :mode "Lisp")

(editor:bind-key "Find Source" "Hyper-." :mode "Lisp")
(editor:bind-key "Next Window" "F6" :global)
;; Cmd+i in Listener to inspect last value
;; (editor:bind-key "Inspect Star" "Hyper-i" :mode "Execute")
;; bind Ctrl-C Ctrl-C to compile defun like in Slime
(editor:bind-key "Compile Defun" #("Control-c" "Control-c") :mode "Lisp")
#+:cocoa (editor:bind-key "Next Ordinary Window" "Meta-`" :global)
#-:cocoa (editor:bind-key "Next Window" "Meta-`" :global)

(editor:bind-key "Select Previous Buffer" "Ctrl-Return" :mode "Lisp")


;; Buffers tab
;;(editor:bind-key "List Buffers" "Ctrl-d" :mode "Lisp")
;; Definitions tab
(editor:bind-key "List Buffer Definitions" "Ctrl-d" :mode "Lisp")
;; Changed Definitions tab
;;(editor:bind-key "Buffer Changed Definitions" "Ctrl-d" :mode "Lisp")
;; Find Definitions tab
;;(editor:bind-key "View Source Search" "Ctrl-d" :mode "Lisp")

(editor:bind-key "Focus On Buffers Search" "F2" :mode "Lisp")
