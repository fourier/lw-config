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

(editor:bind-key "Function Documentation" "Hyper-F1" :global)
(editor:bind-key "Show Documentation" "Meta-F1" :global)
;;(editor:bind-key "Function Arglist" "Meta-=" :global)
;;(editor:bind-key "Function Argument List" "Meta-A" :global)

;; selection keys to emulate Mac-emulation mode
(require "mac-mode")
(require "selection-mode")
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
(editor:bind-key "Kill Previous Word" "Control-Backspace" :global)

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
(editor:bind-key "Debugger Abort" #("Control-c" "Control-c") :global)


;; Start Symbol browser by F3
(editor:bind-key "Apropos" "F3" :global)
