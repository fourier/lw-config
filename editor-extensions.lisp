;;(in-package "EDITOR")
(defpackage #:lw-config.editor-commands
  (:use #:cl #:editor)
  (:add-use-defaults t))


(in-package lw-config.editor-commands)

(defun region-commented-p ()
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (with-point
       ((start  (buffer-point buffer))
        (end (buffer-mark buffer)))
      (when (point> start end)
        (rotatef start end)) ; so point<= start end
      (loop with line-is-comment = nil
            while (point< start end)
            do
            (unless (start-line-p start)               ; ensure at start of line
              (line-start start))
            (with-point ((end1 start))
              ;; skip starting spaces
              (loop while (eq (character-at end1 0) #\Space)
                    do (character-offset end1 1))
              ;; move a point to end of command prefix
              (setf line-is-comment nil)
              (loop while (eq (character-at end1 0) #\;)
                    do
                    (character-offset end1 1)
                    (setf line-is-comment t)))
            (line-offset start 1)            
            when (not line-is-comment)
            return line-is-comment
            finally (return line-is-comment)))))                   ; move to next line


(editor:defcommand "Comment Do What I Mean" (p) 
     "Similar to Emacs comment-dwim. If the region is selected,
comment it; otherwise add comment at the end of line" ""
  (let ((b (current-buffer)))
    (with-buffer-locked (b :for-modification t)
      (cond ((and (editor::region-highlighted-p b)
                  (region-commented-p))
             (uncomment-region-command p))
            ((editor::region-highlighted-p b)
             (editor::comment-region-command p))
            (t 
             (indent-for-comment-command p))))))


;; bug: removed the syntax coloring, remains the comment colors
;; http://permalink.gmane.org/gmane.lisp.lispworks.general/671
(editor:defcommand "Uncomment Region" (p)
     "Delete all sequences of ; at start of each line in region"
     "Delete comment prefixes"
  (declare (ignore p))
  (let ((buffer (current-buffer)))
    (with-point
       ((start  (buffer-point buffer)) (end (buffer-mark buffer)))
      (when (point> start end)
        (rotatef start end)) ; so point<= start end
      (collect-undo buffer
        (loop while (point< start end)
              do
              (unless (start-line-p start)               ; ensure at start of line
                (line-start start))
              (with-point ((end1 start))
                ;; skip starting spaces
                (loop while (eq (character-at end1 0) #\Space)
                      do (character-offset end1 1))
                ;; move a point to end of command prefix
                (loop while (eq (character-at end1 0) #\;)
                      do (character-offset end1 1))
                (unless (point= end1 start)
                  (when (eq (character-at end1 0) #\Space) ; remove separating space
                    (character-offset end1 1)))          ; if any
                (delete-between-points start end1))      ; delete the comment prefix
              (line-offset start 1))))                   ; move to next line
    (editor::font-lock-fontify-buffer buffer)))               


(editor:defcommand "Search In Dash" (p &optional name)
     "Search current word in Dash"
     "Search current word in Dash"
  (let* ((name (format nil "~a" (editor:get-symbol-from-point (editor:current-point)))))
    (objc:invoke (objc:invoke "NSWorkspace" "sharedWorkspace") "openURL:" (objc:invoke "NSURL" "URLWithString:" (concatenate 'string "dash://" name)))))


(editor:defcommand "Save File and Compile Defun" (p)
     "Save current file and compile current function"
     "Save current file and compile current function"
  (editor:save-file-command p)
  (editor:compile-defun-command p))
