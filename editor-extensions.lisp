(in-package "EDITOR")

(editor:defcommand "Comment Do What I Mean" (p) 
     "Similar to Emacs comment-dwim. If the region is selected,
comment it; otherwise add comment at the end of line" ""
  (let ((b (current-buffer)))
    (editor:with-buffer-locked (b :for-modification t)
      (if (region-highlighted-p b)
          (comment-region-command p)
          (indent-for-comment-command p)))))


;; bug: removed the syntax coloring, remains the comment colors
;; http://permalink.gmane.org/gmane.lisp.lispworks.general/671
(editor:defcommand "Uncomment Region" (p)
     "Delete all sequences of ; at start of each line in region"
     "Delete comment prefixes"
  (declare (ignore p))
  (let ((buffer (editor:current-buffer)))
    (editor:with-point
       ((start (editor:buffer-point buffer))
        (end (editor:buffer-mark buffer)))
      (when (editor:point> start end)
        (rotatef start end)) ; so point<= start end
      (editor:collect-undo buffer
        (loop while (editor:point< start end)
              do
              (unless (editor:start-line-p start)               ; ensure at start of line
                (editor:line-start start))
              (editor:with-point ((end1 start))
                ;; move a point to end of command prefix
                (loop while (eq (editor:character-at end1 0) #\;)
                      do (editor:character-offset end1 1))
                (unless (editor:point= end1 start)
                  (when (eq (editor:character-at end1 0) #\Space) ; remove separating space
                    (editor:character-offset end1 1)))          ; if any
                (editor:delete-between-points start end1))      ; delete the comment prefix
              (editor:line-offset start 1))))))                 ; move to next line