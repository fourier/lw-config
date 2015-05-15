(in-package "EDITOR")

(editor:defcommand "Comment Do What I Mean" (p) 
     "Similar to Emacs comment-dwim. If the region is selected,
comment it; otherwise add comment at the end of line" ""
  (let ((b (current-buffer)))
    (editor:with-buffer-locked (b :for-modification t)
      (if (region-highlighted-p b)
          (comment-region-command p)
          (indent-for-comment-command p)))))
