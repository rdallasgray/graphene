;; Helper functions necessary for graphene to work.

;; Just kill the default buffer
(defun kill-default-buffer ()
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

;; Kill all file-based buffers
(defun kill-buffer-if-file (buf)
  (if (buffer-file-name buf)
      (kill-buffer)))
(defun kill-all-buffers ()
    "Kill all file-based buffers."
    (interactive)
    (mapc (lambda (buf) (kill-buffer-if-file buf))
     (buffer-list)))

;; Quickly create new buffers
(defun create-new-buffer ()
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))

;; Create a new instance of emacs
(if window-system
    (if (boundp 'path-to-emacs)
      (progn
        (defun new-emacs-instance ()
          (interactive)
          (call-process path-to-emacs nil 0 nil))
        )))

;; Add closing semicolon from anywhere in line
(defun insert-semicolon-at-end-of-line ()
  (interactive)
  (end-of-line)
  (insert ";"))

;; Add and goto newline from anywhere in line (after cua because of C-RET)
(defun newline-anywhere ()
  (interactive)
  (end-of-line)
  (newline))

(provide 'graphene-helper-functions)
