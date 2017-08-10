;;; graphene-helper-functions.el --- Graphene helper functions
;;
;; Copyright (c) 2017 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.9.8
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a set of default settings and functionality to make Emacs a little friendlier.
;; These helper functions enable simple functionality used throughout the rest of the package.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(defun kill-default-buffer ()
  "Kill the currently active buffer -- set to C-x k so that users are not asked which buffer they want to kill."
  (interactive)
  (let (kill-buffer-query-functions) (kill-buffer)))

(global-set-key (kbd "C-x k") 'kill-default-buffer)


(defun kill-buffer-if-file (buf)
  "Kill a buffer only if it is file-based."
  (when (buffer-file-name buf)
    (when (buffer-modified-p buf)
        (when (y-or-n-p (format "Buffer %s is modified - save it?" (buffer-name buf)))
            (save-some-buffers nil buf)))
    (set-buffer-modified-p nil)
    (kill-buffer buf)))


(defun kill-all-buffers ()
    "Kill all file-based buffers."
    (interactive)
    (mapc (lambda (buf) (kill-buffer-if-file buf))
     (buffer-list)))


(defun kill-buffer-and-window ()
  "Close the current window and kill the buffer it's visiting."
  (interactive)
  (progn
    (kill-buffer)
    (delete-window)))

(global-set-key (kbd "C-x C-k") 'kill-buffer-and-window)


(defun create-new-buffer ()
  "Create a new buffer named *new*[num]."
  (interactive)
  (switch-to-buffer (generate-new-buffer-name "*new*")))

(global-set-key (kbd "C-c n") 'create-new-buffer)


(defun insert-semicolon-at-end-of-line ()
  "Add a closing semicolon from anywhere in the line."
  (interactive)
  (save-excursion
    (end-of-line)
    (insert ";")))

(global-set-key (kbd "C-;") 'insert-semicolon-at-end-of-line)


(defun comment-current-line-dwim ()
  "Comment or uncomment the current line."
  (interactive)
  (save-excursion
    (push-mark (beginning-of-line) t t)
    (end-of-line)
    (comment-dwim nil)))

(global-set-key (kbd "C-M-;") 'comment-current-line-dwim)


(defun newline-anywhere ()
  "Add a newline from anywhere in the line."
  (interactive)
  (end-of-line)
  (newline-and-indent))

(global-set-key (kbd "M-RET") 'newline-anywhere)


(defun increase-window-height (&optional arg)
  "Make the window taller by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg))

(global-set-key (kbd "C->") 'increase-window-height)


(defun decrease-window-height (&optional arg)
  "Make the window shorter by one line. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg)))

(global-set-key (kbd "C-<") 'decrease-window-height)


(defun decrease-window-width (&optional arg)
  "Make the window narrower by one column. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window (- 0 arg) t))

(global-set-key (kbd "C-,") 'decrease-window-width)


(defun increase-window-width (&optional arg)
  "Make the window wider by one column. Useful when bound to a repeatable key combination."
  (interactive "p")
  (enlarge-window arg t))

(global-set-key (kbd "C-.") 'increase-window-width)


(when window-system
  (defun new-emacs-instance ()
    "Create a new instance of Emacs."
    (interactive)
    (let ((path-to-emacs
           (locate-file invocation-name
                        (list invocation-directory) exec-suffixes)))
      (call-process path-to-emacs nil 0 nil))))

(global-set-key (kbd "C-c N") 'new-emacs-instance)


(provide 'graphene-helper-functions)

;;; graphene-helper-functions.el ends here
