;;; graphene-projects.el --- Graphene defaults for project management
;;
;; Copyright (c) @YEAR Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: @VERSION
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Graphene is a set of default settings and functionality to make Emacs a little friendlier.
;; This file defines default settings and functionality for project management.
;; It uses Project-persist (https://github.com/rdallasgray/project-persist) as a base
;; for its functionality, adding just the capability to load and save desktops along with project settings,
;; and to open the speedbar at the correct directory location.

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

(defcustom graphene-project-drawer-auto t
  "Whether graphene should open a project drawer when a project is loaded."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-project-drawer-adaptor 'ppd-sr-speedbar
  "The adaptor graphene should use to show the project persist drawer."
  :type 'symbol
  :group 'graphene)

(require 'graphene-helper-functions)
(require 'project-persist)

(project-persist-mode t)

(require graphene-project-drawer-adaptor)

(project-persist-drawer-mode graphene-project-drawer-auto)
(global-set-key (kbd "C-c s") 'sr-speedbar-select-window)

(defun graphene-load-project-desktop ()
  "Load the project's desktop if available."
  (ignore-errors
    (setq default-directory project-persist-current-project-settings-dir)
    (message (format "Loading project desktop from %s" default-directory))
    (desktop-read project-persist-current-project-settings-dir)))

(add-hook 'project-persist-before-load-hook 'kill-all-buffers)
(add-hook 'project-persist-after-close-hook 'kill-all-buffers)
(add-hook 'project-persist-after-load-hook 'graphene-load-project-desktop)

(add-hook 'project-persist-after-save-hook
          (lambda ()
            (message (format "Saving project desktop in %s" project-persist-current-project-settings-dir))
            (desktop-save project-persist-current-project-settings-dir)))

(require 'dash)

(defun emacs-process-p (pid)
  "If PID is the process ID of an EMACS process, return t, else nil.
Also returns nil if pid is nil."
  (when (and pid (memq pid (list-system-processes)))
    (let* ((attributes (process-attributes pid))
           (cmd-cell (-first (lambda (cell) (string= "comm" (car cell))) attributes)))
      (and cmd-cell (string-match-p "emacs" (cdr cmd-cell))))))

(defun pid-if-active-emacs-process (orig &optional dirname)
  (let ((pid (apply orig '(dirname))))
    (when (emacs-process-p pid) pid)
    pid))

(advice-add 'desktop-owner :around #'pid-if-active-emacs-process)

(provide 'graphene-projects)

;;; graphene-projects.el ends here
