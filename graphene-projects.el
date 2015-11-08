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

;; http://www.emacswiki.org/DeskTop#toc4: Overriding stale desktop locks
(defun emacs-process-p (pid)
  "If pid is the process ID of an emacs process, return t, else nil.
Also returns nil if pid is nil."
  (when pid
    (let ((attributes (process-attributes pid)) (cmd))
      (dolist (attr attributes)
        (if (string= "comm" (car attr))
            (setq cmd (cdr attr))))
      (if (and cmd (or (string= "emacs" cmd) (string= "emacs.exe" cmd))) t))))

(defadvice desktop-owner (after pry-from-cold-dead-hands activate)
  "Don't allow dead emacsen to own the desktop file."
  (when (not (emacs-process-p ad-return-value))
    (setq ad-return-value nil)))

(provide 'graphene-projects)

;;; graphene-projects.el ends here
