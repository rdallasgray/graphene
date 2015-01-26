;;; graphene-projects.el --- Graphene defaults for project management
;;
;; Copyright (c) 2015 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.8.2
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

(require 'graphene-helper-functions)
(require 'graphene-speedbar)
(require 'project-persist)

(project-persist-mode t)

(defun graphene-set-project-root (dir)
  "Change the default directory and update speedbar if used."
  (setq default-directory dir)
  (when graphene-speedbar-auto
    (require 'sr-speedbar)
    (sr-speedbar-open)
    (speedbar-update-contents)
    (when graphene-project-pin-speedbar
      (graphene-pin-speedbar dir))))

(defun graphene-load-project-desktop ()
  "Load the project's desktop if available."
  (ignore-errors
    (setq default-directory project-persist-current-project-settings-dir)
    (message (format "Loading project desktop from %s" default-directory))
    (desktop-read project-persist-current-project-settings-dir)))

(defun graphene-maybe-unpin-speedbar ()
  "Try to unpin the speedbar if using it."
  (when graphene-speedbar-auto
    (ignore-errors (graphene-unpin-speedbar))))

(add-hook 'project-persist-before-load-hook
          (lambda ()
            (graphene-maybe-unpin-speedbar)
            (kill-all-buffers)))

(add-hook 'project-persist-after-close-hook
          (lambda ()
            (kill-all-buffers)
            (graphene-maybe-unpin-speedbar)))

(add-hook 'project-persist-after-load-hook
          (lambda ()
            (graphene-load-project-desktop)
            (graphene-set-project-root project-persist-current-project-root-dir)))

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
