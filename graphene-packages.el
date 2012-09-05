;; Automatically install and initialise packages needed by Graphene (with thanks to Emacs Prelude - https://github.com/bbatsov/prelude)

(require 'package)
(require 'cl)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade"  . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar graphene-packages
  '(
    ;; editing
    auto-complete autopair smart-tab multi-web-mode
    ;; env
    smex expand-region
    ;; look
    color-theme-solarized
    ;; projects
    project-mode)
   "The packages Graphene needs to work.")

(defun graphene-packages-installed-p ()
  (loop for p in graphene-packages
        when (not (package-installed-p p)) do (return nil)
        finally (return t)))

(defun graphene-install-packages ()
  (unless (graphene-packages-installed-p)
    ;; check for new packages (package versions)
    (message "%s" "Graphene is refreshing its package database ... ")
    (package-refresh-contents)
    (message "%s" " done.")
    ;; install the missing packages
    (dolist (p graphene-packages)
      (unless (package-installed-p p)
        (package-install p)))))

(graphene-install-packages)

(provide 'graphene-packages)
