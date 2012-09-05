;; Uses project-mode by Benjamin Cluff https://github.com/psyllo/emacsenations.
;; Adds hooks to allow actions on switching projects.
;; Opening a project saves any open project, kills all file-based buffers,
;; loads the project's desktop, and sets the speedbar directory to the project's root.
;; Saving a project also saves its desktop.

(require 'graphene-helper-functions)

(defvar graphene-projects-folder (concat user-emacs-directory "projects/"))

(unless (file-exists-p graphene-projects-folder)
  (make-directory graphene-projects-folder))

;; Enable project-mode.
(require 'project-mode)
(project-mode t)

;; Define hooks.
(defvar graphene-project-before-open-hook)
(defvar graphene-project-after-open-hook)
(defvar graphene-project-after-save-hook)

;; Define a variable to expose the project's root directory.
(defvar graphene-project-root)

;; Project folder for the .proj and .desktop files etc.
(defvar graphene-project-folder)

;; Set graphene-project-root, set default-directory, update speedbar
(defun graphene-set-project-directory (dir)
  (setq graphene-project-root dir)
  (setq graphene-project-folder (concat graphene-projects-folder (project-name (project-current))))
  (unless (file-exists-p graphene-project-folder)
    (make-directory graphene-project-folder))
  (setq project-proj-files-dir graphene-project-folder)
  (message (concat "Set graphene-project-root to " graphene-project-root))
  (setq default-directory dir)
  (message "Updating speedbar")
  (sr-speedbar-open)
  (speedbar-update-contents)
  )

;; Create hooks in project-mode.
(defadvice project-new (after graphene-project-run-after-new-hook () activate)
  "Run graphene-project-after-new-hook after project-new."
  (progn (message "Running graphene-project-after-new-hook")
    (run-hooks 'graphene-project-after-new-hook))
  )
(defadvice project-load (before graphene-project-run-before-open-hook () activate)
  "Run graphene-project-before-open-hook before project-load."
  (progn (message "Running graphene-project-before-open-hook")
    (run-hooks 'graphene-project-before-open-hook))
  )
(defadvice project-load-and-select (after graphene-project-run-after-open-hook () activate)
  "Run graphene-project-after-open-hook after project select."
  (progn (message "Running graphene-project-after-open-hook")
    (run-hooks 'graphene-project-after-open-hook))
  )
(defadvice project-save (after graphene-project-run-after-save-hook () activate)
  "Run graphene-project-after-save-hook after project-save."
  (progn (message "Running graphene-project-after-save-hook")
    (run-hooks 'graphene-project-after-save-hook))
  )

;; Save any open project and kill all file-based buffers before opening a new project.
(add-hook 'graphene-project-after-new-hook
          (lambda ()
            (progn
              (graphene-set-project-directory
                    (project-default-directory (project-current)))
              (project-save)
              )))

;; Save any open project and kill all file-based buffers before opening a new project.
(add-hook 'graphene-project-before-open-hook
          (lambda ()
            (progn
              (if (boundp 'graphene-project-root)
                  (project-save))
              (message "Saved project")
              (kill-all-buffers)
              )))
;; Set the project root directory, load the project desktop and update speedbar.
(add-hook 'graphene-project-after-open-hook
          (lambda ()
            (progn
              (graphene-set-project-directory
                    (project-default-directory (project-current)))
              (message (concat "Loading project desktop from " graphene-project-root))
              (desktop-read)
              )))
;; Save the project desktop.
(add-hook 'graphene-project-after-save-hook
          (lambda ()
            (progn
              (message (format "Saving project desktop in %s" graphene-project-folder))
              (desktop-save graphene-project-folder)
              )))
(provide 'graphene-projects)
