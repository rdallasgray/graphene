;; Set up the necessary directories and add to load-path.

(defvar graphene-root-dir (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(add-to-list 'load-path graphene-root-dir)
(add-to-list 'load-path (concat graphene-root-dir "vendor/"))
(add-to-list 'custom-theme-load-path (concat graphene-root-dir "theme/"))

(defvar graphene-sys
  (cond ((eq system-type 'darwin) "osx")
        ((eq system-type 'gnu/linux) "linux")
        ((eq system-type 'windows-nt) "windows")
        (t "other")
        ))

(defvar graphene-sys-defaults-file (format "graphene-%s-defaults.el" graphene-sys))

(require 'graphene-helper-functions)
(require 'graphene-editing)
(require 'graphene-env)
(require 'graphene-speedbar)
(require 'graphene-projects)
(require 'graphene-keys)
(load-file (concat graphene-root-dir graphene-sys-defaults-file))
(require 'graphene-look)

(provide 'graphene)
