;; Set up the necessary directories and add to load-path.
(defvar graphene-root-dir (file-name-directory (if load-file-name load-file-name buffer-file-name)))
(add-to-list 'load-path graphene-root-dir)
(add-to-list 'load-path (concat graphene-root-dir "vendor/"))
(add-to-list 'custom-theme-load-path (concat graphene-root-dir "theme/"))

(defvar graphene-sys
  (if (eq system-type "darwin")
      "osx"
    (if (eq system-type "gnu/linux")
        "linux"
      (if (eq system-type "windows-nt")
          "windows")
      "other"
    )))
(defvar graphene-sys-defaults-file (format "graphene-%s-defaults" graphene-sys))

(require 'graphene-packages)
(require 'graphene-osx-defaults)
(require 'graphene-helper-functions)
(require 'graphene-editing)
(require 'graphene-env)
(require 'graphene-speedbar)
(require 'graphene-projects)
(require 'graphene-look)
(require 'graphene-keys)

(provide 'graphene)
