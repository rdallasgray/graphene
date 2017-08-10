;;; graphene.el --- Newbie-friendly defaults
;;
;; Copyright (c) 2017 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.9.8
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; [![Melpa Status](http://melpa.org/packages/graphene-badge.svg)](http://melpa.org/#/graphene)
;; 
;; 
;; # Graphene
;; Graphene is a 'starter kit' for Emacs, in the vein of
;; [Prelude](https://github.com/bbatsov/prelude) or
;; [emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit).
;; 
;; ![Graphene screenshot](http://s3-eu-west-1.amazonaws.com/graphene/graphene.png)
;; 
;; It provides:
;; - Some sensible defaults
;; - A clean look
;; - The basics you need to make Emacs functionality discoverable
;; - A set of third-party packages that set the standard for the
;;   essential functionality they provide
;; - Some plumbing to make the above 'just work', seamlessly and silently
;; 
;; Although Graphene is intended to help users of GUI editors such as
;; Textmate or Sublime Text find their feet quickly, it is not an attempt
;; to turn Emacs into a sparkly GUI editor or IDE. It is minimal,
;; lightweight, and respectful of the history and character of Emacs.
;; 
;; # News
;; Graphene 0.9 depends on [project-persist-drawer](https://github.com/rdallasgray/project-persist-drawer) and [ppd-sr-speedbar](https://github.com/rdallasgray/ppd-sr-speedbar) to hook a drawer into project activity.
;; 
;; Custom variables are extended and refined -- see the relevant info documentation in the `graphene` group.
;; 
;; ## Sensible defaults
;; Among many other things, Graphene turns off the Emacs startup screen,
;; turns on
;; [line wrapping](http://www.emacswiki.org/emacs/VisualLineMode), turns
;; off the scroll bars and tool bar (and menu bar on non-OS X systems),
;; moves automatic backups into the temp directory -- generally clears
;; the way of small annoyances and makes things look and work the way
;; you'd expect.
;; 
;; ## A clean look
;; Graphene includes its own 'meta-theme' which works hard to unify the
;; look of the editor across a range of packages. This theme loads on top
;; of any 'normal' theme you want to load, so you can still choose
;; exactly how you want your Emacs to look.
;; 
;; It also sets more pleasant and modern default fonts appropriate to the
;; host platform (which can be overridden), and maintains window size and
;; position across sessions.
;; 
;; ## Discoverability
;; At first Emacs can appear a little opaque; it is in fact a very
;; discoverable environment, and Graphene tries to turn this up to
;; maximum, by using
;; [Ido](http://emacswiki.org/emacs/InteractivelyDoThings) everywhere,
;; [Smex](http://www.emacswiki.org/Smex) for running extended commands,
;; and [Company](http://company-mode.github.io) for in-editor
;; completion. These allow gradual discovery of Emacs' functionality, and
;; gradual building of speed and fluidity.
;; 
;; ## Essential packages
;; The collection of packages Graphene includes prevents you having to
;; research and discover on your own what the Emacs community has largely
;; decided on as best-in-class packages.
;; 
;; - [project-persist](https://github.com/rdallasgray/project-persist)
;;   Disclaimer: this is my own project, and is perhaps the exception to the
;;   above rule. It provides simple project loading and saving;
;;   Graphene adds a project 'drawer' using
;;   [Sr-Speedbar](https://github.com/emacsmirror/sr-speedbar)
;; - [Smartparens](https://github.com/Fuco1/smartparens)
;;   For auto-pairing
;; - [Company](http://company-mode.github.io)
;;   For code completion
;; - [Web-mode](https://github.com/fxbois/web-mode)
;;   For mixed-mode editing
;; - [Smex](http://www.emacswiki.org/Smex)
;;   For command completion
;; - [Ido](http://emacswiki.org/emacs/InteractivelyDoThings)
;;   For general completion
;; - [Flycheck](https://github.com/flycheck/flycheck)
;;   For error checking
;; 
;; ## Installation
;; Graphene is available on [Melpa](http://melpa.org).
;; 
;; If you don't already have your Emacs set up to use the package
;; installation system, let me gently point you to
;; [Pallet](https://github.com/rdallasgray/pallet).
;; 
;; Anyway -- your default initialisation file is in (old-school)
;; `~/.emacs` or (new-school, and where it *should* be)
;; `~/.emacs.d/init.el`. First, you need to set up the Emacs package
;; system and tell it about Melpa, so create one of those files if it
;; doesn't already exist, and add these lines to the file:
;; 
;; ```
;; ;; Require Emacs' package functionality
;; (require 'package)
;; 
;; ;; Add the Melpa repository to the list of package sources
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;; 
;; ;; Initialise the package system.
;; (package-initialize)
;; ```
;; 
;; Then either select those lines and do `M-x eval-region`, or restart
;; Emacs. After that, do `M-x list-packages`, search for
;; 'graphene' (either manually or using `C-s`), mark it for installation
;; by pressing 'i', and install it by pressing 'x'.
;; 
;; It will take a while to install itself and its various dependencies, and will
;; probably raise a few compilation issues. You can probably safely ignore
;; these. Once it's done, add this to your initialisation file:
;; 
;; ```
;; (require 'graphene)
;; ```
;; Restart Emacs, and away you go.
;; 
;; ## How do I ... ?
;; All of the packages Graphene includes are well-documented, and I'll
;; refer you to them rather than retread that documentation here. That
;; said, there are some Graphene-specific things you need to know.
;; 
;; ### Keybindings
;; Graphene creates some new keybindings, and alters some existing ones:
;; 
;; - `C-x k` always kills the active buffer, rather than asking you which
;;   one you want to kill
;; - `C-x C-k` kills the default buffer and closes its window
;; - `C-c n` creates a new buffer
;; - `C-c N` creates a new instance of Emacs
;; - `C-;` adds a semicolon at the end of the line
;; - `M-RET` creates a newline below the current line and moves to it
;; - `C-M-;` comments or uncomments the current line
;; - `C->` increases the height of the current window
;; - `C-<` decreases it
;; - `C-.` increases the width of the current window
;; - `C-,` decreases it
;; - `C-c s` selects the Speedbar window
;; 
;; Graphene used to bind the standard Mac keys for various purposes
;; (Command-n for new buffer, for instance), but no longer does.
;; 
;; ### Projects
;; [project-persist](https://github.com/rdallasgray/project-persist) uses
;; the following keybindings:
;; 
;; - `C-c P n` to create a new project
;; - `C-c P f` to find an existing project
;; - `C-c P k` to close the current project
;; - `C-c P d` to delete an existing project
;; 
;; ### Customising
;; Try `M-x customize-group` and type 'graphene', for an idea of what can
;; be customised; you may wish to set these programmatically in your init
;; file, instead.
;; 
;; ## Contributions and feedback
;; Contributions to Graphene are very welcome, as are feedback and bug
;; reports. The latter can be raised via the Issues section.
;; 
;; To contribute code, fork and clone the repo. If you want to be able to
;; build the package, run `git submodule update --init`,
;; which will install [el.mk](http://github.com/rdallasgray/el.mk), then
;; [install Cask](https://github.com/cask/cask) and run `cask install`.
;; 
;; When you've created your feature, make a pull request against master
;; in this repo.

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

(defgroup graphene nil
  "Graphene custom settings."
  :group 'environment)

(require 'graphene-helper-functions)
(require 'graphene-editing)
(require 'graphene-env)
(require 'graphene-projects)
(require 'graphene-look)

(provide 'graphene)

;;; graphene.el ends here
