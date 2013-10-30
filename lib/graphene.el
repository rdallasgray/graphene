;;; graphene.el --- Newbie-friendly defaults
;;
;; Copyright (c) 2013 Robert Dallas Gray
;;
;; Author: Robert Dallas Gray <mail@robertdallasgray.com>
;; URL: https://github.com/rdallasgray/graphene
;; Version: 0.4.0
;; Keywords: defaults

;; This file is not part of GNU Emacs.

;;; Commentary:

;; What is Graphene?
;; ================= 
;; It's a set of defaults for Emacs which
;; should be friendly to users migrating from Textmate, Sublime Text or the like.
;; 
;; I don't pretend to be an Emacs expert -- I've only been using it for a year or
;; so -- but I've found that after experiencing its capabilities I'm not able to go
;; back to Textmate, and I'm reluctant to throw my lot in with another
;; closed-source editor, for all sorts of reasons.
;; 
;; At the same time, neither base-level Emacs nor any of the defaults packages I've
;; tried ([Prelude](https://github.com/bbatsov/prelude),
;; [emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit), etc.)
;; have made Emacs work (and look) the way I want it to.
;; 
;; Graphene brings together what for me are the essentials. It is a little
;; opinionated, but still more liberal than I've found other similiar packages to
;; be.
;; 
;; ![Graphene screenshot](http://s3-eu-west-1.amazonaws.com/graphene/graphene.png)
;; 
;; What does it include?
;; =====================
;; 1. **A useful project mode**  
;;    There are several project-management packages available for Emacs. All that I
;;    really want from such a package, though, is to be able to easily load and
;;    switch among different projects without losing my place. For this reason,
;;    Graphene uses
;;    [project-persist](https://github.com/rdallasgray/project-persist), around
;;    which it puts in place hooks to load and save the Emacs desktop in tandem
;;    with project settings. Because this approach is so lightweight, it does not
;;    interfere with other project-management capabilities, and in fact Graphene
;;    takes advantage of this by using
;;    [Projectile's](https://github.com/bbatsov/projectile) excellent
;;    search-in-project functionality, for instance.
;;    
;; 2. **An attractive and functional 'project drawer'**  
;;    I missed this from Textmate. It helps me, when looking at a large project, to
;;    get a quick visual sense of where things are, and to find things when I don't
;;    immediately know where to look for them. Also (*ducks*), I sometimes like to
;;    take a break from the keyboard and click around with the mouse. It gives my
;;    back and my hands a break from the typing position. Graphene uses
;;    [Sr-Speedbar](https://github.com/emacsmirror/sr-speedbar) to create a
;;    Speedbar inside the working frame, with some tweaks to the Speedbar look to
;;    make it more ... acceptable.
;;    
;; 3. **An immediately usable and discoverable Emacs environment**  
;;    At first Emacs can appear a little opaque; it is in fact a very discoverable
;;    environment, and Graphene tries to turn this up to maximum, by using
;;    [Ido](http://emacswiki.org/emacs/InteractivelyDoThings) everywhere, the
;;    Ido-like [Smex](http://www.emacswiki.org/Smex) for running extended commands,
;;    and [Auto Complete](http://emacswiki.org/emacs/AutoComplete) for in-editor
;;    completion. These allow gradual discovery of Emacs' functionality, and
;;    gradual building of speed and fluidity.
;;    
;; 4. **Sundry other timesavers, helpers and UI improvements**  
;;    Graphene will also:
;;    - Save the size and position of your window (frame) on quit, and restore it
;;      on restart;
;;    - Allow you to create a new buffer with a standard UI shortcut (cmd-n on
;;      macs, C-c n on other platforms);
;;    - Pair brackets, braces, etc. (using
;;      [Smartparens](https://github.com/Fuco1/smartparens));
;;    - Set nicer default fonts, appropriate to the platform you're working on;
;;    - Tone down and rationalise some common theme settings, without interfering
;;      too much with the overall theme's look.
;;    
;; Users may eventually want to read through the code to find out precisely what
;; Graphene is doing -- there's too much to list here without making the README
;; confusing.
;; 
;; Who is it for?
;; ============== 
;; Primarily, people who have been Textmate or
;; Sublime Text users, and who want to give Emacs a try but have been daunted by
;; its perceived complexity.
;; 
;; Graphene intentionally doesn't do too much -- it just sets up some defaults
;; which should make Emacs a little more familiar to start with, and provides some
;; functionality to let new users find their way around more easily.
;; 
;; How do I install it?
;; ==================== 
;; Short answer: Graphene is available
;; on [Melpa](http://melpa.milkbox.net).
;; 
;; Long answer: your default initialisation file is in (old-school) `~/.emacs` or
;; (new-school) `~/.emacs.d/init.el`. First, you need to set up the Emacs package
;; system and tell it about Melpa, so create one of those files if it doesn't
;; already exist, and add these lines to the file: ``` ;; Require Emacs' package
;; functionality (require 'package) ;; Add the Melpa repository to the list of
;; package sources (add-to-list 'package-archives '("melpa"
;; . "http://melpa.milkbox.net/packages/") t) ;; Initialise the package system.
;; (package-initialize) ``` Then either select those lines and do `M-x
;; eval-region`, or restart Emacs. After that, do `M-x package-list-packages`,
;; search for 'graphene' (either manually or using `C-s`), mark it for installation
;; by pressing 'i', and install it by pressing 'x'.
;; 
;; It will take a while to install itself and its various dependencies, and will
;; probably raise a few compilation issues. You can most probably safely ignore
;; these. Once it's done, add this to your initialisation file: ``` (require
;; 'graphene) ``` Restart Emacs, and away you go.
;; 
;; I'm lost.
;; ========
;; To open the project drawer:  
;; `M-x sr-speedbar-open`
;; 
;; To create a project:  
;; `M-x project-persist-create` or `C-c P n`
;; 
;; To open a project:  
;; `M-x project-persist-find` or `C-c P f`
;; 
;; What's all this M-x, C-c, aargh?  
;; ================================ 
;; You probably need to run the Emacs Tutorial: `C-h t`
;; 
;; (That's hold down the CTRL key, press 'h', release those, then press 't'.)
;; 
;; Stop patronising me.  
;; ==================== 
;; OK. You can customise some basic defaults by doing `M-x customize-group` and
;; selecting `graphene`. At present you can determine whether Graphene does certain
;; things automatically, like opening the Speedbar when you open a project, and
;; applying certain modes when you enter a programming mode.
;; 
;; The latter of these is quite interesting: Emacs' list of 'prog modes' excludes
;; some modes, like css-mode and sgml-mode, which you may actually wish to treat as
;; prog modes for certain purposes. Graphene allows you to do this by adding mode
;; hooks to the list `graphene-prog-mode-hooks`, like so: 
;; ``` (push 'css-mode-hook graphene-prog-mode-hooks) ``` 
;; Or: 
;; ``` (setq graphene-prog-mode-hooks '(prog-mode-hook sgml-mode-hook)) ``` 
;; Or (bearing in mind that `graphene-prog-mode-hooks` contains `prog-mode-hook` by default): 
;; ``` (setq graphene-prog-mode-hooks 
;;       (append '(css-mode-hook sgml-mode-hook) graphene-prog-mode-hooks) ```
;; 
;; The default value for `graphene-prog-mode-hooks` is:
;; ```'(csharp-mode-hook coffee-mode-hook css-mode-hook sgml-mode-hook html-mode-hook)```
;; 
;; By default, the `graphene-prog-mode-hook` enables `linum-mode`, `smartparens-mode` 
;; and `autocomplete-mode`, and rebinds the return key to `newline-and-indent`. 
;; All but the latter can be disabled using the customize settings.
;; 
;; You can add your own functions to run on entering a de facto prog mode by adding
;; hook functions to `graphene-prog-mode-hook`: 
;; ``` (add-hook 'graphene-prog-mode-hook 'my-hook-function) ```
;; 
;; Is there a Graphene 'philosophy'?  
;; ================================= 
;; Graphene is intended to be simple and liberal (it doesn't do too much, and it
;; doesn't prevent you from working in any particular way). Its liberalism may be
;; interpreted by some as 'anti-Emacs', but I would respectfully respond that the
;; *whole point* of Emacs is its configurability. I hope very much that Graphene
;; helps others to discover the wonder of Emacs.
;; 
;; A second important point is that wherever possible Graphene includes projects
;; under active development in preference to ones which appear dead, and will
;; *only* include code available via Emacs' package system.
;; 
;; What's to come?  
;; =============== 
;; More customisability, more documentation, possibly a dedicated menu to allow 
;; easy access to custom settings and functionality.
;; 
;; I installed Graphene, but it doesn't look like your screenshot ...
;; ================================================================== 
;; I use this version of the [Solarized theme](https://github.com/sellout/emacs-color-theme-solarized) (available on Melpa).

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

;; Set up the system-based defaults
(let ((graphene-sys
  (cond ((eq system-type 'darwin) "osx")
        ((eq system-type 'gnu/linux) "linux")
        ((eq system-type 'windows-nt) "windows")
        (t "other"))))
  (defvar graphene-sys-defaults (intern (format "graphene-%s-defaults" graphene-sys))
    "Symbol for the specific system-based defaults file."))


;; Define custom settings
(defgroup graphene nil
  "Graphene custom settings.")

(defcustom graphene-speedbar-auto t
  "Whether graphene should open sr-speedbar when a project is loaded."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-project-pin-speedbar t
  "Pin the speedbar directory when opening a project."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-linum-auto t
  "Whether graphene should enable linum-mode with prog-modes."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-autopair-auto t
  "Whether graphene should enable smartparens with prog-modes."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-autopair-ruby t
  "Whether graphene should enable special smartparens Ruby pairs."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-autocomplete-auto t
  "Whether graphene should enable autocomplete-mode with prog-modes."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-parens-auto t
  "Whether graphene should enable show-paren-mode."
  :type 'boolean
  :group 'graphene)

(defcustom graphene-prog-mode-hooks
  '(prog-mode-hook
    csharp-mode-hook
    coffee-mode-hook
    css-mode-hook
    sgml-mode-hook
    html-mode-hook)
  "List of hooks to be treated as prog-mode."
  :type 'sexp
  :group 'graphene)

(defcustom graphene-speedbar-refresh-hooks '(after-save-hook)
  "List of hooks which on being run will cause speedbar to refresh."
  :type 'sexp
  :group 'graphene)

(defcustom graphene-default-font nil
  "The universal default font."
  :type 'string
  :group 'graphene)

(defcustom graphene-variable-pitch-font nil
  "The font to use in the variable-pitch face."
  :type 'string
  :group 'graphene)

(defcustom graphene-fixed-pitch-font nil
  "The font to use in the fixed-pitch face."
  :type 'string
  :group 'graphene)

(defcustom graphene-default-font nil
  "The universal default font."
  :type 'string
  :group 'graphene)

(defvar graphene-prog-mode-hook nil
  "A hook to be run on entering a de facto prog mode.")

;; Require necessary files
(require 'graphene-helper-functions)
(require 'graphene-editing)
(require 'graphene-env)
(require 'graphene-speedbar)
(require 'graphene-projects)
(require 'graphene-keys)
(require graphene-sys-defaults)
(require 'graphene-look)

(provide 'graphene)
