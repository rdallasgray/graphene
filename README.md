What is Graphene?
=================
It's a set of defaults for Emacs which should be friendly to users migrating from Textmate, Sublime Text or the like.

I don't pretend to be an Emacs expert -- I've only been using it for a year or so -- but I've found that after experiencing its capabilities I'm not able to go back to Textmate, and I'm reluctant to throw my lot in with another closed-source editor, for all sorts of reasons.

At the same time, neither base-level Emacs nor any of the defaults packages I've tried ([Prelude](https://github.com/bbatsov/prelude), [emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit), etc.) have made Emacs work (and look) the way I want it to.

Graphene brings together what for me are the essentials. It is a little opinionated, but still more liberal than I've found other similiar packages to be.

![Graphene screenshot](http://s3-eu-west-1.amazonaws.com/graphene/graphene.png)

What does it include?
=====================
1. **A useful project mode**  
   There are several project-management packages available for Emacs. All that I really want from such a package, though, is to be able to easily load and switch among different projects without losing my place. For this reason, Graphene uses [project-persist](https://github.com/rdallasgray/project-persist), around which it puts in place hooks to load and save the Emacs desktop in tandem with project settings. Because this approach is so lightweight, it does not interfere with other project-management capabilities, and in fact Graphene takes advantage of this by using [Projectile's](https://github.com/bbatsov/projectile) excellent search-in-project functionality, for instance.
   
2. **An attractive and functional 'project drawer'**  
   I missed this from Textmate. It helps me, when looking at a large project, to get a quick visual sense of where things are, and to find things when I don't immediately know where to look for them. Also (*ducks*), I sometimes like to take a break from the keyboard and click around with the mouse. It gives my back and my hands a break from the typing position. Graphene uses [Sr-Speedbar](https://github.com/emacsmirror/sr-speedbar) to create a Speedbar inside the working frame, with some tweaks to the Speedbar look to make it more ... acceptable.
   
3. **An immediately usable and discoverable Emacs environment**  
   At first Emacs can appear a little opaque; it is in fact a very discoverable environment, and Graphene tries to turn this up to maximum, by using [Ido](http://emacswiki.org/emacs/InteractivelyDoThings) everywhere, the Ido-like [Smex](http://www.emacswiki.org/Smex) for running extended commands, and [Auto Complete](http://emacswiki.org/emacs/AutoComplete) for in-editor completion. These allow gradual discovery of Emacs' functionality, and gradual building of speed and fluidity.
   
4. **Sundry other timesavers, helpers and UI improvements**  
   Graphene will also:
   - Save the size and position of your window (frame) on quit, and restore it on restart;
   - Allow you to create a new buffer with a standard UI shortcut (cmd-n on macs, C-x n on other platforms);
   - Pair brackets, braces, etc. (using [Autopair](https://github.com/capitaomorte/autopair));
   - Set nicer default fonts, appropriate to the platform you're working on;
   - Tone down and rationalise some common theme settings, without interfering too much with the overall theme's look.
   
Users may eventually want to read through the code to find out precisely what Graphene is doing -- there's too much to list here without making the README confusing.

Who is it for?
==============
Primarily, people who have been Textmate or Sublime Text users, and who want to give Emacs a try but have been daunted by its perceived complexity.

Graphene intentionally doesn't do too much -- it just sets up some defaults which should make Emacs a little more familiar to start with, and provides some functionality to let new users find their way around more easily.

How do I install it?
====================
Short answer: Graphene is available on [Melpa](http://melpa.milkbox.net).

Long answer: your default initialisation file is in (old-school) `~/.emacs` or (new-school) `~/.emacs.d/init.el`. First, you need to set up the Emacs package system and tell it about Melpa, so create one of those files if it doesn't already exist, and add these lines to the file:
```
;; Require Emacs' package functionality
(require 'package)
;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
;; Initialise the package system.
(package-initialize)
```
Then either select those lines and do `M-x eval-region`, or restart Emacs. After that, do `M-x package-list-packages`, search for 'graphene' (either manually or using `C-s`), mark it for installation by pressing 'i', and install it by pressing 'x'.

It will take a while to install itself and its various dependencies, and will probably raise a few compilation issues. You can most probably safely ignore these. Once it's done, add this to your initialisation file:
```
(require 'graphene)
```
Restart Emacs, and away you go.

I'm lost.
========
To open the project drawer:  
`M-x sr-speedbar-open`

To create a project:  
`M-x project-persist-create` or `C-c P n`

To open a project:  
`M-x project-persist-find` or `C-c P f`

What's all this M-x, C-c, aargh?
================================
You probably need to run the Emacs Tutorial: `C-h t`

(That's hold down the CTRL key, press 'h', release those, then press 't'.

Is there a Graphene 'philosophy'?
=================================
Graphene is intended to be simple and liberal (it doesn't do too much, and it doesn't prevent you from working in any particular way). Its liberalism may be interpreted by some as 'anti-Emacs', but I would respectfully respond that the *whole point* of Emacs is its configurability. I hope very much that Graphene helps others to discover the wonder of Emacs.

A second important point is that wherever possible Graphene includes projects under active development in preference to ones which appear dead, and will *only* include code available via Emacs' package system.

I installed Graphene, but it doesn't look like your screenshot ...
==================================================================
I use this version of the [Solarized theme](https://github.com/sellout/emacs-color-theme-solarized) (available on Melpa).
