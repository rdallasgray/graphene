#Graphene
Graphene is a 'starter kit' for Emacs, in the vein of
[Prelude](https://github.com/bbatsov/prelude) or
[emacs-starter-kit](https://github.com/technomancy/emacs-starter-kit).

![Graphene screenshot](http://s3-eu-west-1.amazonaws.com/graphene/graphene.png)

It provides:
- Some sensible defaults
- A clean look
- The basics you need to make Emacs functionality discoverable
- A set of third-party packages that set the standard for the
  essential functionality they provide
- Some plumbing to make the above 'just work', seamlessly and silently

Although Graphene is intended to help users of GUI editors such as
Textmate or Sublime Text find their feet quickly, it is not an attempt
to turn Emacs into a sparkly GUI editor or IDE. It is minimal,
lightweight, and respectful of the history and character of Emacs.

##Sensible defaults
Among many other things, Graphene turns off the ugly Emacs startup
screen, turns on line wrapping, turns off the scroll bars and tool
bar (and menu bar on non-OS X systems), moves automatic backups into
the temp directory -- generally clears the way of small annoyances and
makes things look and work the way you'd expect.

##A clean look
Graphene includes its own 'meta-theme' which works hard to unify the
look of the editor across a range of packages. This theme loads on top
of any 'normal' theme you want to load, so you can still choose
exactly how you want your Emacs to look.

It also sets more pleasant and modern default fonts appropriate to the
host platform (which can be overridden), and maintains window size and
position across sessions.

##Discoverability
At first Emacs can appear a little opaque; it is in fact a very
discoverable environment, and Graphene tries to turn this up to
maximum, by using
[Ido](http://emacswiki.org/emacs/InteractivelyDoThings) everywhere,
the Ido-like [Smex](http://www.emacswiki.org/Smex) for running
extended commands, and
[Auto Complete](http://emacswiki.org/emacs/AutoComplete) for in-editor
completion. These allow gradual discovery of Emacs' functionality, and
gradual building of speed and fluidity.

##Essential packages
The collection of packages Graphene includes prevents you having to
research and discover on your own what the Emacs community has largely
decided on as best-in-class packages.

- [project-persist](https://github.com/rdallasgray/project-persist)
  Disclaimer: this is my own project, and is perhaps the exception to the
  above rule. It provides simple project loading and saving;
  Graphene adds a project 'drawer' using
  [Sr-Speedbar](https://github.com/emacsmirror/sr-speedbar)
- [Smartparens](https://github.com/Fuco1/smartparens)
  For auto-pairing
- [Auto Complete](http://emacswiki.org/emacs/AutoComplete)
  For code completion
- [Web-mode](https://github.com/fxbois/web-mode)
  For mixed-mode editing
- [Smex](http://www.emacswiki.org/Smex)
  For command completion
- [Ido](http://emacswiki.org/emacs/InteractivelyDoThings)
  For general completion
- [Flycheck](https://github.com/flycheck/flycheck)
  For error checking

##Installation
Graphene is available on [Melpa](http://melpa.milkbox.net).

If you don't already have your Emacs set up to use the package
installation system, let me gently point you to
[Pallet](https://github.com/rdallasgray/pallet).

Anyway -- your default initialisation file is in (old-school) `~/.emacs` or
(new-school) `~/.emacs.d/init.el`. First, you need to set up the Emacs package
system and tell it about Melpa, so create one of those files if it doesn't
already exist, and add these lines to the file:

```
;; Require Emacs' package functionality
(require 'package)

;; Add the Melpa repository to the list of package sources
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Initialise the package system.
(package-initialize)
```

Then either select those lines and do `M-x eval-region`, or restart
Emacs. After that, do `M-x list-packages`, search for
'graphene' (either manually or using `C-s`), mark it for installation
by pressing 'i', and install it by pressing 'x'.

It will take a while to install itself and its various dependencies, and will
probably raise a few compilation issues. You can most probably safely ignore
these. Once it's done, add this to your initialisation file:

```
(require 'graphene)
```
Restart Emacs, and away you go.

##How do I ... ?
All of the packages Graphene includes are well-documented, and I'll
refer you to them rather than retread that documentation here. If
there's something you don't understand, please raise an issue here and
I'll consider adding more documentation.
