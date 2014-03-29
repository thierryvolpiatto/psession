psession
========

Yet another package for emacs persistent sessions

#Commentary:

Less featured than other alternatives, but do it faster with a minimal configuration.
All objects are saved individually in compiled files.

#Features:

- Save and restore all your buffers
- Save and restore your last window configuration
- Save and restore value of any vars
- Save and restore registers except windows register
  (you can save your windows configs though with M-x psession-save-winconf)

#Install:

Add "persistent-sessions.el" to `load-path`

Customize at least `psession-object-to-save-alist`.

Add to init file:

```elisp
(autoload 'psession-mode "persistent-sessions.el")
(psession-mode 1)
```

