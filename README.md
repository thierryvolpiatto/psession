psession
========

You can [![Donate](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=thierry.volpiatto@gmail.com&lc=US&currency_code=EUR&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted) to help this project.

# Description
Yet another package for emacs persistent sessions

# Commentary:

Less featured than other alternatives, but do it faster with a minimal configuration.
All objects are saved individually in compiled files.

# Features:

- Save and restore all your buffers
- Save and restore your last window configuration
- Save and restore value of any vars
- Save automatically history variables (replacement of savehist-mode)
- Save and restore registers except windows register
  (you can save your windows configs though with M-x psession-save-winconf)
- Autosave your session

Saving is done when quitting emacs (run in kill-emacs-hook), however you can save at regular time 
with `psession-auto-save` set to a non nil value if you want to have your session saved 
with your recentest changes in case emacs crashes.

# Install:

Add "psession.el" to `load-path` (not needed when installed from Emacs packaging system)

Customize at least `psession-object-to-save-alist`, don't add here minibuffer history variables,
instead enable `psession-savehist-mode` which will add these variables automatically.

Add to init file:

If installed from git

    (require 'psession)
    (psession-mode 1)

Otherwise (Melpa install) only `(psession-mode 1)` is needed.

For saving minibuffer history, use `(psession-savehist-mode 1)` as a replacement of `savehist-mode`.

If you want to save periodically (autosave) your emacs session, add `(psession-autosave-mode 1)` to your init file.
