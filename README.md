psession
========

You can  [![Support via Gratipay](https://cdn.rawgit.com/gratipay/gratipay-badge/2.1.3/dist/gratipay.png)](https://gratipay.com/thierryvolpiatto) or [![Donate](https://www.paypalobjects.com/en_US/i/btn/btn_donate_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=thierry.volpiatto@gmail.com&lc=US&currency_code=EUR&bn=PP-DonationsBF:btn_donateCC_LG.gif:NonHosted) to help this project.

# Description
Yet another package for emacs persistent sessions

# Commentary:

Less featured than other alternatives, but do it faster with a minimal configuration.
All objects are saved individually in compiled files.

# Features:

- Save and restore all your buffers
- Save and restore your last window configuration
- Save and restore value of any vars
- Save and restore registers except windows register
  (you can save your windows configs though with M-x psession-save-winconf)

# Install:

Add "psession.el" to `load-path` (not needed when installed from Emacs packaging system)

Customize at least `psession-object-to-save-alist`.

Add to init file:

If installed from git

    (autoload 'psession-mode "psession.el")
    (psession-mode 1)

Otherwise only `(psession-mode 1)` is needed.


