;;; persistent-sessions.el --- Persistent save of elisp objects. -*- lexical-binding: t -*-

;; Copyright (C) 2012 ~ 2013 Thierry Volpiatto <thierry.volpiatto@gmail.com>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Code:

(eval-when-compile (require 'dired)
                   (require 'cl-lib))


(defgroup psession nil
  "Persistent sessions."
  :group 'frames)

(defcustom psession-elisp-objects-default-directory "~/.emacs.d/elisp-objects/"
  "The directory where lisp objects will be stored."
  :group 'psession
  :type 'string)

(defcustom psession-object-to-save-alist
  '((ioccur-history . "ioccur-history.el")
    (extended-command-history . "extended-command-history.el")
    (helm-external-command-history . "helm-external-command-history.el")
    (helm-surfraw-engines-history . "helm-surfraw-engines-history.el")
    (psession--save-buffers-alist . "tv-save-buffers-alist.el")
    (helm-ff-history . "helm-ff-history.el")
    (helm-grep-history . "helm-grep-history.el")
    (kill-ring . "kill-ring.el")
    (kill-ring-yank-pointer . "kill-ring-yank-pointer.el")
    (register-alist . "register-alist.el")
    (psession--winconf-alist . "psession-winconf-alist.el"))
  "Alist of vars to save persistently.
It is composed of (var_name . \"var_name.el\")"
  :group 'psession
  :type '(alist :key-type symbol :value-type string))

(defcustom psession-save-buffers-unwanted-buffers-regexp ".*[.]org$\\|diary$\\|[.]newsticker-cache$"
  "Regexp matching buffers you don't want to save."
  :group 'psession
  :type 'string)

;;; The main function to save objects to byte compiled file.
;;
;; Each object have its own compiled file.
(defun psession--dump-object-to-file (obj file)
  "Save symbol object OBJ to the byte compiled version of FILE.
OBJ can be any lisp object, list, hash-table, etc...
Windows configurations and markers are not supported.
FILE must be an elisp file with ext \"*.el\" (NOT \"*.elc\").
Loading the *.elc file will restitute object.
That may not work with Emacs versions <=23.1 for hash tables."
  (require 'cl-lib) ; Be sure we use the CL version of `eval-when-compile'.
  (assert (not (file-exists-p file)) nil
          (format "dump-object-to-file: File `%s' already exists, please remove it." file))
  (unwind-protect
       (let ((print-length           nil)
             (print-level            nil)
             (print-circle           t)
             (print-escape-nonascii  t)
             (print-escape-multibyte t))
         (with-temp-file file
           (prin1 `(setq ,obj (eval-when-compile ,obj)) (current-buffer)))
         (byte-compile-file file)
         (message "`%s' dumped to %sc" obj file))
    (delete-file file)))

;;; Objects (variables to save)
;;
;;
(defun psession--dump-object-to-file-save-alist ()
  (when psession-object-to-save-alist
    (cl-loop for (o . f) in psession-object-to-save-alist
             for abs = (expand-file-name f psession-elisp-objects-default-directory)
             ;; Registers are treated specially.
             if (and (eq o 'register-alist)
                     (symbol-value o))
             do (psession--dump-object-save-register-alist f)
             else do
             ;; Don't dump object when it is nil or not bound.
             (when (and (boundp o) (symbol-value o))
               (psession--dump-object-to-file o abs)))))

(cl-defun psession--restore-objects-from-directory
    (&optional (dir psession-elisp-objects-default-directory))
  (let ((file-list (cddr (directory-files dir t))))
    (cl-loop for file in file-list do (and file (load file)))))

(cl-defun psession--dump-object-save-register-alist (&optional (file "register-alist.el"))
  "Save `register-alist' but only supported objects."
  (let ((register-alist (cl-loop for (char . val) in register-alist
                                 unless (or (markerp val)
                                            (vectorp val)
                                            (and (consp val) (window-configuration-p (car val))))
                                 collect (cons char val)))
        (def-file (expand-file-name file psession-elisp-objects-default-directory)))
    (psession--dump-object-to-file 'register-alist def-file)))

;;; Persistents window configs
;;
;;
(defconst psession--last-winconf "last_session5247")
(defvar psession--winconf-alist nil)
(defun psession--window-name ()
  (let (result)
    (walk-windows (lambda (w) (pushnew (buffer-name (window-buffer w)) result)))
    (mapconcat 'identity result " | ")))

;;;###autoload
(defun psession-save-winconf (place)
  (interactive (list (let ((name (psession--window-name)))
                       (read-string (format "Place (%s) : " name) nil nil name))))
  (let ((assoc (assoc place psession--winconf-alist))
        (new-conf (list (cons place (window-state-get nil 'writable)))))
    (if assoc
        (setq psession--winconf-alist (append new-conf
                                             (delete assoc psession--winconf-alist)))
        (setq psession--winconf-alist (append new-conf psession--winconf-alist)))))

;;;###autoload
(defun psession-restore-winconf (conf)
  (interactive (list (completing-read
                      "WinConfig: "
                      (sort (mapcar 'car psession--winconf-alist) #'string-lessp))))
  (delete-other-windows)
  (window-state-put (cdr (assoc conf psession--winconf-alist))))

;;;###autoload
(defun psession-delete-winconf (conf)
  (interactive (list (completing-read
                      "WinConfig: "
                      (sort (mapcar 'car psession--winconf-alist) #'string-lessp))))
  (let ((assoc (assoc conf psession--winconf-alist)))
    (setq psession--winconf-alist (delete assoc psession--winconf-alist))))

(defun psession-save-last-winconf ()
  (psession-save-winconf psession--last-winconf))

(defun psession-restore-last-winconf ()
  (psession-restore-winconf psession--last-winconf))

;;; Persistents-buffer 
;;
;;
(defun psession--save-some-buffers ()
  (cl-loop with dired-blist = (cl-loop for (_f . b) in dired-buffers
                                       when (buffer-name b)
                                       collect b)
           with blist = (append (buffer-list) dired-blist)
           for b in blist
           for buf-fname = (or (buffer-file-name b) (car (rassoc b dired-buffers)))
           for place = (with-current-buffer b (point))
           when (and buf-fname
                     (not (string-match tramp-file-name-regexp buf-fname))
                     (not (string-match  psession-save-buffers-unwanted-buffers-regexp
                                         buf-fname))
                     (file-exists-p buf-fname))
           collect (cons buf-fname place)))

(defvar psession--save-buffers-alist nil)
(defun psession--dump-some-buffers-to-list ()
  (setq psession--save-buffers-alist (psession--save-some-buffers)))

(defun psession--restore-some-buffers ()
  (let* ((max (length psession--save-buffers-alist))
         (progress-reporter (make-progress-reporter "Restoring buffers..." 0 max)))
    (cl-loop for (f . p) in psession--save-buffers-alist
             for count from 0
             do
             (with-current-buffer (find-file-noselect f 'nowarn)
               (goto-char p)
               (push-mark p 'nomsg)
               (progress-reporter-update progress-reporter count)))
    (progress-reporter-done progress-reporter)))

;;;###autoload
(define-minor-mode psession-mode
    "Persistent emacs sessions."
  :global t
  (if psession-mode
      (progn
        (unless (file-directory-p psession-elisp-objects-default-directory)
          (make-directory psession-elisp-objects-default-directory t))
        (add-hook 'kill-emacs-hook 'psession--dump-object-to-file-save-alist)
        (add-hook 'emacs-startup-hook 'psession--restore-objects-from-directory)
        (add-hook 'kill-emacs-hook 'psession--dump-some-buffers-to-list)
        (add-hook 'emacs-startup-hook 'psession--restore-some-buffers 'append)
        (add-hook 'kill-emacs-hook 'psession-save-last-winconf)
        (add-hook 'emacs-startup-hook 'psession-restore-last-winconf 'append))
      (remove-hook 'kill-emacs-hook 'psession--dump-object-to-file-save-alist)
      (remove-hook 'emacs-startup-hook 'psession--restore-objects-from-directory)
      (remove-hook 'kill-emacs-hook 'psession--dump-some-buffers-to-list)
      (remove-hook 'emacs-startup-hook 'psession--restore-some-buffers)
      (remove-hook 'kill-emacs-hook 'psession-save-last-winconf)
      (remove-hook 'emacs-startup-hook 'psession-restore-last-winconf)))


(provide 'persistent-sessions)

;;; persistent-sessions.el ends here
