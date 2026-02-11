;;; init.el --- Point d'entrée -*- lexical-binding: t; -*-
;;; Code:

;; --- Cache/state dans var/ ---
(defconst user-cache-directory (expand-file-name "var/" user-emacs-directory))
(dolist (dir '("backup/" "auto-save/" "savehist/"))
  (make-directory (expand-file-name dir user-cache-directory) t))

(setq backup-directory-alist
      `(("." . ,(expand-file-name "backup/" user-cache-directory))))

(setq custom-file (expand-file-name "custom.el" user-cache-directory))
(when (file-exists-p custom-file)
  (load custom-file 'noerror 'nomessage))

;; --- Bootstrap straight.el ---
(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el"
                                        user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; --- straight + use-package ---
(setq straight-enable-use-package-integration t
      straight-use-package-by-default t
      use-package-always-ensure nil) ; ne pas utiliser package.el

(require 'use-package) ; Emacs 29 l’inclut

;; Built-ins : ne pas les installer
(use-package project :ensure nil)
(use-package eglot   :ensure nil)

;; --- Modules perso ---
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'core-packages)
(require 'core-ui)
(require 'core-editing)
(require 'core-completion)
(require 'core-project)
(require 'core-vcs)
(require 'core-org)
(require 'core-prog)
(require 'core-term)
(require 'core-misc)

;; Revenir à un GC raisonnable après le démarrage
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 64 1024 1024)
                  gc-cons-percentage 0.1)
            (message "Emacs ready in %.2fs with %d GCs"
                     (float-time (time-subtract after-init-time before-init-time))
                     gcs-done)))




(provide 'init)
;;; init.el ends here
