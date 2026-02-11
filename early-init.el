;;; early-init.el --- Chargé avant init.el (Emacs 27+) -*- lexical-binding: t; -*-

;; Accélérer le démarrage et éviter le flicker
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t
      frame-inhibit-implied-resize t
      package-enable-at-startup nil) ; on gère les paquets nous-mêmes

;; Évite des pauses liées au cache des polices
(setq inhibit-compacting-font-caches t)

;; Scratch propre
(setq initial-scratch-message nil
      initial-major-mode 'fundamental-mode)

;; GC agressif pendant le chargement + perf I/O (LSP/JSON)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      read-process-output-max (* 4 1024 1024)) ; 4Mo pour LSP

;; UI minimaliste dès le début
(when (fboundp 'tool-bar-mode)   (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode)   (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(setq frame-resize-pixelwise t)

;; Optionnel: fenêtre sans décoration (peut être pénible/buggy sous Windows)
(unless (eq system-type 'windows-nt)
  (add-to-list 'default-frame-alist '(undecorated . t)))

(provide 'early-init)
;;; early-init.el ends here
