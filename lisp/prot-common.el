;;; prot-common.el --- Common functions for my dotemacs -*- lexical-binding: t -*-

;; Copyright (C) 2020-2023  Protesilaos Stavrou

;; Author: Protesilaos Stavrou <info@protesilaos.com>
;; URL: https://protesilaos.com/emacs/dotemacs
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1"))
;; Keywords: convenience

;;; Commentary:

;; Common functions that are used throughout my Emacs configuration.

;;; Code:

(defun prot-common-string-empty-p (string)
  "Return non-nil if STRING is empty or nil."
  (or (null string)
      (string= string "")))

(defun prot-common-trim-whitespace (string)
  "Remove leading and trailing whitespace from STRING."
  (when string
    (string-trim string)))

(defun prot-common-buffer-empty-p ()
  "Return non-nil if current buffer is empty."
  (= (point-min) (point-max)))

(defun prot-common-line-beginning-position ()
  "Return position of beginning of line without moving point."
  (save-excursion
    (beginning-of-line)
    (point)))

(defun prot-common-line-end-position ()
  "Return position of end of line without moving point."
  (save-excursion
    (end-of-line)
    (point)))

(defun prot-common-region-active-p ()
  "Return non-nil if region is active."
  (use-region-p))

(defun prot-common-file-exists-p (file)
  "Return non-nil if FILE exists."
  (and file (file-exists-p file)))

(defun prot-common-directory-exists-p (dir)
  "Return non-nil if DIR exists."
  (and dir (file-directory-p dir)))

(defun prot-common-current-time-string ()
  "Return current time as formatted string."
  (format-time-string "%Y-%m-%d %H:%M:%S"))

(defun prot-common-message-time (message)
  "Display MESSAGE prefixed with current time."
  (message "[%s] %s"
           (prot-common-current-time-string)
           message))

(provide 'prot-common)

;;; prot-common.el ends here
