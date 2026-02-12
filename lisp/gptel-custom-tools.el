;;; gptel-custom-tools.el --- gptel tools (filesystem + Emacs introspection) -*- lexical-binding: t; -*-

;; Ce fichier définit des tools pour gptel via `gptel-make-tool`.
;; À charger uniquement quand tu utilises gptel (pas dans le socle).

(eval-when-compile (require 'subr-x))
(require 'seq)

(with-eval-after-load 'gptel

  ;; --------------------------------------------------------------------------
  ;; Filesystem tools
  ;; --------------------------------------------------------------------------

  (gptel-make-tool
   :function (lambda (path filename content)
               (let* ((dir (expand-file-name (or path ".")))
                      (_ (make-directory dir t))
                      (full-path (expand-file-name filename dir)))
                 (with-temp-buffer
                   (insert (or content ""))
                   (write-region (point-min) (point-max) full-path nil 'silent))
                 (format "Created file %s" full-path)))
   :name "create_file"
   :description "Create a new file with the specified content."
   :args (list '(:name "path" :type string :description "Directory where to create the file (supports ~).")
               '(:name "filename" :type string :description "Name of the file to create.")
               '(:name "content" :type string :description "Content to write to the file."))
   :category "filesystem")

  (gptel-make-tool
   :function (lambda (filepath)
               (let ((p (expand-file-name filepath)))
                 (unless (file-exists-p p)
                   (error "File does not exist: %s" p))
                 (with-temp-buffer
                   (insert-file-contents p)
                   (buffer-string))))
   :name "read_file"
   :description "Read and return the contents of a file."
   :args (list '(:name "filepath" :type string :description "Path to the file to read (supports relative paths and ~)."))
   :category "filesystem")

  (defun my-gptel--edit-file (file-path file-edits)
    "Apply FILE-EDITS to FILE-PATH.
FILE-EDITS is a list of plists with :line_number :old_string :new_string."
    (let ((file-name (expand-file-name file-path)))
      (unless (file-exists-p file-name)
        (error "File does not exist: %s" file-name))
      (unless (and (listp file-edits) file-edits)
        (error "file-edits must be a non-empty list"))

      (with-current-buffer (get-buffer-create "*gptel-edit-file*")
        (erase-buffer)
        (insert-file-contents file-name)
        (let ((case-fold-search nil)
              (edited 0))
          (dolist (edit file-edits)
            (when-let* ((line (plist-get edit :line_number))
                        (old  (plist-get edit :old_string))
                        (new  (plist-get edit :new_string)))
              (when (and (integerp line) (>= line 1) (stringp old) (not (string-empty-p old)) (stringp new))
                (goto-char (point-min))
                (forward-line (1- line))
                (when (search-forward old (line-end-position) t)
                  (replace-match new t t)
                  (setq edited (1+ edited))))))

          (if (> edited 0)
              (progn
                ;; Diff visuel (optionnel)
                (ediff-buffers (find-file-noselect file-name) (current-buffer))
                (format "Edited %s (%d change(s) applied)" file-name edited))
            (format "No changes applied to %s" file-name))))))

  (gptel-make-tool
   :function #'my-gptel--edit-file
   :name "edit_file"
   :description "Edit a file with a list of edits (line_number + old_string -> new_string)."
   :args (list '(:name "file-path" :type string :description "Full path of the file to edit.")
               '(:name "file-edits"
                       :type array
                       :items (:type object
                                     :properties
                                     (:line_number (:type integer :description "Line number where the edit starts (1-based).")
                                      :old_string  (:type string  :description "Text to replace (must match within that line).")
                                      :new_string  (:type string  :description "Replacement text.")))
                       :description "List of edits to apply."))
   :category "filesystem")

  ;; --------------------------------------------------------------------------
  ;; Emacs tools
  ;; --------------------------------------------------------------------------

  (gptel-make-tool
   :function (lambda (buffer)
               (let ((buf (get-buffer buffer)))
                 (unless (buffer-live-p buf)
                   (error "Buffer is not live: %s" buffer))
                 (with-current-buffer buf
                   (buffer-substring-no-properties (point-min) (point-max)))))
   :name "read_buffer"
   :description "Return the contents of an Emacs buffer."
   :args (list '(:name "buffer" :type string :description "Name of the buffer to read."))
   :category "emacs")

  (gptel-make-tool
   :function (lambda (function-name)
               (let* ((sym (intern-soft function-name))
                      (doc (and sym (fboundp sym) (documentation sym t))))
                 (or doc (error "No docstring for: %s" function-name))))
   :name "get_docstring"
   :description "Get docstring for an Emacs Lisp function."
   :args (list '(:name "function-name" :type string :description "Function name as string."))
   :category "emacs")

  (gptel-make-tool
   :function (lambda (variable-name)
               (save-window-excursion
                 (describe-variable (intern variable-name))
                 (with-current-buffer "*Help*"
                   (prog1 (buffer-string)
                     (kill-buffer)))))
   :name "describe_elisp_variable"
   :description "Describe an Emacs Lisp variable."
   :args (list '(:name "variable-name" :type string :description "Variable name as string."))
   :category "emacs")

  (gptel-make-tool
   :function (lambda (key-sequence)
               (save-window-excursion
                 (describe-key (kbd key-sequence))
                 (with-current-buffer "*Help*"
                   (prog1 (buffer-string)
                     (kill-buffer)))))
   :name "describe_key_binding"
   :description "Describe what KEY-SEQUENCE runs."
   :args (list '(:name "key-sequence" :type string :description "Key sequence like \"C-x C-f\"."))
   :category "emacs")

  (gptel-make-tool
   :function (lambda ()
               (save-window-excursion
                 (describe-mode)
                 (with-current-buffer "*Help*"
                   (prog1 (buffer-string)
                     (kill-buffer)))))
   :name "describe_current_mode"
   :description "Describe current major/minor modes."
   :args nil
   :category "emacs")

  (gptel-make-tool
   :function (lambda (pattern)
               (require 'apropos)
               (save-window-excursion
                 (apropos-command pattern)
                 (with-current-buffer "*Apropos*"
                   (prog1 (buffer-string)
                     (kill-buffer)))))
   :name "apropos_command"
   :description "Search commands matching PATTERN."
   :args (list '(:name "pattern" :type string :description "Regex or string."))
   :category "emacs")

  (gptel-make-tool
   :function (lambda (pattern)
               (require 'apropos)
               (save-window-excursion
                 (apropos-variable pattern)
                 (with-current-buffer "*Apropos*"
                   (prog1 (buffer-string)
                     (kill-buffer)))))
   :name "apropos_variable"
   :description "Search variables matching PATTERN."
   :args (list '(:name "pattern" :type string :description "Regex or string."))
   :category "emacs")

  ;; Note: j'ai volontairement retiré les tools "package.el" (describe-package / package-alist)
  ;; parce que tu n'utilises pas package.el (straight). Si tu les veux, on les réécrit version straight.
  )

(provide 'gptel-custom-tools)
;;; gptel-custom-tools.el ends here
