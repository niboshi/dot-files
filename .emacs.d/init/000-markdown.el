;;-----------------------
;; Markdown
;;-----------------------
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook
            (lambda()
            ;; Override markdown preview filename
            (defun markdown-export-file-name (&optional extension)
              (when (buffer-file-name)
                (unless extension
                  (setq extension ".html"))
                (concat
                 (file-name-as-directory temporary-file-directory)
                 (file-name-as-directory "emacs-markdown")
                 (file-name-nondirectory (buffer-file-name))
                 extension)))

            ;; Select available markdown renderer
            (let ((command-succeeded (lambda(cmd) (eq 0 (call-process shell-file-name nil nil nil shell-command-switch cmd)))))
              (setq markdown-command
                    (cond
                     ((funcall command-succeeded "markdown --help") "markdown")
                     ((funcall command-succeeded "python -c \"import markdown2\"") "python -m markdown2 --extras fenced-code-blocks")
                     ((funcall command-succeeded "python -c \"import markdown\"") "python -m markdown -x markdown.extensions.fenced_code")
                     (t nil)))))))
