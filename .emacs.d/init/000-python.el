;;-----------------------
;; Python
;;-----------------------
(use-package python
  :mode (("\\.wsgi\\'" . python-mode))
  :config
  ;; Define run-python3 function
  (defun run-python3() (interactive)
    (let ((python-shell-interpreter "python3"))
      (call-interactively 'run-python))))


;;-----------------------
;; python-mode-hook
;;-----------------------
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent-offset 4)))

;;-----------------------
;; Jedi
;;-----------------------
(use-package jedi
  :commands jedi:setup
  :init
  (defvar jedi:complete-on-dot t)
  (defvar jedi:use-shortcuts t)
  (add-hook 'python-mode-hook 'jedi:setup))


;;-----------------------
;; Misc
;;-----------------------
;; Mode name map
(add-hook
 'after-init-hook
 (lambda()
   (add-to-list 'niboshi-mode-name-alist '(python-mode . "PY"))))
