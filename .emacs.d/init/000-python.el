;;-----------------------
;; python-pyx-mode
;;-----------------------
(define-derived-mode python-pyx-mode
  python-mode "Pyx"
  "Major mode for pyx.
  \\{python-pyx-mode-map}")

(font-lock-add-keywords
 'python-pyx-mode
 '(("\\<\\(cimport\\|cdef\\|cpdef\\)\\>" . font-lock-keyword-face)))


;;-----------------------
;; Python
;;-----------------------
(use-package python
  :ensure t
  :mode (("\\.wsgi\\'" . python-mode)
         ("\\.pyx\\'" . python-pyx-mode)
         ("\\.pxd\\'" . python-pyx-mode)
         ("\\.pxi\\'" . python-pyx-mode))
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
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)))


;;-----------------------
;; Misc
;;-----------------------
;; Mode name map
(add-hook
 'after-init-hook
 (lambda()
   (add-to-list 'niboshi-mode-name-alist '(python-mode . "PY"))))

;;-----------------------
;; Open egg files
;;-----------------------
(add-to-list 'auto-mode-alist '("\\.egg\\'" . archive-mode))

;;-----------------------
;; Syntax checkers
;;-----------------------
(use-package py-autopep8 :ensure t)
(use-package flycheck-pyflakes :ensure t)
