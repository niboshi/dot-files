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
(defvar niboshi-missing-flake8-message-shown nil)
(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil)
            (setq tab-width 4)
            (setq python-indent-offset 4)
            (setq electric-indent-mode nil)
            (unless niboshi-missing-flake8-message-shown
              (unless (locate-file "flake8" exec-path)
                (message-box "Warning: flake8 is not available!!")
                (setq niboshi-missing-flake8-message-shown t)
                ))))

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
