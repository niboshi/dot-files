(use-package cmake-mode
  :ensure t
)

(add-hook 'cmake-mode-hook
          (lambda ()
            (setq cmake-tab-width 4)))
