(use-package cuda-mode
  :ensure t
  :mode ("\\.cu\\'"
         "\\.cuh\\'")
  :init
  (eval-after-load "irony"
    '(progn
       (push 'cuda-mode irony-supported-major-modes))))
