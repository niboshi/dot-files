;;-----------------------
;; ido-mode (Interactive buffer switch, etc.)
;;-----------------------
(use-package ido
  :ensure t
  :commands ido-mode
  :init
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-case-fold t) ; Ignore case
  (setq ido-use-virtual-buffers t)
  (ido-mode t)
  :config
  (ido-vertical-mode t)

  ;; http://emacs.stackexchange.com/questions/3063/recently-opened-files-in-ido-mode
  (use-package recentf)
  (defun ido-recentf-open ()
    "Use `ido-completing-read' to find a recent file."
    (interactive)
    (if (find-file (ido-completing-read "Find recent file: " recentf-list))
        (message "Opening file...")
      (message "Aborting")))

  ;; http://stackoverflow.com/questions/20863386/idomenu-not-working-in-javascript-mode
  (add-hook 'js-mode-hook
            (lambda()
              (setq imenu-create-index-function
                    (lambda()
                      (save-excursion
                        (imenu--generic-function '((nil "function\\s-+\\([^ ]+\\)(" 1)
                                                   (nil "\\.\\([^\\. ]+\\)\\s-*=\\s-*function\\s-*(" 1))))))))
  )

(use-package ido-vertical-mode
  :ensure t
  :commands ido-vertical-mode
  :config
  (add-hook 'ido-setup-hook
            (lambda()
              (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
              (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
              (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
              (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))))

;; idomenu
(use-package idomenu
  :ensure t
  :bind (("C-c ; i" . idomenu)))
