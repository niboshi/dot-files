;;-----------------------
;; c-mode-common-hook
;;-----------------------
(add-hook 'c-mode-common-hook
          (lambda()
            (setq tab-width 4)
            (setq c-basic-offset tab-width)
            (c-set-offset 'arglist-intro '++)
            (setq indent-tabs-mode t)))

;;-----------------------
;; ggtags
;;-----------------------
(use-package ggtags
  :ensure t
  :commands ggtags-mode
  :init
  (setq ggtags-oversize-limit t)
  (add-hook 'cc-mode-common-hook
            (lambda()
              (ignore-errors (ggtags-mode t))))
  (add-hook
   'ggtags-mode-hook
   (lambda()
     (niboshi-set-key (kbd "S-<f12>") 'ggtags-find-reference)
     (niboshi-set-key (kbd "C-<f12>") 'ggtags-find-definition)
     (niboshi-set-key (kbd "<f12>")   'ggtags-find-definition)
     (niboshi-set-key (kbd "C-c g r") 'ggtags-find-reference)
     (niboshi-set-key (kbd "C-c g d") 'ggtags-find-definition)
     (niboshi-set-key (kbd "C-c g g") 'ggtags-find-tag-dwim)
     (niboshi-set-key (kbd "C-c g e") 'ggtags-find-tag-regexp)
     (niboshi-set-key (kbd "C-c g f") 'ggtags-find-file)
     (niboshi-set-key (kbd "C-c g u") 'ggtags-update-tags)
     ;; Put ggtags buffer behind
     (niboshi-set-key (kbd "C-c g -") (lambda() (interactive)
                                        (progn
                                          (let ((buf (get-buffer "*ggtags-global*")))
                                            (if buf
                                                (replace-buffer-in-windows buf))))))))
  :config
  ;; Disable some key binding
  (define-key ggtags-navigation-map (kbd "M-p") nil)
  (define-key ggtags-navigation-map (kbd "M-n") nil)
  )
