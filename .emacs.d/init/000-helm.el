;;-----------------------
;; helm
;;-----------------------
(use-package helm
  :ensure t
  :bind (("M-," . helm-etags-select))
  :init
  (niboshi-set-key (kbd "C-x C-r") 'helm-recentf)
  (niboshi-set-key (kbd "C-x C-b") 'helm-buffers-list))

;;-----------------------
;; helm-swoop
;;-----------------------
(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
)

;;-----------------------
;; helm-projectile
;;-----------------------
(use-package helm-projectile
  :ensure t
  :commands (helm-projectile-toggle helm-projectile-find-file)
  :bind (("C-c p a"   . helm-projectile-find-other-file)
         ("C-c p p"   . helm-projectile-switch-project)
         ("C-c p f"   . helm-projectile-find-file)
         ("C-c p s g" . helm-projectile-grep))
  :config
  (projectile-global-mode)
  (helm-projectile-toggle 1)
  )
