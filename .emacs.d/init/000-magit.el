;;-----------------------
;; magit
;;-----------------------
(use-package magit
  :ensure t
  :defer t
  :init
  (niboshi-set-key (kbd "C-c m m") 'magit-status)
  (niboshi-set-key (kbd "C-c m s") 'magit-status)
  (niboshi-set-key (kbd "C-c m l") 'magit-log-buffer-file)
  (niboshi-set-key (kbd "C-c m L") 'magit-log-current)
  (niboshi-set-key (kbd "C-c m b") 'magit-blame)
  (setq magit-save-repository-buffers nil)
  :config
  (let ((background-highlight "gray10"))
    (set-face-attribute 'magit-diff-context-highlight nil :foreground nil       :background background-highlight)
    (set-face-attribute 'magit-diff-added             nil :foreground "green"   :background nil)
    (set-face-attribute 'magit-diffstat-added         nil :foreground "green"   :background nil)
    (set-face-attribute 'magit-diff-removed           nil :foreground "red"     :background nil)
    (set-face-attribute 'magit-diffstat-removed       nil :foreground "red"     :background nil)
    (set-face-attribute 'magit-diff-added-highlight   nil :foreground "green"   :background background-highlight)
    (set-face-attribute 'magit-diff-removed-highlight nil :foreground "red"     :background background-highlight)
    (set-face-attribute 'magit-diff-file-heading      nil :foreground "#ccccff" :background "#006699")

    ;; Remove unwanted background highlight in smerge mode
    (set-face-attribute 'smerge-refined-added         nil :foreground nil       :background nil)
    (set-face-attribute 'smerge-refined-removed       nil :foreground nil       :background nil)
    ))
