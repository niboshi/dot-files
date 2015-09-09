(require 'server)
(unless (server-running-p)
  (server-start))

;-----------------------
(add-to-list 'load-path "~/.emacs.d/lisp")

;-----------------------
; Disable original features
;-----------------------
; Disable Contrl-Z to suspend
(if (display-graphic-p)
  (global-unset-key (kbd "C-z")))
(global-unset-key (kbd "C-c C-z"))

; Disable ALT-key hook, to re-enable Windows system menu
(setq w32-pass-alt-to-system 1)

; Disable IME hotkey
(global-set-key (kbd "C-\\") 'ignore)

;-----------------------
; Essential keybinding
;-----------------------
(global-set-key (kbd "C-l") 'goto-line)
(define-key key-translation-map (kbd "C-h") "\C-?")

;-----------------------
; UI
(if (display-graphic-p)
    (progn
      (setq inhibit-splash-screen t)
      (tool-bar-mode -1)
      ))
(menu-bar-mode -1)

; Colors
(set-background-color "black")
(set-foreground-color "#cccccc")

; Transparency
; '(active inactive)
(if (display-graphic-p)
  (progn
    (set-frame-parameter (selected-frame) 'alpha '(97 95))
    (add-to-list 'default-frame-alist '(alpha 97 95))
))

;
(setq make-backup-files nil)
(setq create-lockfiles nil)
(setq default-truncate-lines t)
(global-linum-mode 1)
(unless (display-graphic-p)
  (progn
    (setq linum-format "%d ")
    ))
(set-face-attribute 'linum nil :background nil :foreground "#666666")
(setq-default indent-tabs-mode nil)

(add-hook 'c-mode-common-hook
          (progn
            (setq tab-width 4)
            (setq indent-tabs-mode t)))

(global-set-key (kbd "C-c p") (lambda () (interactive) (message buffer-file-name)))

;-----------------------
; MELPA
;-----------------------
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   '("melpa" . "http://melpa.org/packages/")
   t)
  (package-initialize))

;-----------------------
; Load init scripts
;-----------------------
(dolist (base-dir '("~/.emacs.d/init-user" "~/.emacs.d/init"))
  (let ((ignore-list '("." "..")))
    (if (file-accessible-directory-p base-dir)
        (dolist (file (directory-files base-dir))
          (if (not (member file ignore-list))
              (progn
                (load-file (concat base-dir "/" file)))
            )))))

;-----------------------
; eval hotkeys
;-----------------------
(global-set-key (kbd "C-c e e") 'eval-expression)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e b") 'eval-buffer)


;-----------------------
; Open explorer
;-----------------------
(if (eq system-type 'windows-nt)
    (if (not (fboundp 'explorer))
        (defun explorer ()
          (interactive)
          (shell-command (concat "C:/Windows/explorer /select, " (replace-regexp-in-string "/" "\\" buffer-file-name t t))))
      (message "Function explorer is already defined.")))


;-----------------------
; Cygwin/MSYS find-grep workaround
;-----------------------
; Cygwin/MSYS's find can't handle "NUL" file, which causes some problem in find-grep command family.
; Here's workaround.
(if (eq system-type 'windows-nt)
    (if (boundp 'niboshi-grep-windows-null-device-workaround)
        (progn
          (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device activate)
            ;;; This is neccessary for 'grep' command
            (setq null-device "/dev/null")
            ;;; This is neccessary for 'lgrep', 'rgrep', 'find-grep', etc.
            (let ((null-device "/dev/null")) ad-do-it)))))

;-----------------------
; Disable startup "For information..." message
; (Note: inhibit-startup-* method also works but it requires hardcoded user name)
;-----------------------
(defun display-startup-echo-area-message () ())

;-----------------------
; Recent files
;-----------------------
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)
(setq recentf-max-menu-items 100)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;-----------------------
; Whitespace
;-----------------------
(if (require 'whitespace nil 'noerror)
    (progn
      (autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)

      (setq whitespace-style '(empty face tabs newline tab-mark newline-mark))
      ;(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))
      (global-set-key "\C-c=w" 'global-whitespace-mode)

      (set-face-attribute 'whitespace-tab nil :background nil :foreground "#333333")
      (set-face-attribute 'whitespace-newline nil :background nil :foreground "#333333")
      (set-face-attribute 'whitespace-empty nil :background "#200000" :foreground nil)
      (set-face-attribute 'whitespace-trailing nil :background "#200000" :foreground nil)
      )

  (progn
    (message "Whitespace is unavailable.")
    (global-set-key "\C-c=w" 'ignore)
    ))


;-----------------------
; visible-bell
;-----------------------
 (defun niboshi-ring-bell-function ()
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))
 
 (setq visible-bell nil
       ring-bell-function 'niboshi-ring-bell-function)

;-----------------------
; incremental search
;-----------------------
(add-hook 'isearch-mode-hook
          (function
           (lambda()
             (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
             (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
             (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
             (define-key isearch-mode-map "\C-j" 'isearch-edit-string))))

;-----------------------
; ggtags
;-----------------------
(if (require 'ggtags nil 'noerror)
    (progn
      (global-set-key (kbd "S-<f12>") 'ggtags-find-reference)
      (global-set-key (kbd "C-<f12>") 'ggtags-find-definition)
      (global-set-key (kbd "<f12>") 'ggtags-find-definition)
      (global-set-key (kbd "C-c g r") 'ggtags-find-reference)
      (global-set-key (kbd "C-c g d") 'ggtags-find-definition)
      (global-set-key (kbd "C-c g g") 'ggtags-find-tag-dwim)
      (global-set-key (kbd "C-c g e") 'ggtags-find-tag-regexp)
      (global-set-key (kbd "C-c g f") 'ggtags-find-file)
      ))


;-----------------------
(message "Ready.")
