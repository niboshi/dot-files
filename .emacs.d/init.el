(require 'server)
(unless (server-running-p)
  (server-start))

;-----------------------
(add-to-list 'load-path "~/.emacs.d/lisp")

(setq niboshi-hotkey-prefix (kbd "C-c ;"))
(defun niboshi-make-hotkey (k) (concat niboshi-hotkey-prefix (kbd k)))

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
(global-unset-key (kbd "C-\\"))

; Disable mouse commands
(global-set-key [down-mouse-2] 'ignore)
(global-set-key [mouse-2] 'ignore)
(global-set-key [down-mouse-3] 'ignore)
(global-set-key [mouse-3] 'ignore)

(global-set-key [S-down-mouse-1] 'ignore)
(global-set-key [S-mouse-1] 'ignore)
(global-set-key [S-down-mouse-2] 'ignore)
(global-set-key [S-mouse-2] 'ignore)
(global-set-key [S-down-mouse-3] 'ignore)
(global-set-key [S-mouse-3] 'ignore)

(global-set-key [C-down-mouse-1] 'ignore)
(global-set-key [C-mouse-1] 'ignore)
(global-set-key [C-down-mouse-2] 'ignore)
(global-set-key [C-mouse-2] 'ignore)
(global-set-key [C-down-mouse-3] 'ignore)
(global-set-key [C-mouse-3] 'ignore)

(global-set-key [M-down-mouse-1] 'ignore)
(global-set-key [M-mouse-1] 'ignore)
(global-set-key [M-down-mouse-2] 'ignore)
(global-set-key [M-mouse-2] 'ignore)
(global-set-key [M-down-mouse-3] 'ignore)
(global-set-key [M-mouse-3] 'ignore)

(global-set-key [mouse-4] (lambda () (interactive) (scroll-down-command 1)))
(global-set-key [mouse-5] (lambda () (interactive) (scroll-up-command 1)))

;-----------------------
; Essential keybinding
;-----------------------
; goto-line
(global-set-key (kbd "C-l") 'goto-line)

; Move to previous window (opposite of C-x o)
(global-set-key (kbd "C-x O") (kbd "C-- C-x o"))

; Move window cursor by arrows.
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

; C-h
(define-key key-translation-map (kbd "C-h") "\C-?")
(setq backward-delete-char-untabify-method nil) ; This prevents C-h from converting a tab to spaces.

; bs-show
(if (fboundp 'bs-show)
    (global-set-key (kbd "C-x C-b") 'bs-show))

; Kill a word
(global-set-key (kbd "M-h") 'backward-kill-word)

; Scroll without moving cursor
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down-command 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up-command 1)))

;-----------------------
; Essential
;-----------------------
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;-----------------------
; UI
;-----------------------
(if (display-graphic-p)
    (progn
      (setq inhibit-splash-screen t)
      (tool-bar-mode -1)
      ))
(menu-bar-mode -1)

; Colors
(set-background-color "black")
(set-foreground-color "#cccccc")

(if (display-graphic-p)
    (global-hl-line-mode 1))

; Transparency
; '(active inactive)
(if (display-graphic-p)
  (progn
    (set-frame-parameter (selected-frame) 'alpha '(97 95))
    (add-to-list 'default-frame-alist '(alpha 97 95))
))

;-----------------------
; Show message buffer to the right
;-----------------------
(set-window-buffer
 (split-window nil nil "right")
 "*Messages*")


;-----------------------
; Scroll
;-----------------------
(unless (display-graphic-p)
  (xterm-mouse-mode t))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

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

(defun niboshi-setup()
  (interactive )
  (progn
    (package-install 'ggtags)
    (package-install 'replace+)
    (package-install 'neotree)
    (package-install 'helm)
    (package-install 'projectile)
    (package-install 'helm-projectile)
    ))

;-----------------------
; Load init scripts
;-----------------------
(dolist (base-dir '("~/.emacs.d/init-user" "~/.emacs.d/init"))
  (let ((ignore-list '("." "..")))
    (if (file-accessible-directory-p base-dir)
        (dolist (file (directory-files base-dir))
          (if (not (member file ignore-list))
              (load-file (concat base-dir "/" file)))))))

;-----------------------
; Misc
;-----------------------
(setq default-truncate-lines t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

; Show path hotkey
(global-set-key (niboshi-make-hotkey "p")
                (lambda () (interactive)
                  (kill-new buffer-file-name)
                  (message buffer-file-name)))

; Evaluation hotkeys
(global-set-key (kbd "C-c e e") 'eval-expression)
(global-set-key (kbd "C-c e r") 'eval-region)
(global-set-key (kbd "C-c e b") 'eval-buffer)

; Encoding
(prefer-coding-system 'utf-8)

; Enable CamelCase-aware word editing.
(global-subword-mode 1)

;-----------------------
; Line number
;-----------------------
(global-linum-mode 1)
(if (display-graphic-p)
    (progn
      (set-face-attribute 'linum nil :background nil :foreground "#444444")
      )
  (progn
    (setq linum-format "%d ")
    (set-face-attribute 'linum nil :background nil :foreground "brightblack")
    ))

;-----------------------
; fill-column-indicator
;-----------------------
(if (display-graphic-p)
    (when (require 'fill-column-indicator nil 'noerror)
      (setq-default fci-always-use-textual-rule t) ; Default behaviour (draw line by image) causes line height problem.
      (setq-default fci-rule-column 80)
      (setq-default fci-rule-color "gray11")
      (add-hook 'c-mode-common-hook 'fci-mode)))

;-----------------------
; Hooks
;-----------------------
(add-hook 'c-mode-common-hook
          (lambda()
            (setq tab-width 4)
            (setq c-basic-offset tab-width)
            (c-set-offset 'arglist-intro '++)
            (setq indent-tabs-mode t)
            (which-function-mode t) ; show function name
            (if (fboundp 'whitespace-mode) (whitespace-mode t))))

(add-hook 'python-mode-hook
          (lambda ()
            (setq indent-tabs-mode t)
            (setq tab-width 4)
            (setq python-indent 4)
            (which-function-mode t) ; show function name
            (if (fboundp 'whitespace-mode) (whitespace-mode t))))

;-----------------------
; ido-mode (Interactive buffer switch, etc.)
;-----------------------
(ido-mode nil)
(setq ido-enable-prefix nil)
(setq ido-enable-flex-matching t)
(setq ido-case-fold t) ; Ignore case
;(setq ido-use-virtual-buffers t)

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
        (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device activate)
          ;;; This is neccessary for 'grep' command
          (setq null-device "/dev/null")
          ;;; This is neccessary for 'lgrep', 'rgrep', 'find-grep', etc.
          (let ((null-device "/dev/null")) ad-do-it))))

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
(setq recentf-max-saved-items 1000)
(setq recentf-max-menu-items 1000)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;-----------------------
; Whitespace
;-----------------------
(if (require 'whitespace nil 'noerror)
    (progn
      (autoload 'whitespace-mode "whitespace" "Toggle whitespace visualization." t)

      (setq whitespace-style '(empty face tabs newline tab-mark newline-mark))
      ;(setq whitespace-style '(spaces tabs newline space-mark tab-mark newline-mark))
      (global-set-key (kbd "C-c = w") 'whitespace-mode)

      (if (display-graphic-p)
          (progn
            (set-face-attribute 'whitespace-tab nil :background nil :foreground "#333333")
            (set-face-attribute 'whitespace-newline nil :background nil :foreground "#333333")
            (set-face-attribute 'whitespace-empty nil :background "#200000" :foreground nil)
            (set-face-attribute 'whitespace-trailing nil :background "#200000" :foreground nil))
        (progn
          (set-face-attribute 'whitespace-tab nil :background nil :foreground "brightblack")
          (set-face-attribute 'whitespace-newline nil :background nil :foreground "brightblack")
          (set-face-attribute 'whitespace-empty nil :background "red" :foreground nil)
          (set-face-attribute 'whitespace-trailing nil :background "red" :foreground nil)
          )))

  (progn
    (message "Whitespace is unavailable.")
    (global-set-key (kbd "C-c = w") 'ignore)
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
          (lambda()
            (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
            (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
            (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
            (define-key isearch-mode-map "\C-j" 'isearch-edit-string)))

;-----------------------
; Put other buffer behind
;-----------------------
(defun niboshi-put-other-buffer-behind()
  (interactive)
  (let ((win (car (cdr (window-list)))))
    (if win
        (let ((buf (window-buffer win)))
          (if buf
              (replace-buffer-in-windows buf))))))

(global-set-key (niboshi-make-hotkey "-") 'niboshi-put-other-buffer-behind)

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
      (global-set-key (kbd "C-c g u") 'ggtags-update-tags)
      ; Put ggtags buffer behind
      (global-set-key (kbd "C-c g -") (lambda() (interactive)
                                        (progn
                                          (let ((buf (get-buffer "*ggtags-global*")))
                                            (if buf
                                                (replace-buffer-in-windows buf))))))
      ))

;-----------------------
; compilation
;-----------------------
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c n") 'compilation-next-error)
(global-set-key (kbd "C-c c p") 'compilation-previous-error)
(add-hook 'compilation-mode-hook
          (lambda () 
            (progn
              (setq compilation-scroll-output 'first-error)
              (setq complation-auto-jump-to-first-error t)
              )))

;-----------------------
; diff
;-----------------------
(custom-set-faces
 '(diff-added ((t (:foreground "green"))) 'now)
 '(diff-removed ((t (:foreground "red"))) 'now)
 )

;-----------------------
; Projectile
;-----------------------
(when (require 'helm-projectile nil 'noerror)
  (projectile-global-mode)
  (helm-projectile-on)
  )
  


;-----------------------
; Hooks
;-----------------------
; Special buffers (whose name is surrounded by * *)
(defun niboshi-after-change-major-mode-hook()
  (when (string-match "^\*.*\*$" (buffer-name))
    (setq truncate-lines t)
    (linum-mode 0)
    ))
(add-hook 'after-change-major-mode-hook 'niboshi-after-change-major-mode-hook)

;-----------------------

; Ignore warning for redefining functions with defadvice.
;  (ex. in rgrep)
(setq ad-redefinition-action 'accept)

(message "Ready.")
