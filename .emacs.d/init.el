;;-----------------------
;; Profiling setup
;;-----------------------
(setq niboshi-init-start-time (current-time))
; How much time did it take to come here?
(message "(%.03f seconds before init.el)" (float-time (time-subtract niboshi-init-start-time before-init-time)))

(defun niboshi-uptime() (float-time (time-subtract (current-time) niboshi-init-start-time)))
(defun niboshi-profile (msg fn)
  (let ((start (niboshi-uptime)))
    (funcall fn)
    (let ((end (niboshi-uptime)))
      (message "%.03f[%.03f] seconds: %s" end (- end start) msg))))

;;-----------------------
;; Custom variable
;;-----------------------
(setq niboshi-default-background "black"
      niboshi-default-foreground "#cccccc")

;;-----------------------
;; niboshi-setup
;;-----------------------
(defun niboshi-setup()
  (interactive)
  (niboshi-install-packages))

;;-----------------------
(add-to-list 'load-path "~/.emacs.d/lisp")

(setq niboshi-hotkey-prefix (kbd "C-c ;"))
(defun niboshi-make-hotkey (k) (concat niboshi-hotkey-prefix (kbd k)))

;;-----------------------
;; Disable original features
;;-----------------------
;; Disable Contrl-Z to suspend
(if (display-graphic-p)
  (global-unset-key (kbd "C-z")))
(global-unset-key (kbd "C-c C-z"))

;; Disable ALT-key hook, to re-enable Windows system menu
(setq w32-pass-alt-to-system 1)

;; Disable IME hotkey
(global-unset-key (kbd "C-\\"))

;; Disable mouse commands
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

;;-----------------------
;; Essential keybinding
;;-----------------------
;; goto-line
(global-set-key (kbd "C-l") 'goto-line)

;; Move to previous window (opposite of C-x o)
(global-set-key (kbd "C-x O") (kbd "C-- C-x o"))

;; Move window cursor by arrows.
(global-set-key (kbd "C-x <up>")    'windmove-up)
(global-set-key (kbd "C-x <down>")  'windmove-down)
(global-set-key (kbd "C-x <left>")  'windmove-left)
(global-set-key (kbd "C-x <right>") 'windmove-right)

;; C-h
(define-key key-translation-map (kbd "C-h") "\C-?")
(setq backward-delete-char-untabify-method nil) ; This prevents C-h from converting a tab to spaces.

;; Kill a word
(global-set-key (kbd "M-h") 'backward-kill-word)

;; Scroll without moving cursor
(global-set-key (kbd "M-p") (lambda () (interactive) (scroll-down-command 1)))
(global-set-key (kbd "M-n") (lambda () (interactive) (scroll-up-command 1)))

;;-----------------------
;; Essential
;;-----------------------
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;;-----------------------
;; MELPA
;;-----------------------
(niboshi-profile
 "MELPA"
 (lambda()
   (when (>= emacs-major-version 24)
     (require 'package)
     (add-to-list
      'package-archives
      '("melpa" . "http://melpa.org/packages/")
      t)
     (setq package-enable-at-startup nil)
     (package-initialize)
     )))

(defun niboshi-install-packages()
  (interactive)
  (progn
    (message "niboshi-setup: Starting...")
    (ignore-errors (package-install 'use-package))
    (ignore-errors (package-install 'ggtags))
    (ignore-errors (package-install 'neotree))
    (ignore-errors (package-install 'helm))
    (ignore-errors (package-install 'projectile))
    (ignore-errors (package-install 'helm-projectile))
    (ignore-errors (package-install 'helm-swoop))
    (ignore-errors (package-install 'dtrt-indent)) ; auto-detect indentation
    (ignore-errors (package-install 'magit))
    (ignore-errors (package-install 'markdown-mode))
    (message "niboshi-setup: Finished")
    ))

;; require use-package
;; If it is not yet installed, install it now.
(unless (require 'use-package nil 'noerror)
  (message "Installing use-package...")
  (package-install 'use-package)
  (require 'use-package))

;;-----------------------
;; UI
;;-----------------------
(niboshi-profile
 "UI"
 (lambda()
   (progn
     (setq inhibit-splash-screen t)
     (if (display-graphic-p)
         (progn
           (tool-bar-mode -1)
           ))
     (menu-bar-mode -1)

     ;; Start maximized
     (add-to-list 'default-frame-alist '(fullscreen . maximized))

     ;; Colors
     (add-to-list 'default-frame-alist `(background-color . ,niboshi-default-background))
     (add-to-list 'default-frame-alist `(foreground-color . ,niboshi-default-foreground))

     ;; Highlight line
     (global-hl-line-mode 1)
     (set-face-background
      hl-line-face
      (if (display-graphic-p) "#112222" "#262626"))

     ;; Transparency
     ;; '(active inactive)
     (if (display-graphic-p)
         (progn
           (set-frame-parameter (selected-frame) 'alpha '(97 95))
           (add-to-list 'default-frame-alist '(alpha 97 95))
           )))))

(redraw-display)

;;-----------------------
;; Show message buffer to the right
;;-----------------------
(set-window-buffer
 (split-window nil nil "right")
 "*Messages*")


;;-----------------------
;; Scroll
;;-----------------------
(unless (display-graphic-p)
  (xterm-mouse-mode t))
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1) ((control) . nil)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't)

;;-----------------------
;; Load init scripts
;;-----------------------
(niboshi-profile
 "Load init scripts"
 (lambda()
   (dolist (base-dir '("~/.emacs.d/init-user" "~/.emacs.d/init"))
     (let ((ignore-list '("." "..")))
       (if (file-accessible-directory-p base-dir)
           (dolist (file (directory-files base-dir))
             (if (not (member file ignore-list))
                 (load-file (concat base-dir "/" file)))))))))

;;-----------------------
;; Per-host coloring
;;-----------------------
(defvar niboshi-host-color-alist
  '(
    ("amane" . "green")
    ("natsumi" . "deep sky blue")
  ))
(defvar niboshi-host-color
  (let ((color (cdr (assoc (downcase system-name) niboshi-host-color-alist))))
    (if color color "orange")))

(add-hook 'after-init-hook (lambda() (set-face-attribute 'mode-line nil :background niboshi-host-color)))

;;-----------------------
;; Misc
;;-----------------------
(setq default-truncate-lines t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Show path hotkey
(global-set-key (niboshi-make-hotkey "p")
                (lambda () (interactive)
                  (kill-new buffer-file-name)
                  (message buffer-file-name)))

;; Evaluation hotkeys
(defun niboshi-eval-wrapper(eval-func)
  (progn
    (niboshi-bring-message-buffer-to-front)
    (message "--- eval")
    (call-interactively eval-func)
    (message "OK")))

(global-set-key (kbd "C-c e e") (lambda() (interactive) (niboshi-eval-wrapper 'eval-expression)))
(global-set-key (kbd "C-c e r") (lambda() (interactive) (niboshi-eval-wrapper 'eval-region)))
(global-set-key (kbd "C-c e b") (lambda() (interactive) (niboshi-eval-wrapper 'eval-buffer)))

;; Encoding
(prefer-coding-system 'utf-8)

;; Enable CamelCase-aware word editing.
(global-subword-mode 1)

;;-----------------------
;; Server
;;-----------------------
(use-package server
  :config
  (unless (server-running-p)
    (server-start)))

;;-----------------------
;; Line number
;;-----------------------
(use-package linum
  :commands linum-mode
  :init
  (add-hook 'find-file-hook (lambda() (linum-mode 1)))
  :config
  (if (display-graphic-p)
      (set-face-attribute 'linum nil :background nil :foreground "#444444")
    (progn
      (setq linum-format "%d ")
      (set-face-attribute 'linum nil :background nil :foreground "brightblack"))))

;;-----------------------
;; fill-column-indicator
;;-----------------------
(use-package fill-column-indicator
  :if (display-graphic-p)
  :commands fci-mode
  :init
  (message "init fill-column-indicator")
  (add-hook 'c-mode-common-hook 'fci-mode)
  :config
  (message "config fill-column-indicator")
  (setq-default fci-always-use-textual-rule t) ; Default behaviour (draw line by image) causes line height problem.
  (setq-default fci-rule-column 80)
  (setq-default fci-rule-color "gray11"))


;;-----------------------
;; bs-show
;;-----------------------
(use-package bs-show
  :commands bs-show
  :bind (("C-x C-b" . bs-show)))

;;-----------------------
;; dtrt-indent
;;-----------------------
(use-package dtrt-indent
  :commands dtrt-indent-mode)

;;-----------------------
;; Hooks
;;-----------------------
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
            (if (fboundp 'whitespace-mode) (whitespace-mode t))
            (dtrt-indent-mode t)))

;;-----------------------
;; ido-mode (Interactive buffer switch, etc.)
;;-----------------------
(niboshi-profile
 "ido"
 (lambda()
   (progn
     (ido-mode nil)
     (setq ido-enable-prefix nil)
     (setq ido-enable-flex-matching t)
     (setq ido-case-fold t) ; Ignore case
     (setq ido-use-virtual-buffers t)
     )))

;;-----------------------
;; Open explorer
;;-----------------------
(if (eq system-type 'windows-nt)
    (if (not (fboundp 'explorer))
        (defun explorer ()
          (interactive)
          (shell-command (concat "C:/Windows/explorer /select, " (replace-regexp-in-string "/" "\\" buffer-file-name t t))))
      (message "Function explorer is already defined.")))


;;-----------------------
;; Cygwin/MSYS find-grep workaround
;;-----------------------
;; Cygwin/MSYS's find can't handle "NUL" file, which causes some problem in find-grep command family.
;; Here's workaround.
(if (eq system-type 'windows-nt)
    (if (boundp 'niboshi-grep-windows-null-device-workaround)
        (defadvice grep-compute-defaults (around grep-compute-defaults-advice-null-device activate)
          ;;; This is neccessary for 'grep' command
          (setq null-device "/dev/null")
          ;;; This is neccessary for 'lgrep', 'rgrep', 'find-grep', etc.
          (let ((null-device "/dev/null")) ad-do-it))))

;;-----------------------
;; Disable startup "For information..." message
;; (Note: inhibit-startup-* method also works but it requires hardcoded user name)
;;-----------------------
(defun display-startup-echo-area-message () ())

;;-----------------------
;; Recent files
;;-----------------------
(use-package recentf
  :commands recentf-open-files
  :bind (("C-x C-r" . recentf-open-files))
  :config
  (setq recentf-save-file (concat "~/.emacs.d/recentf-" system-name))
  (setq recentf-keep '(file-remote-p file-readable-p))
  (setq recentf-max-saved-items 1000)
  (setq recentf-max-menu-items 1000)
  (setq recentf-auto-cleanup 'never)
  ;; Start isearch automatically
  (add-hook
   'recentf-dialog-mode-hook
   (lambda()
     (run-with-timer 0.01 nil 'isearch-forward)))
  (recentf-mode 1))

;;-----------------------
;; Whitespace
;;-----------------------
(use-package whitespace
  :commands whitespace-mode
  :bind (("C-c = w" . whitespace-mode))
  :config
  (setq whitespace-style '(empty face tabs newline tab-mark newline-mark))
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'whitespace-tab nil :background niboshi-default-background :foreground "#333333")
        (set-face-attribute 'whitespace-newline nil :background niboshi-default-background :foreground "#333333")
        (set-face-attribute 'whitespace-empty nil :background "#200000" :foreground nil)
        (set-face-attribute 'whitespace-trailing nil :background "#200000" :foreground nil))
    (progn
      (set-face-attribute 'whitespace-tab nil :background niboshi-default-background :foreground "brightblack")
      (set-face-attribute 'whitespace-newline nil :background niboshi-default-background :foreground "brightblack")
      (set-face-attribute 'whitespace-empty nil :background "red" :foreground nil)
      (set-face-attribute 'whitespace-trailing nil :background "red" :foreground nil)
      )))

;;-----------------------
;; visible-bell
;;-----------------------
 (defun niboshi-ring-bell-function ()
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))
 
 (setq visible-bell nil
       ring-bell-function 'niboshi-ring-bell-function)

;;-----------------------
;; incremental search
;;-----------------------
(add-hook 'isearch-mode-hook
          (lambda()
            (define-key isearch-mode-map "\C-h" 'isearch-mode-help)
            (define-key isearch-mode-map "\C-t" 'isearch-toggle-regexp)
            (define-key isearch-mode-map "\C-c" 'isearch-toggle-case-fold)
            (define-key isearch-mode-map "\C-j" 'isearch-edit-string)))

;;-----------------------
;; Put other buffer behind
;;-----------------------
(defun niboshi-put-other-buffer-behind()
  (let ((win (car (cdr (window-list)))))
    (if win
        (let ((buf (window-buffer win)))
          (if buf
              (replace-buffer-in-windows buf))))))

(global-set-key (niboshi-make-hotkey "-") (lambda() (interactive) (niboshi-put-other-buffer-behind)))

(defun niboshi-bring-message-buffer-to-front()
  (let ((old-win (get-buffer-window)))
    (progn
      (switch-to-buffer-other-window "*Messages*")
      (select-window old-win))))

;;-----------------------
;; ggtags
;;-----------------------
(use-package ggtags
  :commands ggtags-mode
  :init
  (add-hook
   'ggtags-mode-hook
   (lambda()
     (global-set-key (kbd "S-<f12>") 'ggtags-find-reference)
     (global-set-key (kbd "C-<f12>") 'ggtags-find-definition)
     (global-set-key (kbd "<f12>")   'ggtags-find-definition)
     (global-set-key (kbd "C-c g r") 'ggtags-find-reference)
     (global-set-key (kbd "C-c g d") 'ggtags-find-definition)
     (global-set-key (kbd "C-c g g") 'ggtags-find-tag-dwim)
     (global-set-key (kbd "C-c g e") 'ggtags-find-tag-regexp)
     (global-set-key (kbd "C-c g f") 'ggtags-find-file)
     (global-set-key (kbd "C-c g u") 'ggtags-update-tags)
     ;; Put ggtags buffer behind
     (global-set-key (kbd "C-c g -") (lambda() (interactive)
                                       (progn
                                         (let ((buf (get-buffer "*ggtags-global*")))
                                           (if buf
                                               (replace-buffer-in-windows buf)))))))))
;;-----------------------
;; compilation
;;-----------------------
(global-set-key (kbd "C-c c c") 'compile)
(global-set-key (kbd "C-c c n") 'compilation-next-error)
(global-set-key (kbd "C-c c p") 'compilation-previous-error)
(add-hook 'compilation-mode-hook
          (lambda () 
            (progn
              (setq compilation-scroll-output 'first-error)
              (setq complation-auto-jump-to-first-error t)
              )))

;;-----------------------
;; diff
;;-----------------------
(use-package diff-mode
  :config
  (set-face-attribute 'diff-added   nil :foreground "green" :background "black")
  (set-face-attribute 'diff-removed nil :foreground "red"   :background "black"))

;;-----------------------
;; magit
;;-----------------------
(use-package magit
  :defer t
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
    ))

;;-----------------------
;; Projectile
;;-----------------------
;; Trigger C-c p to enable projectile-mode.
(global-set-key (kbd "C-c p") (lambda() (interactive)
                                (message "Enabling projectile...")
                                (global-unset-key (kbd "C-c p"))
                                ;; Enable globally
                                (projectile-global-mode)
                                (helm-projectile-toggle 1)
                                (message nil)))

(use-package projectile
  :commands projectile-global-mode
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line "(p)") ; Prevent lag on cursor move
)

(use-package helm-projectile
  :commands helm-projectile-toggle
  )

;;-----------------------
;; etags
;;-----------------------
(defun create-tags (dir-name filename-pattern)
  "Create tags file."
  (interactive
   (list (read-directory-name "Directory: ")
         (read-string "File name pattern: " "**/*.py" nil nil)))
  (eshell-command 
   (format "cd %s ; etags %s" dir-name filename-pattern)))


;;-----------------------
;; Hooks
;;-----------------------
;; Disable truncate-lines for special buffers (whose name is surrounded by * *)
(defun niboshi-choose-truncate-lines()
  (when (string-match "^\*.*\*$" (buffer-name))
    (setq truncate-lines t)))
;; Choose mode line text
(defvar niboshi-mode-name-alist
  '(
    (emacs-lisp-mode       . "elisp")
    (lisp-interaction-mode . "i-elisp")
    (python-mode           . "PY")
  ))
(defun niboshi-choose-mode-line-text()
  (let ((n (cdr (assoc major-mode niboshi-mode-name-alist))))
    (if n (setq mode-name n))))

(add-hook 'after-change-major-mode-hook (lambda()
                                          (niboshi-choose-truncate-lines)
                                          (niboshi-choose-mode-line-text)))

;;-----------------------
;; helm-swoop
;;-----------------------
(use-package helm-swoop
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all))
)

;;-----------------------
;; vc
;;-----------------------
;; Disable backends except SVN
;; because vc often causes slowdown.
(setq vc-handled-backends '(SVN))

;;-----------------------
;; Markdown
;;-----------------------
(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode))
  :config
  (add-hook 'markdown-mode-hook
            (lambda()
            ;; Override markdown preview filename
            (defun markdown-export-file-name (&optional extension)
              (when (buffer-file-name)
                (unless extension
                  (setq extension ".html"))
                (concat
                 (file-name-as-directory temporary-file-directory)
                 (file-name-as-directory "emacs-markdown")
                 (file-name-nondirectory (buffer-file-name))
                 extension)))

            ;; Select available markdown renderer
            (let ((command-succeeded (lambda(cmd) (eq 0 (call-process shell-file-name nil nil nil shell-command-switch cmd)))))
              (setq markdown-command
                    (cond
                     ((funcall command-succeeded "markdown --help") "markdown")
                     ((funcall command-succeeded "python -c \"import markdown2\"") "python -m markdown2 --extras fenced-code-blocks")
                     ((funcall command-succeeded "python -c \"import markdown\"") "python -m markdown -x markdown.extensions.fenced_code")
                     (t nil)))))))

;;-----------------------
;; Tips
;;-----------------------
(add-hook 'after-init-hook
          (lambda()
            (let ((filename "~/.emacs.d/tips.md"))
              (when (file-exists-p filename)
                (let ((grid-l (concat (char-to-string #x2506))))
                  (message " ")
                  (dolist (line (split-string
                                 (with-temp-buffer
                                   (insert-file-contents filename)
                                   (buffer-string)) "\n"))
                    (message (concat grid-l line))))))))

;;-----------------------

;; Ignore warning for redefining functions with defadvice.
;;  (ex. in rgrep)
(setq ad-redefinition-action 'accept)

;; Print the time spent in initialization
(add-hook 'after-init-hook
 (lambda()
   (message "Initialization: %.3f seconds" (float-time (time-subtract after-init-time before-init-time)))))

(niboshi-profile "Ready" 'ignore)
(message "Ready.")
