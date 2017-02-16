;;-----------------------
;; Profiling setup
;;-----------------------
(defvar niboshi-init-start-time (current-time))
; How much time did it take to come here?
(message "(%.03f seconds before init.el)" (float-time (time-subtract niboshi-init-start-time before-init-time)))

(defun niboshi-uptime() (float-time (time-subtract (current-time) niboshi-init-start-time)))
(defun niboshi-profile (msg fn)
  (let ((start (niboshi-uptime)))
    (funcall fn)
    (let ((end (niboshi-uptime)))
      (message "%.03f[%.03f] seconds: %s" end (- end start) msg))))

;;-----------------------
;; load path
;;-----------------------
(add-to-list 'load-path "~/.emacs.d/lisp")

;;-----------------------
;; Custom variable
;;-----------------------
(defvar niboshi-default-background "black")
(defvar niboshi-default-foreground "#cccccc")

;;-----------------------
;; niboshi common functions
;;-----------------------
;; niboshi-setup
(defun niboshi-setup()
  (interactive)
  (niboshi-install-packages))

;; niboshi-make-hotkey
(defvar niboshi-hotkey-prefix (kbd "C-c ;"))
(defun niboshi-make-hotkey  (k) (concat niboshi-hotkey-prefix (kbd k)))
(defun niboshi-make-hotkey- (k) (key-description (concat niboshi-hotkey-prefix (kbd k))))

;; niboshi-add-path
(defun niboshi-add-path(path)
  (progn
    ;; Environment variable
    (setenv "PATH" (concat path ";" (getenv "PATH")))
    ;; exec-path variable
    (let ((path_posix (replace-regexp-in-string "\\\\" "/" path)))
      (setq exec-path (append `(,path_posix) exec-path)))))

;; niboshi-add-paths
(defun niboshi-add-paths(paths)
  (dolist (path paths)
    (niboshi-add-path path)))

;; Custom key map minor mode
(defvar niboshi-keys-minor-mode-map
  (make-sparse-keymap)
  "niboshi-keys-minor-mode keymap.")

(defun niboshi-set-key (k fn)
  (define-key niboshi-keys-minor-mode-map k fn))

(defun niboshi-unset-key (k)
  (define-key niboshi-keys-minor-mode-map k nil))

;;-----------------------
;; Disable original features
;;-----------------------
;; Disable Contrl-Z to suspend
(if (display-graphic-p)
  (global-unset-key (kbd "C-z")))
(global-unset-key (kbd "C-c C-z"))

;; Disable ALT-key hook, to re-enable Windows system menu
(defvar w32-pass-alt-to-system 1)

;;-----------------------
;; Essential keybinding
;;-----------------------

;; C-h
(define-key key-translation-map (kbd "C-h") "\C-?")
(setq backward-delete-char-untabify-method nil) ; This prevents C-h from converting a tab to spaces.

;; Page scroll without changing the cursor's apparent position in screen
(setq scroll-preserve-screen-position t)

(when t
    ;; goto-line
    (niboshi-set-key (kbd "C-l") 'goto-line)

    ;; Disable IME hotkey
    (niboshi-set-key (kbd "C-\\") 'ignore)

    ;; Kill a word
    (niboshi-set-key (kbd "M-h") 'backward-kill-word)

    ;; Move cursor to the middle of screen
    (niboshi-set-key (kbd "C-c .") 'recenter)

    ;; Move cursor position without changing it's apparetn position in screen
    (niboshi-set-key (kbd "M-p") (lambda () (interactive) (forward-line -1) (scroll-down-command 1)))
    (niboshi-set-key (kbd "M-n") (lambda () (interactive) (forward-line +1) (scroll-up-command 1)))

    ;; Scroll without moving cursor
    (niboshi-set-key (kbd "M-P") (lambda () (interactive) (scroll-down-command 1)))
    (niboshi-set-key (kbd "M-N") (lambda () (interactive) (scroll-up-command 1)))

    ;; niboshi-error-navigation-minor-mode:
    ;;   C-c ; e         to start navigating erros
    ;;   C-M-p or C-M-n  to navigate errors during the navigation mode
    (when t
      ;; Custom key map minor mode
      (defvar niboshi-error-navigation-minor-mode-map
        (make-sparse-keymap))

      (define-minor-mode niboshi-error-navigation-minor-mode
        "A minor mode for navigating errors."
        :init-value nil
        :global t
        :lighter " ")

      (define-key niboshi-error-navigation-minor-mode-map (kbd "C-M-p") 'previous-error)
      (define-key niboshi-error-navigation-minor-mode-map (kbd "C-M-n") 'next-error)

      ;; Next/previous match
      (niboshi-set-key (niboshi-make-hotkey "e") 'niboshi-error-navigation-minor-mode)
      )

    ;; Use regex search by default
    (niboshi-set-key (kbd "C-s") 'isearch-forward-regexp)
    (niboshi-set-key (kbd "C-r") 'isearch-backward-regexp)
    (niboshi-set-key (kbd "C-M-s") 'isearch-forward)
    (niboshi-set-key (kbd "C-M-r") 'isearch-forward)

    ;; Move to previous window (opposite of C-x o)
    (niboshi-set-key (kbd "C-x O") (kbd "C-- C-x o"))

    ;; Move window *CURSOR* by arrows.
    (niboshi-set-key (kbd "C-x <up>")    'windmove-up)
    (niboshi-set-key (kbd "C-x <down>")  'windmove-down)
    (niboshi-set-key (kbd "C-x <left>")  'windmove-left)
    (niboshi-set-key (kbd "C-x <right>") 'windmove-right)

    ;; Move window *POSITION* by arrows.
    (niboshi-set-key (kbd "C-x S-<up>")    'buf-move-up)
    (niboshi-set-key (kbd "C-x S-<down>")  'buf-move-down)
    (niboshi-set-key (kbd "C-x S-<left>")  'buf-move-left)
    (niboshi-set-key (kbd "C-x S-<right>") 'buf-move-right)

    ;; Buffer rotation
    (niboshi-set-key (kbd "C-x <end>")  'bury-buffer)
    (niboshi-set-key (kbd "C-x <home>") 'unbury-buffer)

    ;; Buffer switch
    (niboshi-set-key (kbd "C-x C-x") (lambda () (interactive) (switch-to-buffer (other-buffer))))

    ;; Disable mouse commands
    (niboshi-set-key [down-mouse-2] 'ignore)
    (niboshi-set-key [mouse-2] 'ignore)
    (niboshi-set-key [down-mouse-3] 'ignore)
    (niboshi-set-key [mouse-3] 'ignore)

    (niboshi-set-key [S-down-mouse-1] 'ignore)
    (niboshi-set-key [S-mouse-1] 'ignore)
    (niboshi-set-key [S-down-mouse-2] 'ignore)
    (niboshi-set-key [S-mouse-2] 'ignore)
    (niboshi-set-key [S-down-mouse-3] 'ignore)
    (niboshi-set-key [S-mouse-3] 'ignore)

    (niboshi-set-key [C-down-mouse-1] 'ignore)
    (niboshi-set-key [C-mouse-1] 'ignore)
    (niboshi-set-key [C-down-mouse-2] 'ignore)
    (niboshi-set-key [C-mouse-2] 'ignore)
    (niboshi-set-key [C-down-mouse-3] 'ignore)
    (niboshi-set-key [C-mouse-3] 'ignore)

    (niboshi-set-key [M-down-mouse-1] 'ignore)
    (niboshi-set-key [M-mouse-1] 'ignore)
    (niboshi-set-key [M-down-mouse-2] 'ignore)
    (niboshi-set-key [M-mouse-2] 'ignore)
    (niboshi-set-key [M-down-mouse-3] 'ignore)
    (niboshi-set-key [M-mouse-3] 'ignore)

    (niboshi-set-key [mouse-4] (lambda () (interactive) (scroll-down-command 1)))
    (niboshi-set-key [mouse-5] (lambda () (interactive) (scroll-up-command 1)))
    )

(define-minor-mode niboshi-keys-minor-mode
  "A minor mode so that my key settings override annoying major modes."
  :init-value t
  :lighter " nib-keys")

(niboshi-keys-minor-mode 1)

(add-hook 'after-load-functions 'my-keys-have-priority)
(defun my-keys-have-priority (_file)
  "Try to ensure that my keybindings retain priority over other minor modes.
Called via the `after-load-functions' special hook."
  (unless (eq (caar minor-mode-map-alist) 'niboshi-keys-minor-mode)
    (let ((map (assq 'niboshi-keys-minor-mode minor-mode-map-alist)))
      (assq-delete-all 'niboshi-keys-minor-mode minor-mode-map-alist)
      (add-to-list 'minor-mode-map-alist map)))

  (unless (eq (caar minor-mode-alist) 'niboshi-keys-minor-mode)
    (let ((map (assq 'niboshi-keys-minor-mode minor-mode-alist)))
      (assq-delete-all 'niboshi-keys-minor-mode minor-mode-alist)
      (add-to-list 'minor-mode-alist map)))
)


;;-----------------------
;; Essential
;;-----------------------
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; Prefer modified .el file over byte-compiled package
(when (not (version< emacs-version "24.4"))
  (setq load-prefer-newer t))

;; Follow symlinks in find-file
(setq-default find-file-visit-truename t)


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
    (jedi:install-server)
    (message "niboshi-setup: Finished")
    ))

;; require use-package
;; If it is not yet installed, install it now.
(unless (require 'use-package nil 'noerror)
  (message "Installing use-package...")
  (package-refresh-contents)
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
;; Misc
;;-----------------------
(setq-default truncate-lines t)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)

;; Evaluation wrapper
(defun niboshi-eval-wrapper(eval-func)
  (progn
    (niboshi-bring-message-buffer-to-front)
    (message "--- eval")
    (call-interactively eval-func)
    (message "OK")
    (with-current-buffer "*Messages*"
      (goto-char (point-max)))))

(when t
  (niboshi-set-key (kbd "C-c e e") (lambda() (interactive) (niboshi-eval-wrapper 'eval-expression)))
  (niboshi-set-key (kbd "C-c e r") (lambda() (interactive) (niboshi-eval-wrapper 'eval-region)))
  (niboshi-set-key (kbd "C-c e b") (lambda() (interactive) (niboshi-eval-wrapper 'eval-buffer)))
  )

;; Encoding
(prefer-coding-system 'utf-8)
(setq-default buffer-file-coding-system 'utf-8-unix)

;; Enable CamelCase-aware word editing.
(global-subword-mode 1)

;; Show path hotkey
(defun niboshi-print-current-buffer-file-name() (interactive)
  (kill-new buffer-file-name)
  (message buffer-file-name))
(niboshi-set-key (niboshi-make-hotkey "p") 'niboshi-print-current-buffer-file-name)

;;-----------------------
;; Server
;;-----------------------
(use-package server
  :ensure t
  :config
  (unless (server-running-p)
    (server-start)))

;;-----------------------
;; Recent files
;;-----------------------
(use-package recentf
  :ensure t
  :commands recentf-open-files
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
     ;; C-g to close recentf buffer
     (local-set-key (kbd "C-g") (lambda() (interactive) (kill-buffer (current-buffer))))
     ;; Begin isearch-forward automatically
     (run-with-timer 0.01 nil 'isearch-forward-regexp)))
  (recentf-mode 1))

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
  (interactive)
  (let ((win (car (cdr (window-list)))))
    (if win
        (let ((buf (window-buffer win)))
          (if buf
              (replace-buffer-in-windows buf))))))

(niboshi-set-key (niboshi-make-hotkey "-") 'niboshi-put-other-buffer-behind)

(defun niboshi-bring-message-buffer-to-front()
  (let ((old-win (get-buffer-window)))
    (progn
      (switch-to-buffer-other-window "*Messages*")
      (select-window old-win))))

;;-----------------------
;; Swap windows
;;-----------------------
;; Original: https://www.emacswiki.org/emacs/TransposeWindows
(defun niboshi-transpose-windows (arg)
  "Transpose the buffers shown in two windows."
  (interactive "p")
  (let ((selector (if (>= arg 0) 'next-window 'previous-window)))
    (while (/= arg 0)
      (let ((this-win (window-buffer))
            (next-win (window-buffer (funcall selector))))
        (set-window-buffer (selected-window) next-win)
        (set-window-buffer (funcall selector) this-win)
        (select-window (funcall selector)))
      (setq arg (if (plusp arg) (1- arg) (1+ arg))))))

(niboshi-set-key (niboshi-make-hotkey "x") 'niboshi-transpose-windows)

;;-----------------------
;; buffer-menu
;;-----------------------
(defun niboshi-buffer-menu-other-window()
  (interactive)
  (defvar niboshi-buffer-menu-other-window-old-buffer)
  (setq niboshi-buffer-menu-other-window-old-buffer (get-buffer-window))
  (add-hook 'Buffer-menu-mode-hook
            (lambda()
              (local-set-key (kbd "C-g") (lambda() (interactive)
                                           (kill-buffer (current-buffer))
                                           (select-window niboshi-buffer-menu-other-window-old-buffer)
                                           (setq niboshi-buffer-menu-other-window-old-buffer nil)))))
  (buffer-menu-other-window))
(niboshi-set-key (kbd "C-x C-b") 'niboshi-buffer-menu-other-window)

;;-----------------------
;; switch-to-minibuffer
;;-----------------------
(defun niboshi-switch-to-minibuffer ()
  "Switch to minibuffer window."
  (interactive)
  (let ((win (active-minibuffer-window)))
    (if win
        (select-window win)
      (message "Minibuffer is not active"))))

(niboshi-set-key (niboshi-make-hotkey "m") 'niboshi-switch-to-minibuffer)

;;-----------------------
;; visible-bell
;;-----------------------
 (defun niboshi-ring-bell-function ()
   (invert-face 'mode-line)
   (run-with-timer 0.1 nil 'invert-face 'mode-line))

 (setq visible-bell nil
       ring-bell-function 'niboshi-ring-bell-function)

;;-----------------------
;; Line number
;;-----------------------
(use-package linum
  :ensure t
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
  :ensure t
  :commands fci-mode
  :init
  (add-hook 'prog-mode-hook
            (lambda()
              (run-at-time "0 sec" nil 'fci-mode)))
  :config
  (setq-default fci-always-use-textual-rule t) ; Default behaviour (draw line by image) causes line height problem.
  (setq-default fci-rule-column 80)
  (if (display-graphic-p)
      (setq-default fci-rule-color "gray11")
    (setq-default fci-rule-color "color-234")))

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
  (niboshi-set-key (kbd "C-x C-r") 'ido-recentf-open)

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
  :bind (("C-c ; i" . idomenu))
)


;;-----------------------
;; Company
;;-----------------------
(use-package company
  :ensure t
  :commands company-mode
  :init
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'go-mode-hook
            (lambda()
              (set (make-local-variable 'company-backends) '(company-go))))
  :config
  (add-to-list 'company-backends 'company-racer)
  (add-to-list 'company-backends 'company-jedi)
  (niboshi-set-key (niboshi-make-hotkey "TAB") 'company-complete)
  )

(use-package company-go :ensure t)
(use-package company-jedi :ensure t)
(use-package company-racer :ensure t)

;;-----------------------
;; flycheck
;;-----------------------
(use-package flycheck
  :ensure t
  :commands flycheck-mode
  :init
  (add-hook 'prog-mode-hook
            (lambda()
              (flycheck-mode t)))
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
)

;;-----------------------
;; dtrt-indent
;;-----------------------
(use-package dtrt-indent
  :ensure t
  :commands dtrt-indent-mode
  :init
  (add-hook 'prog-mode-hook
            (lambda()
              (dtrt-indent-mode t))))

;;-----------------------
;; which-func
;;-----------------------
(use-package which-func
  :ensure t
  :commands which-function-mode
  :init
  (add-hook 'prog-mode-hook
            (lambda()
              (which-function-mode t))))


;;-----------------------
;; grep
;;-----------------------
(use-package grep
  :ensure t
  :config
  ;; Modify default extension list
  ;; Replace C extensions with those of C++.
  (add-to-list 'grep-files-aliases
               `("ch" . ,(cdr (assoc "cchh" grep-files-aliases)))))


;;-----------------------
;; Whitespace
;;-----------------------
(use-package whitespace
  :ensure t
  :commands whitespace-mode
  :init
  (niboshi-set-key (kbd "C-c = w") 'whitespace-mode)
  (add-hook 'prog-mode-hook
            (lambda()
              (whitespace-mode t)))
  :config
  (setq whitespace-style '(empty face tabs newline tab-mark newline-mark trailing))
  (if (display-graphic-p)
      (progn
        (set-face-attribute 'whitespace-tab nil :background niboshi-default-background :foreground "#333333")
        (set-face-attribute 'whitespace-newline nil :background niboshi-default-background :foreground "#333333")
        (set-face-attribute 'whitespace-empty nil :background "#ff0000" :foreground nil)
        (set-face-attribute 'whitespace-trailing nil :background "#ff0000" :foreground nil))
    (progn
      (set-face-attribute 'whitespace-tab nil :background niboshi-default-background :foreground "color-237")
      (set-face-attribute 'whitespace-newline nil :background niboshi-default-background :foreground "color-237")
      (set-face-attribute 'whitespace-empty nil :background "red" :foreground nil)
      (set-face-attribute 'whitespace-trailing nil :background "red" :foreground nil)
      )))

;;-----------------------
;; compilation
;;-----------------------
(niboshi-set-key (kbd "C-c c c") 'compile)
(niboshi-set-key (kbd "C-c c n") 'compilation-next-error)
(niboshi-set-key (kbd "C-c c p") 'compilation-previous-error)
(add-hook 'compilation-mode-hook
          (lambda ()
            (progn
              (setq compilation-scroll-output 'first-error)
              (setq compilation-auto-jump-to-first-error t)
              )))

;;-----------------------
;; diff
;;-----------------------
(use-package diff-mode
  :ensure t
  :config
  (set-face-attribute 'diff-added   nil :foreground "green" :background "black")
  (set-face-attribute 'diff-removed nil :foreground "red"   :background "black"))

;;-----------------------
;; Projectile
;;-----------------------
(use-package projectile
  :ensure t
  :commands projectile-global-mode
  :init
  (setq projectile-indexing-method 'alien)
  (setq projectile-enable-caching t)
  (setq projectile-mode-line "(p)") ; Prevent lag on cursor move
)

(use-package helm-projectile
  :ensure t
  :commands helm-projectile-toggle
  :bind (("C-c p a" . helm-projectile-find-other-file)
         ("C-c p p" . helm-projectile-switch-project)
         ("C-c p f" . helm-projectile-find-file))
  :config
  (projectile-global-mode)
  (helm-projectile-toggle 1)
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
  :ensure t
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
;; Makefile
;;-----------------------
(use-package make-mode
  :ensure t
  :mode (("\\Makefile.*\\'" . makefile-mode)))

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
                 (ignore-errors (load-file (concat base-dir "/" file))))))))))

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
;; Disable startup "For information..." message
;; (Note: inhibit-startup-* method also works but it requires hardcoded user name)
;;-----------------------
(defun display-startup-echo-area-message () ())

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
