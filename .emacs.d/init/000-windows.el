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
