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
