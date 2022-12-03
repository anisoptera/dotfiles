;;; ../.dotfiles/doom.d.symlink/org-monster.el -*- lexical-binding: t; -*-

(require 'org)

(defvar monster-frame-name "*Org-Monster*")
(defvar monster-frame nil)

(defun raise-or-make-monster-frame ()
  (when (not (frame-live-p monster-frame))
    (setq monster-frame (make-frame `((name . ,monster-frame-name)))))
  (select-frame-set-input-focus monster-frame)
  monster-frame)

;; Pop agenda when idle if I'm not actually clocked in on something
(defun ia/org-idle-hook ()
  (if (org-clocking-buffer)
      (message "Clocked in on “%s”" org-clock-heading)
    (with-selected-frame (raise-or-make-monster-frame)
      (org-agenda nil " "))))

(run-with-idle-timer 300 t #'ia/org-idle-hook)

(provide 'org-monster)
