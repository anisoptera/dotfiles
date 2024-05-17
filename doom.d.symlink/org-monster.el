;;; ../.dotfiles/doom.d.symlink/org-monster.el -*- lexical-binding: t; -*-

(require 'org)

(defvar monster-frame-name "*Org-Monster*")
(defvar monster-frame nil)

;; Make a mode to prevent focus stealing from causing me to accidentally type in the wrong window
(defvar ia/focus-stolen-protection-mode-timer nil)

(define-minor-mode ia/focus-stolen-protection-mode
  "A mode that eats all keyboard input until 5 seconds have passed without keyboard input."
  :lighter " Focus-Stolen-Protection"
  :keymap (let ((map (make-sparse-keymap)))
            (dotimes (i 256)
              (define-key map (vector i) 'ignore))
            map)
  (if ia/focus-stolen-protection-mode
      (let ((my-buffer (current-buffer)))
        (setq ia/focus-stolen-protection-mode-timer
              (run-with-idle-timer 5 nil (lambda ()
                                           (with-current-buffer my-buffer
                                             (ia/focus-stolen-protection-mode 0))))))
    (when ia/focus-stolen-protection-mode-timer
      (cancel-timer ia/focus-stolen-protection-mode-timer)
      (setq ia/focus-stolen-protection-mode-timer nil))))

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

;; (run-with-idle-timer 300 t #'ia/org-idle-hook)



(provide 'org-monster)
