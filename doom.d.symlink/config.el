;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(cond
 (IS-MAC
  (setq doom-font (font-spec :family "BerkeleyMono Nerd Font" :size 12)
        doom-variable-pitch-font (font-spec :family "Avenir" :size 18)))
 (t
  (setq doom-font (font-spec :family "BerkeleyMono Nerd Font" :size 22)
        doom-variable-pitch-font (font-spec :family "Fira Sans" :size 28))))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-tokyo-night)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
;;
(use-package! platformio-mode
  :config (progn
            (add-hook 'c++-mode-hook (lambda ()
                                       (lsp-deferred)
                                       (platformio-conditionally-enable)))))

(when IS-LINUX
  (use-package! fcitx
    :config (progn
              (setq fcitx-remote-command "fcitx5-remote")
              (setq fcitx-use-dbus nil)
              (fcitx-aggressive-setup))))

(use-package! mixed-pitch
  :hook (org-mode . mixed-pitch-mode))

(after! vterm
  (setq vterm-shell "/usr/bin/zsh"))

(progn
  (map! :map org-mode-map :localleader :n "c p" #'org-pomodoro)
  (map! :map org-mode-map :localleader :n "c P" #'org-pomodoro-extend-last-clock))

(map! :i "C-o" #'evil-execute-in-normal-state)

(setq max-specpdl-size 50000)
(setq max-lisp-eval-depth 32000)

;; wslg is just not quite there, here are some helpers for copy/paste to it
(defun wsl-copy (start end)
  (interactive "r")
  (shell-command-on-region start end "clip.exe"))

(defun wsl-paste ()
  (interactive)
  (let ((coding-system-for-read 'dos))
    (insert
     (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2>/dev/null"))))

(map! "C-c C-c" #'wsl-copy)
(map! "C-c C-p" #'wsl-paste)

(when (and (eq system-type 'gnu/linux)
           (string-match
            "Linux.*Microsoft.*Linux"
            (shell-command-to-string "uname -a")))
  (setq
   browse-url-generic-program  "/mnt/c/Windows/System32/cmd.exe"
   browse-url-generic-args     '("/c" "start")
   browse-url-browser-function #'browse-url-generic))

;; A generic adviser for responding yes to yes or no prompts automatically.
(defun +always-yes-for-prompts-a (oldfun &rest args)
  (cl-letf (((symbol-function #'yes-or-no-p) (symbol-function #'always))
            ((symbol-function #'y-or-n-p) (symbol-function #'always)))
    (apply oldfun args)))

(advice-add #'emojify--confirm-emoji-download :around #'+always-yes-for-prompts-a)

;; set keys used to jump around to dvoraks
(setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s ?i ?d))

(defun term-send-region (start end)
  "Send the region to the connected terminal (after light preprocessing)."
  (interactive "r")

  (let ((src-buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring src-buf start end)

      ;;  delete comments before sending
      (while (re-search-backward ";.*\n" nil t)
        (replace-match "\n\n"))

      (term-send-string (get-process "/dev/ttyACM0")
                        (concat (buffer-string) "\n")))))

;; for some reason, `C-t` opens a new workspace. I don't like that.
(unbind-key "C-t" 'evil-normal-state-map)

;; send a specific term type for tramp
(after! tramp
  (setq tramp-terminal-type "tramp")
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path))

;; Make a minor mode for embiggening org headlines
(defvar ia/bigger-org-headlines-cookies nil)
(make-variable-buffer-local 'ia/bigger-org-headlines-cookies)

(define-minor-mode ia/bigger-org-headlines
  "Increases the size of Org headlines (inverse) proportionally to their depth."
  :lighter " Big-Org-Headlines"
  (if ia/bigger-org-headlines
      (let ((scale-step (/ (1- text-scale-mode-step) 1)))
        (seq-map-indexed
         (lambda (face idx)
           (push (face-remap-add-relative face
                                          :height (1+ (* scale-step (1+ idx)))
                                          :weight 'bold)
                 ia/bigger-org-headlines-cookies))
         ;; reverse this so that smallest is first
         ;; also drop the first 4 faces so that this doesnt get out of control
         (seq-drop (reverse org-level-faces) 4)))
    (mapc #'face-remap-remove-relative ia/bigger-org-headlines-cookies))
  (force-window-update (current-buffer)))

(add-hook! 'org-mode-hook #'ia/bigger-org-headlines)

(defun ia/notepad-frame ()
  (interactive)
  ;; can't do this one on windows lol
  (set-frame-position nil 0 0)
  ;; TODO make this pixelwise
  (set-frame-size nil 163 60))

;; Fix skeleton to allow some sort of escape
(defadvice! fixed-projectile-skel-dir-locals (&optional str arg)
  :override #'projectile-skel-dir-locals
  (interactive "*P\nP")
  (skeleton-proxy-new
   '(nil "((nil . (" ("" '(projectile-skel-variable-cons) n)
     resume: ")))")
   str arg))

(after! elfeed
  (defun elfeed-eww-open (&optional use-generic-p)
    "open with eww"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (eww-browse-url it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  (map! :map elfeed-search-mode-map :localleader :n "w" #'elfeed-eww-open))

;; Actually focus new frames
(defun ia/focus-new-client-frame ()
  (select-frame-set-input-focus (selected-frame)))

(add-hook 'server-after-make-frame-hook #'ia/focus-new-client-frame)

;; easier eval in clojure mode
(map! :after cider-mode :map clojure-mode-map :n "," #'cider-eval-last-sexp)

(load (string-join `(,doom-user-dir "org-monster.el")))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)

;; Fun with dashboard phrases
;; https://git.tecosaur.net/tec/emacs-config/src/branch/master/config.org#headline-55

(defvar splash-phrase-source-folder
  (expand-file-name "misc/splash-phrases" doom-user-dir)
  "A folder of text files with a fun phrase on each line.")

;; It would be good to specify/detect which of the two cases apply based on the file name alone. I've done this by setting the simple check that if the file name contains -N- (where N is some number) then it is taken as the N=​th phrase component, with everything preceding the =-N- token taken as the collection identifier, and everything after -N- ignored.
(defvar splash-phrase-sources
  (let* ((files (directory-files splash-phrase-source-folder nil "\\.txt\\'"))
         (sets (delete-dups (mapcar
                             (lambda (file)
                               (replace-regexp-in-string "\\(?:-[0-9]+-\\w+\\)?\\.txt" "" file))
                             files))))
    (mapcar (lambda (sset)
              (cons sset
                    (delq nil (mapcar
                               (lambda (file)
                                 (when (string-match-p (regexp-quote sset) file)
                                   file))
                               files))))
            sets))
  "A list of cons giving the phrase set name, and a list of files which contain phrase components.")

(defvar splash-phrase-set
  (nth (random (length splash-phrase-sources)) (mapcar #'car splash-phrase-sources))
  "The default phrase set. See `splash-phrase-sources'.")

(defun splash-phrase-set-random-set ()
  "Set a new random splash phrase set."
  (interactive)
  (setq splash-phrase-set
        (nth (random (1- (length splash-phrase-sources)))
             (cl-set-difference (mapcar #'car splash-phrase-sources) (list splash-phrase-set))))
  (+doom-dashboard-reload t))

(defun splash-phrase-select-set ()
  "Select a specific splash phrase set."
  (interactive)
  (setq splash-phrase-set (completing-read "Phrase set: " (mapcar #'car splash-phrase-sources)))
  (+doom-dashboard-reload t))

(defvar splash-phrase--cached-lines nil)

(defun splash-phrase-get-from-file (file)
  "Fetch a random line from FILE."
  (let ((lines (or (cdr (assoc file splash-phrase--cached-lines))
                   (cdar (push (cons file
                                     (with-temp-buffer
                                       (insert-file-contents (expand-file-name file splash-phrase-source-folder))
                                       (split-string (string-trim (buffer-string)) "\n")))
                               splash-phrase--cached-lines)))))
    (nth (random (length lines)) lines)))

(defun splash-phrase (&optional set)
  "Construct a splash phrase from SET. See `splash-phrase-sources'."
  (mapconcat
   #'splash-phrase-get-from-file
   (cdr (assoc (or set splash-phrase-set) splash-phrase-sources))
   " "))

(defun splash-phrase-dashboard-formatted ()
  "Get a splash phrase, flow it over multiple lines as needed, and fontify it."
  (mapconcat
   (lambda (line)
     (+doom-dashboard--center
      +doom-dashboard--width
      (with-temp-buffer
        (insert-text-button
         line
         'action
         (lambda (_) (+doom-dashboard-reload t))
         'face 'doom-dashboard-menu-title
         'mouse-face 'doom-dashboard-menu-title
         'help-echo "Random phrase"
         'follow-link t)
        (buffer-string))))
   (split-string
    (with-temp-buffer
      (insert (splash-phrase))
      (setq fill-column (min 70 (/ (* 2 (window-width)) 3)))
      (fill-region (point-min) (point-max))
      (buffer-string))
    "\n")
   "\n"))

(defun splash-phrase-dashboard-insert ()
  "Insert the splash phrase surrounded by newlines."
  (insert "\n" (splash-phrase-dashboard-formatted) "\n"))

(setq fancy-splash-image "~/.doom.d/M-x_butterfly.png")

(setq +doom-dashboard-functions
      (list #'doom-dashboard-widget-banner
            #'splash-phrase-dashboard-insert))
