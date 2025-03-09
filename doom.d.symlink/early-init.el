;;; ../.dotfiles/doom.d.symlink/early-init.el -*- lexical-binding: t; -*-

;; HACK Work around native compilation on macOS failing with 'ld: library not
;; found for -lemutls_w'.
;; https://github.com/d12frosted/homebrew-emacs-plus/issues/554
(setenv "LIBRARY_PATH"
	(string-join
	 '("/opt/homebrew/opt/gcc/lib/gcc/13"
	   "/opt/homebrew/opt/libgccjit/lib/gcc/13"
	   "/opt/homebrew/opt/gcc/lib/gcc/13/gcc/aarch64-apple-darwin22/13")
	 ":"))

;; on macOS we should use gnu grep if it's there
(add-to-list 'exec-path "/opt/homebrew/opt/grep/libexec/gnubin")
