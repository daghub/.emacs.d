; Emacs config file

; Add load directory
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/apel-10.8")
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(add-to-list 'load-path "~/.emacs.d/sunrise-commander")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/mo-git-blame")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")

;; ;; Try the Menlo font
;; (custom-set-faces
;;   ;; custom-set-faces was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(default ((t (:height 120 :family "Menlo"))))
;;  '(font-lock-comment-face ((t (:foreground "#3f7f5f")))))

;; Color theme
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; Disable scrollbars
(scroll-bar-mode -1)

; Avoid emacs creating backup files
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;
;ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-use-filename-at-point 'guess)
(ido-mode t)
; idomenu allows ido to search iMenu results for a buffer
(autoload 'idomenu "idomenu" nil t)
(add-hook 'c-mode-common-hook 'imenu-add-menubar-index)
(global-set-key (kbd "M-i") 'idomenu)

;;;;;;;;;;;;;;;;;;;;
;; Sunrise commander
;;;;;;;;;;;;;;;;;;;;
(require 'sunrise-commander)
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
(global-set-key (kbd "C-c x") 'sunrise)
(global-set-key (kbd "C-c X") 'sunrise-cd)

; Active google style guides for C/C++
(require 'google-c-style)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; open *help* in current frame
(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

;;;;;;; GNU Global
(defun gtags-root-dir ()
   "Returns GTAGS root directory or nil if doesn't exist."
   (with-temp-buffer
     (if (zerop (call-process "global" nil t nil "-pr"))
 	(buffer-substring (point-min) (1- (point-max)))
       nil)))
(defun gtags-update-single(filename)
   "Update Gtags database for changes in a single file"
   (interactive)
   (start-process "update-gtags" "update-gtags" "bash" "-c" (concat "cd " (gtags-root-dir) " ; gtags --single-update " filename )))
(defun gtags-update-current-file()
  (interactive)
  (defvar filename)
  (setq filename (replace-regexp-in-string (gtags-root-dir) "." (buffer-file-name (current-buffer))))
  (gtags-update-single filename)
  (message "Gtags updated for %s" filename))
(defun gtags-update-hook()
  "Update GTAGS file incrementally upon saving a file"
  (when gtags-mode
    (when (gtags-root-dir)
      (gtags-update-current-file))))
(add-hook 'after-save-hook 'gtags-update-hook)

(add-hook 'gtags-mode-hook 
	  (lambda() (interactive)
	    (local-set-key (kbd "M-.") 'gtags-find-tag)
	    (local-set-key (kbd "M-,") 'gtags-find-rtag)
	    (local-set-key (kbd "C-M-,") 'gtags-find-symbol)
	    ))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (require 'gtags)
	    (gtags-mode t)))
(setq gtags-mode 0) ;default
;;;;;;;

;;;;;;; auto complete
(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories (concat myoptdir "AC/ac-dict"))
(require 'auto-complete-clang)
(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map  [M-tab] 'auto-complete)
(defun my-ac-config ()
  (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  ;;(add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
  ;;(add-hook 'css-mode-hook 'ac-css-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(defun my-ac-cc-mode-setup ()
  (setq ac-sources (append '(ac-source-clang ac-source-yasnippet ac-source-gtags) ac-sources)))
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(my-ac-config)

(setq ac-clang-flags-string
 "-D_DEBUG -D__cplusplus -DSP_LIBSPOTIFY=1 -DSP_WITH_SOCIAL=1 -DSP_LIBSPOTIFY_WITH_SOCIAL=1 -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug -I/Users/dag/Documents/Dev/gitrepos/libspotify/ -I/Users/dag/Documents/Dev/gitrepos/libspotify/client -I/Users/dag/Documents/Dev/gitrepos/libspotify/src -I/Users/dag/Documents/Dev/gitrepos/libspotify/client/base/lib -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug/obj/log-parser -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug/obj/boink -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug/obj/protobuf -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug/obj/passive_boink -I/Users/dag/Documents/Dev/gitrepos/libspotify/client/boink/cpp"
 )
(setq ac-clang-flags
      (split-string ac-clang-flags-string))
(setq c-macro-preprocessor "cpp -xc++")
(setq c-macro-cppflags ac-clang-flags-string)
;;;;;;;;;
;;;;;;;;;

; Enable jumping between cpp and header file using keyboard shortcut
(global-set-key (kbd "M-o") 'ff-find-other-file)
; Make sure we can find headers where we want them
(setq cc-search-directories
      '("." ".." "../.."))

; Enable copying between dired windows
(setq dired-dwim-target t)

; uniquify - use sensibel buffer names
(require 'uniquify)

; multi eshell support
(require 'multi-eshell)
; make eshell tab completion behave like bash
(setq eshell-cmpl-cycle-completions nil)


; ediff settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; integrate control panel in active frame
(setq ediff-split-window-function 'split-window-horizontally) ; split horiz
;(setq ediff-diff-options "-w") ; ignore white space
;(setq-default ediff-ignore-similar-regions t)


;;;;;;;;;;;;;;;;;;;

; Show trailing white space
;(setq-default show-trailing-whitespace t)

; CMake major mode
(setq load-path (cons (expand-file-name "~/emacs/cmake-mode.el") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

; Egg - Emacs interface to git
;(require 'egg)

; Magit - Emacs interface to git
(require 'magit)
(autoload 'magit-status "magit" nil t)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(setq mo-git-blame-use-ido t)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

;; indent whole buffer. M-x iwb
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))


; Activate elscreen with independent most-recently-used-buffer-lists
;(require 'elscreen-buffer-list)


(require 'buffer-stack)
;(global-set-key [(f9)] 'buffer-stack-bury)
;(global-set-key [(control f9)] 'buffer-stack-bury-and-kill)
;(global-set-key [(f10)] 'buffer-stack-up)
(global-set-key [(control f11)] 'buffer-stack-down)
(global-set-key [(control f12)] 'buffer-stack-track)
;(global-set-key [(control f12)] 'buffer-stack-untrack)

; Undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C-z") 'undo-tree-undo)
(global-set-key (kbd "C-S-z") 'undo-tree-redo)

; Visual bookmarks
(require 'bm)

; Switch between windows using cursors keys
(require 'windmove)
(windmove-default-keybindings 'meta)

;; Use left option key as meta, and right option key as ALT-GR
;(setq mac-option-key-is-meta t)
;(setq mac-right-option-modifier nil)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'ctrl)

;; Full screen toogle
(global-set-key (kbd "C-S-F") 'ns-toggle-fullscreen)

;; Revert buffer, will disable to mapping to list directory that I don't
;; use
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(global-set-key (kbd "C-x C-d") 'revert-buffer-no-confirm)

;; Global function key mappings
;(global-set-key (kbd "M-<f1>") (lambda () (interactive) (elscreen-goto 0)))
;(global-set-key (kbd "M-<f2>") (lambda () (interactive) (elscreen-goto 1)))
;(global-set-key (kbd "M-<f3>") (lambda () (interactive) (elscreen-goto 2)))
(global-set-key (kbd "C-<f6>") 'magit-status)
(global-set-key (kbd "C-<f7>") 'flymake-mode)
(global-set-key (kbd "<f5>") 'gud-gdb)
(global-set-key (kbd "<f7>") 'recompile)
;(global-set-key (kbd "C-<f7>") 'compile)
(global-set-key (kbd "C-<f8>") 'multi-eshell)
(global-set-key (kbd "C-<f9>") 'sunrise-cd)
(global-set-key (kbd "C-<f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<f4>")   'auto-complete)

;(global-set-key (kbd "C-<f4>") 'gtags-update-hook)

;; Use Ctrl-H as backspace
;(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key key-translation-map [?\C-h] [?\C-?])

;; Use Ctrl-Ã Ã¤s beginning of buffer
(define-key key-translation-map (kbd "C-ö") 'beginning-of-buffer)
(define-key key-translation-map (kbd "C-ä") 'end-of-buffer)

;; Make Ctrl-W function as backward-kill-word if region is not active
(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

;; Make ALT-key function for entering some special characters on a Mac keyboard
(global-set-key (kbd "M-2") (lambda() (interactive) (insert "@")))
(global-set-key (kbd "M-8") (lambda() (interactive) (insert "[")))
(global-set-key (kbd "M-9") (lambda() (interactive) (insert "]")))
(global-set-key (kbd "M-(") (lambda() (interactive) (insert "{")))
(global-set-key (kbd "M-)") (lambda() (interactive) (insert "}")))
(global-set-key (kbd "M-4") (lambda() (interactive) (insert "$")))
(global-set-key (kbd "M-/") (lambda() (interactive) (insert "\\")))
(global-set-key (kbd "M-7") (lambda() (interactive) (insert "|")))

;; Use Ctrl-x m as a shortcut for Alt-X (execute-extended-command)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-c C-m") 'execute-extended-command) ; In case I mis-type
(global-set-key (kbd "C-x m")   'execute-extended-command) ; In case I mis-type

;: Make sure i never miss a tab char again when editing C/C++
;(defface extra-whitespace-face
;  '((t (:background "pale green")))
;  "Used for tabs and such.")
;(defvar my-extra-keywords
;  '(("\t" . 'extra-whitespace-face)))
;(add-hook 'c-mode-common-hook
;	  (lambda () (font-lock-add-keywords nil my-extra-keywords)))
; Set the cursor to wide, covering a whole tab
;(setq x-stretch-cursor t)

;; Turn off sound beep
;(setq bell-volume 0)
;(setq sound-alist nil)
(setq ring-bell-function 'ignore)


;; Use CUA mode for rectangle editing (only)
(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)

;; Show 100-line rule, show white space problems
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing tab-mark))
(setq whitespace-line-column 100)
(global-whitespace-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake
; Show flymake errors in minibuffer
(eval-after-load 'flymake '(require 'flymake-cursor))
; Use underline instead of highlight for flymake errors
(custom-set-faces
 '(flymake-errline ((((class color)) (:underline "red"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))
; Enable fly make for modes that can be checked
(add-hook 'find-file-hook 'flymake-find-file-hook)
; Avoid the error message box where flymake is not possible
(setq flymake-gui-warnings-enabled nil)
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)

;;-------------
;;Add color to the current GUD line (obrigado google)

(defvar gud-overlay
(let* ((ov (make-overlay (point-min) (point-min))))
(overlay-put ov 'face 'secondary-selection)
ov)
"Overlay variable for GUD highlighting.")

(defadvice gud-display-line (after my-gud-highlight act)
"Highlight current line."
(let* ((ov gud-overlay)
(bf (gud-find-file true-file)))
(save-excursion
  (set-buffer bf)
  (move-overlay ov (line-beginning-position) (line-beginning-position 2)
  (current-buffer)))))

(defun gud-kill-buffer ()
(if (eq major-mode 'gud-mode)
(delete-overlay gud-overlay)))

(add-hook 'kill-buffer-hook 'gud-kill-buffer)
;;-------------------------------------------------------------


