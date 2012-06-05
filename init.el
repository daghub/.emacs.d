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
	  (lambda()
	    (local-set-key (kbd "M-.") 'gtags-find-tag)
	    (local-set-key (kbd "M-,") 'gtags-find-rtag)
	    (local-set-key [(control meta ,)] 'gtags-find-symbol)
	    ))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (require 'gtags)
	    (gtags-mode t)))
;;;;;;;

;;;;;;; auto complete
(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories (concat myoptdir "AC/ac-dict"))
(require 'auto-complete-clang)
(setq ac-auto-start nil)
(setq ac-quick-help-delay 0.5)
;; (ac-set-trigger-key "TAB")
;; (define-key ac-mode-map  [(control tab)] 'auto-complete)
(define-key ac-mode-map  [(f4)] 'auto-complete)
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
;; ac-source-gtags
(my-ac-config)

(setq ac-clang-flags
      (split-string
  "
 -DSP_LIBSPOTIFY=1
 -DSP_WITH_SOCIAL=1
 -DSP_LIBSPOTIFY_WITH_SOCIAL=1
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/client
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/src
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/client/base/lib
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug/obj/log-parser 
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug/obj/boink
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug/obj/protobuf
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/targets/Darwin-x86_64-debug/obj/passive_boink
 -I/Users/dag/Documents/Dev/gitrepos/libspotify/client/boink/cpp
 "
               ))

;;;;;;;;;
;;;;;;;;;

; Enable jumping between cpp and header file using keyboard shortcut
(global-set-key (kbd "M-o") 'ff-find-other-file)

; Enable copying between dired windows
(setq dired-dwim-target t)

; uniquify - use sensibel buffer names
(require 'uniquify)

; multi eshell support
(require 'multi-eshell)
; make eshell tab completion behave like bash
(setq eshell-cmpl-cycle-completions nil)

; enable winner mode (Let's you jump to previous window configurations)
(winner-mode 1)
(windmove-default-keybindings 'meta)
;; (global-set-key (kbd "<left>")  'windmove-left)
;; (global-set-key (kbd "<right>") 'windmove-right)
;; (global-set-key (kbd "<up>")    'windmove-up)
;; (global-set-key (kbd "<down>")  'windmove-down)

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

;;;;;;;;;;;;;;;;;;;;;
;; ERC
;;;;;;;;;;;;;;;;;;;;;
;(require 'tls)
;(require 'erc)
;(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -CAfile /opt/local/etc/openssl/ca-certs.pem"))

; M-x start-irc
;(defun start-irc ()
;  "Connect to IRC."
;  (interactive)
;  (erc-tls :server "irc.spotify.net" :port 7000
;	   :nick "dag" :full-name "Dag Ekengren" :password "OwivCyin8")
;;   (setq erc-autojoin-channels-alist '(("irc.spotify.net" "#platform" "#libspotify" "#client" "#operations")))
;;   )

;; ; Notify growl when I'm mentioned
;; (defvar growlnotify-command (executable-find "growlnotify") "The path to growlnotify")

;; (defun growl (title message)
;;   "Shows a message through the growl notification system using
;;  `growlnotify-command` as the program."
;;   (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
;;     (let* ((process (start-process "growlnotify" nil
;;                                    growlnotify-command
;;                                    (encfn title)
;;                                    "-a" "Emacs"
;;                                    "-n" "Emacs")))
;;       (process-send-string process (encfn message))
;;       (process-send-string process "\n")
;;       (process-send-eof process)))
;;   t)

;; (defun my-erc-hook (match-type nick message)
;;   "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
;;   (unless (posix-string-match "^\\** *Users on #" message)
;;     (growl
;;      (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
;;      message
;;      )))

;; (add-hook 'erc-text-matched-hook 'my-erc-hook)

; Start erc on emacs startup
;(start-irc)

; Activate elscreen with independent most-recently-used-buffer-lists
(require 'elscreen-buffer-list)


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
(global-set-key (kbd "C--") 'undo-tree-undo)
(global-set-key (kbd "M--") 'undo-tree-redo)

; Visual bookmarks
(require 'bm)

;; Use left option key as meta, and right option key as ALT-GR
;(setq mac-option-key-is-meta t)
;(setq mac-right-option-modifier nil)
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'ctrl)

;; Full screen toogle
(global-set-key (kbd "C-S-F") 'ns-toggle-fullscreen)

;; Global function key mappings
(global-set-key (kbd "M-<f1>") (lambda () (interactive) (elscreen-goto 0)))
(global-set-key (kbd "M-<f2>") (lambda () (interactive) (elscreen-goto 1)))
(global-set-key (kbd "M-<f3>") (lambda () (interactive) (elscreen-goto 2)))
(global-set-key (kbd "C-<f6>") 'magit-status)
(global-set-key (kbd "C-<f7>") 'compile)
(global-set-key (kbd "C-<f8>") 'multi-eshell)
(global-set-key (kbd "C-<f9>") 'sunrise-cd)
(global-set-key (kbd "C-<f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
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


; FLymake
;; flymake
(defun my-flymake-show-next-error()
  (interactive)
  (flymake-goto-next-error)
  (flymake-display-err-menu-for-current-line)
  )

;;
;; Setting some C / C++ defaults
;;
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      ;; more stuff here
                      (flymake-mode t)
                      )))



