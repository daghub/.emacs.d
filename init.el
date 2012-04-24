; Emacs config file

; Add load directory
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/apel-10.8")
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(add-to-list 'load-path "~/.emacs.d/sunrise-commander")
(add-to-list 'load-path "~/.emacs.d/magit")

; Avoid emacs creating backup files
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;
;ido
(require 'ido)
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

; Enable incremental update of Gnu Global GTAGS when saving a file
(defun gtags-root-dir ()
  "Returns GTAGS root directory or nil if doesn't exist."
  (with-temp-buffer
    (if (zerop (call-process "global" nil t nil "-pr"))
	(buffer-substring (point-min) (1- (point-max)))
      nil)))
(defun gtags-update ()
  "Make GTAGS incremental update"
  (call-process "global" nil nil nil "-u"))
(defun gtags-update-hook ()
  (when (gtags-root-dir)
    (gtags-update)))
(add-hook 'after-save-hook #'gtags-update-hook)

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
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/targets/Darwin-x86_64-debug
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/client
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/src
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/client/base/lib
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/targets/Darwin-x86_64-debug/obj/log-parser 
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/targets/Darwin-x86_64-debug/obj/boink
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/targets/Darwin-x86_64-debug/obj/protobuf
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/targets/Darwin-x86_64-debug/obj/passive_boink
 -I/Users/dag/Documents/Dev/gitrepos/alt.libspotify/client/boink/cpp
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

; enable winner mode (Let's you jump to previous window configurations)
(winner-mode 1)
(windmove-default-keybindings 'meta)

; ediff settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; integrate control panel in active frame
(setq ediff-split-window-function 'split-window-horizontally) ; split horiz
(setq ediff-diff-options "-w") ; ignore white space
(setq-default ediff-ignore-similar-regions t)

; iswitchb stuff
;(require 'edmacro) ; avoid Symbol’s function definition is void: edmacro-parse-keys errorc
;(iswitchb-mode 1)
;(iswitchb-default-keybindings)
;(setq iswitchb-buffer-ignore '("^ " "*Buffer"))

;(defun iswitchb-local-keys ()
;  (mapc (lambda (K) 
;	  (let* ((key (car K)) (fun (cdr K)))
;	    (define-key iswitchb-mode-map (edmacro-parse-keys key) fun)))
;	'(("<right>" . iswitchb-next-match)
;	  ("<left>"  . iswitchb-prev-match)
;	  ("<up>"    . ignore             )
;	  ("<down>"  . ignore             ))))

;(add-hook 'iswitchb-define-mode-map-hook 'iswitchb-local-keys)



;;;;;;;;;;;;;;;;;;;

; Enable function that will select the tab offset based on the file edited (by guessing)
(require 'guess-offset)

; Show trailing white space
(setq-default show-trailing-whitespace t)

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


; Icicles
;(require 'icicles)
;(icy-mode 1)

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
(require 'tls)
(require 'erc)
(setq tls-program '("openssl s_client -connect %h:%p -no_ssl2 -ign_eof -CAfile /opt/local/etc/openssl/ca-certs.pem"))

; M-x start-irc
(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc-tls :server "irc.spotify.net" :port 7000
	   :nick "dag" :full-name "Dag Ekengren" :password "OwivCyin8")
  (setq erc-autojoin-channels-alist '(("irc.spotify.net" "#platform" "#libspotify" "#client" "#operations")))
  )

; Notify growl when I'm mentioned
(defvar growlnotify-command (executable-find "growlnotify") "The path to growlnotify")

(defun growl (title message)
  "Shows a message through the growl notification system using
 `growlnotify-command` as the program."
  (flet ((encfn (s) (encode-coding-string s (keyboard-coding-system))) )
    (let* ((process (start-process "growlnotify" nil
                                   growlnotify-command
                                   (encfn title)
                                   "-a" "Emacs"
                                   "-n" "Emacs")))
      (process-send-string process (encfn message))
      (process-send-string process "\n")
      (process-send-eof process)))
  t)

(defun my-erc-hook (match-type nick message)
  "Shows a growl notification, when user's nick was mentioned. If the buffer is currently not visible, makes it sticky."
  (unless (posix-string-match "^\\** *Users on #" message)
    (growl
     (concat "ERC: name mentioned on: " (buffer-name (current-buffer)))
     message
     )))

(add-hook 'erc-text-matched-hook 'my-erc-hook)

; Start erc on emacs startup
;(start-irc)

;;;;;;;;;;;;;;;;;;;
;; windows/revive
;;;;;;;;;;;;;;;;;;;
;(require 'windows)
;(win:startup-with-window)
;(define-key ctl-x-map "C" 'see-you-again)

; Activate elscreen with independent most-recently-used-buffer-lists
(require 'elscreen-buffer-list)

;; F9 creates a new elscreen, shift-F9 kills it
;(global-set-key (kbd "<f9>"    ) 'elscreen-create)
;(global-set-key (kbd "S-<f9>"  ) 'elscreen-kill)  


;; Windowskey+PgUP/PgDown switches between elscreens
;(global-set-key (kbd "<f8>") 'elscreen-previous) 
;(global-set-key (kbd "S-<f8>")  'elscreen-next) 

;(require 'workgroups)
;(workgroups-mode 1)
;(setq wg-prefix-key (kbd "C-c a"))
;(wg-load "~/.emacs.d/.workgroups")


(require 'buffer-stack)
;(global-set-key [(f9)] 'buffer-stack-bury)
;(global-set-key [(control f9)] 'buffer-stack-bury-and-kill)
;(global-set-key [(f10)] 'buffer-stack-up)
(global-set-key [(control f11)] 'buffer-stack-down)
(global-set-key [(control f12)] 'buffer-stack-track)
;(global-set-key [(control f12)] 'buffer-stack-untrack)

;; iflipb - flip buffer with alt-TAB

;(require 'iflipb)

;(global-set-key (kbd "<M-tab>") 'iflipb-next-buffer)
;(global-set-key (kbd "<M-S-tab>") 'iflipb-previous-buffer)


;(autoload 'cycle-buffer "cycle-buffer" "Cycle forward." t)
;(autoload 'cycle-buffer-backward "cycle-buffer" "Cycle backward." t)
;(autoload 'cycle-buffer-permissive "cycle-buffer" "Cycle forward allowing *buffers*." t)
;(autoload 'cycle-buffer-backward-permissive "cycle-buffer" "Cycle backward allowing *buffers*." t)
;(autoload 'cycle-buffer-toggle-interesting "cycle-buffer" "Toggle if this buffer/ will be considered." t)
;(global-set-key [kbd "<M-tab>"]        'cycle-buffer-backward)
;(global-set-key [kbd "<M-S-tab>"]       'cycle-buffer)
;;(global-set-key [(shift f9)]  'cycle-buffer-backward-permissive)
;;(global-set-key [(shift f10)] 'cycle-buffer-permissive)

; Undo-tree
(require 'undo-tree)
(global-undo-tree-mode)
(global-set-key (kbd "C--") 'undo-tree-undo)
(global-set-key (kbd "M--") 'undo-tree-redo)


;; Use left option key as meta, and right option key as ALT-GR
(setq mac-option-key-is-meta t)
(setq mac-right-option-modifier nil)

;; Global function key mappings
(global-set-key (kbd "C-<f1>") (lambda () (interactive) (elscreen-goto 0)))
(global-set-key (kbd "C-<f2>") (lambda () (interactive) (elscreen-goto 1)))
(global-set-key (kbd "C-<f3>") (lambda () (interactive) (elscreen-goto 2)))
(global-set-key (kbd "C-<f6>") 'magit-status)
(global-set-key (kbd "C-<f7>") 'compile)

