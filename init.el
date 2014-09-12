; Emacs config file
(server-start) ;; allow emacs-client to connect

;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

; Add load directory
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/apel-10.8")
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/git-modes")
(add-to-list 'load-path "~/.emacs.d/replace-plus")
(add-to-list 'load-path "~/.emacs.d/auto-complete-etags")
(add-to-list 'load-path "~/.emacs.d/plantuml-mode")

;; Treat .h files at c++ headers
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Color theme
;(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(load-theme 'deeper-blue t)

;; Disable scrollbars and toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)
;(menu-bar-mode -1)

; Avoid emacs creating backup files
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;
;ido
(require 'ido)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(setq ido-create-new-buffer 'always)
(setq ido-file-extension-order '(".org" ".txt", ".el"))
(ido-mode t)

;; Install fic (Fixme-in-comments) that will highlight TODO/FIXME/BUG
(require 'fic-ext-mode)
(add-hook 'c-mode-common-hook 'fic-ext-mode)

;; PlantUML mode
(autoload 'plantuml-mode "plantuml-mode" nil t)
(setq auto-mode-alist
      (append '(("\\.uml$" . plantuml-mode)) auto-mode-alist))
;; active Org-babel languages
(org-babel-do-load-languages
 'org-babel-load-languages
 '(;; other Babel languages
   (plantuml . t)))
(setq org-plantuml-jar-path
      (expand-file-name "~/plantuml.jar"))

;; open *help* in current frame
(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

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
  (if (and (boundp 'gtags-mode) gtags-mode)
      (when (gtags-root-dir)
	(gtags-update-current-file))))
(add-hook 'after-save-hook 'gtags-update-hook)


(add-hook 'gtags-mode-hook 
      (lambda()
        (local-set-key (kbd "M-.") 'gtags-find-tag-from-here)
        (local-set-key (kbd "M-,") 'gtags-find-rtag)
        (local-set-key (kbd "C-M-,") 'gtags-find-symbol)
        (local-set-key (kbd "M-ö") 'gtags-pop-stack)
        ))

(add-hook 'gtags-select-mode-hook 
      (lambda()
        (local-set-key (kbd "M-ö") 'gtags-pop-stack)
        ))

(add-hook 'c-mode-common-hook
      (lambda ()
        (require 'gtags)
        (gtags-mode t)))


;;;;;;; auto complete
;(require 'completion-ui)
;(autoload 'auto-completion-mode "auto-completion-mode" "a" t)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-stop-flymake-on-completing t)
(require 'auto-complete-etags)

; Enable objective-C mode for .mm files automatically
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

; Enable jumping between cpp and header file using keyboard shortcut
(global-set-key (kbd "M-o") 'ff-find-other-file)
; Make sure we can find headers where we want them
(setq cc-search-directories
      '("." ".." "../.."))

; Make sure we indent C/C++ with 4 spaces
(setq c-default-style "linux"
      c-basic-offset 4)
(setq-default indent-tabs-mode nil)
; Enable copying between dired windows
(setq dired-dwim-target t)

; uniquify - use sensibel buffer names
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

; Enable common emacs extentions
(require 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(autoload 'ibuffer "ibuffer" "List buffer" t)
(setq column-number-mode t)
(show-paren-mode 1)


; multi eshell support
(require 'multi-eshell)
; make eshell tab completion behave like bash
(setq eshell-cmpl-cycle-completions nil)


; ediff settings
(setq ediff-window-setup-function 'ediff-setup-windows-plain) ; integrate control panel in active frame
(setq ediff-split-window-function 'split-window-horizontally) ; split horiz
;(setq ediff-diff-options "-w") ; ignore white space
;(setq-default ediff-ignore-similar-regions t)
;; Some custom configuration to ediff
(defvar my-ediff-bwin-config nil "Window configuration before ediff.")
(defcustom my-ediff-bwin-reg ?b
  "*Register to be set up to hold `my-ediff-bwin-config'
    configuration.")

(defvar my-ediff-awin-config nil "Window configuration after ediff.")
(defcustom my-ediff-awin-reg ?e
  "*Register to be used to hold `my-ediff-awin-config' window
    configuration.")

(defun my-ediff-bsh ()
  "Function to be called before any buffers or window setup for
    ediff."
  (setq my-ediff-bwin-config (current-window-configuration))
  (when (characterp my-ediff-bwin-reg)
    (set-register my-ediff-bwin-reg
		  (list my-ediff-bwin-config (point-marker)))))

(defun my-ediff-ash ()
  "Function to be called after buffers and window setup for ediff."
  (setq my-ediff-awin-config (current-window-configuration))
  (when (characterp my-ediff-awin-reg)
    (set-register my-ediff-awin-reg
		  (list my-ediff-awin-config (point-marker)))))

(defun my-ediff-qh ()
  "Function to be called when ediff quits."
  (when my-ediff-bwin-config
    (set-window-configuration my-ediff-bwin-config)))

(add-hook 'ediff-before-setup-hook 'my-ediff-bsh)
(add-hook 'ediff-after-setup-windows-hook 'my-ediff-ash 'append)
(add-hook 'ediff-quit-hook 'my-ediff-qh)

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

; Magit - Emacs interface to git
;(require 'magit-popup)
(require 'magit-blame)
;(require 'magit-svn)
;(autoload 'magit-status "magit" nil t)
(eval-after-load 'info
  '(progn (info-initialize)
          (add-to-list 'Info-directory-list "~/.emacs.d/magit/")))
(require 'magit)

;; indent whole buffer. M-x iwb
(defun iwb ()
  "indent whole buffer"
  (interactive)
  (delete-trailing-whitespace)
  (indent-region (point-min) (point-max) nil)
  (untabify (point-min) (point-max)))

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
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)
(setq mac-command-modifier 'ctrl)

;; Revert buffer, will disable to mapping to list directory that I don't
;; use
(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive) (revert-buffer t t))
(global-set-key (kbd "C-x C-d") 'revert-buffer-no-confirm)

;; Global function key mappings
(global-set-key (kbd "C-<f6>") 'magit-status)
(global-set-key (kbd "C-<f3>") 'flymake-mode)
(global-set-key (kbd "S-<f3>") 'flymake-goto-prev-error)
(global-set-key (kbd "<f3>") 'flymake-goto-next-error)
(global-set-key (kbd "<f5>") 'gud-gdb)
(global-set-key (kbd "<f7>") 'recompile)
;(global-set-key (kbd "C-<f7>") 'compile)
(global-set-key (kbd "C-<f8>") 'multi-eshell)
(global-set-key (kbd "C-<f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)
(global-set-key (kbd "<f4>")   'auto-complete)

;; Use Ctrl-H as backspace
;(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
(define-key key-translation-map [?\C-h] [?\C-?])

;; Use Ctrl-Ã Ã¤s beginning of buffer
(define-key key-translation-map (kbd "C-ö") 'beginning-of-buffer)
(define-key key-translation-map (kbd "C-ä") 'end-of-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Make Ctrl-W function as backward-delete-word if region is not active
(defun delete-word (arg)
  "Delete characters forward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (forward-word arg) (point))))

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the end of a word.
With argument, do this that many times."
  (interactive "p")
  (delete-word (- arg)))

(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-delete-word arg)))
(dolist (cmd '(delete-word backward-delete-word))
  (put cmd 'CUA 'move))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Delete a region if a key is pressed
(delete-selection-mode t)

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

;; Turn off sound beep
(setq ring-bell-function 'ignore)


;; Use CUA mode for rectangle editing (only)
(setq cua-enable-cua-keys nil) ;; only for rectangles
(cua-mode t)

;; Show 100-line rule, show white space problems
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing tab-mark))
(setq whitespace-line-column 100)
;;(global-whitespace-mode t)

; try to improve slow performance on windows.
(setq w32-get-true-file-attributes nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; flymake
; Show flymake errors in minibuffer
(eval-after-load 'flymake '(require 'flymake-cursor))

(add-hook 'c-mode-common-hook
	  (function (lambda()
		      (flymake-mode t)
		      )
		    )
	  )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(flymake-errline ((((class color)) (:underline "orange"))))
 '(flymake-warnline ((((class color)) (:underline "yellow")))))


; Avoid the error message box where flymake is not possible
(setq flymake-gui-warnings-enabled nil)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Compilation window. Hides the window after a second if successful
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-compile-buffer-if-successful (buffer string)
  " kill a compilation buffer if succeeded without warnings "
  (if (and
       (string-match "compilation" (buffer-name buffer))
       (string-match "finished" string)
       (not
        (with-current-buffer buffer
          (search-forward "warning" nil t))))
      (run-with-timer 1 nil
                      'kill-buffer
                      buffer)))
(add-hook 'compilation-finish-functions 'kill-compile-buffer-if-successful)
; Automatically scroll the output to the first error
(setq compilation-scroll-output 'first-error)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Enable isearch-like editing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'iedit)
(defun iedit-dwim (arg)
  "Starts iedit but uses \\[narrow-to-defun] to limit its scope."
  (interactive "P")
  (if arg
      (iedit-mode)
    (save-excursion
      (save-restriction
        (widen)
        ;; this function determines the scope of `iedit-start'.
        (narrow-to-defun)
        (if iedit-mode
            (iedit-done)
          ;; `current-word' can of course be replaced by other
          ;; functions.
          (iedit-start (current-word)))))))

(global-set-key (kbd "C-;") 'iedit-mode)

;;;;
;;;; Windows stuff
;;;;
;;
(let* ((bintools-root "C:/Program Files (x86)/Git")
       (bintools-bin (concat bintools-root "/bin")))
  (when (and (eq 'windows-nt system-type)
  	     (file-readable-p bintools-root))

    (setq exec-path (cons bintools-bin exec-path))
    (setenv "PATH" (concat (concat bintools-bin ";") (getenv "PATH")))

    ;; By default use the Windows HOME.
    ;; Otherwise, uncomment below to set a HOME
    ;;      (setenv "HOME" (concat cygwin-root "/home/eric"))

    ;; NT-emacs assumes a Windows shell. Change to bash.
    (setq shell-file-name "bash")
    (setenv "SHELL" shell-file-name)
    (setq explicit-shell-file-name shell-file-name)
    ;; This removes unsightly ^M characters that would otherwise
    ;; appear in the output of java applications.
    (add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
    (global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Replace+
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(eval-after-load "replace" '(progn (require 'replace+)))
(substitute-key-definition 'query-replace 'query-replace-w-options
			   global-map)
;(setq search/replace-region-as-default-flag t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\)$" . org-mode))
(global-set-key "\C-cc" 'org-capture)
;(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-directory "~/SkyDrive @ Microsoft/org")
(setq org-default-notes-file (concat org-directory "/refile.org"))
(setq org-agenda-files (quote( "~/SkyDrive @ Microsoft/org" )))
;; Capture templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/SkyDrive @ Microsoft/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("n" "note" entry (file "~/SkyDrive @ Microsoft/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("m" "Meeting" entry (file "~/SkyDrive @ Microsoft/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/SkyDrive @ Microsoft/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"<%Y-%m-%d %a .+1d/3d>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
; Use full outline paths for refile targets - we file directly with IDO
(setq org-refile-use-outline-path t)
; Targets complete directly with IDO
(setq org-outline-path-complete-in-steps nil)
; Allow refile to create parent tasks with confirmation
(setq org-refile-allow-creating-parent-nodes (quote confirm))
;;;; Refile settings
; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))
(setq org-refile-target-verify-function 'bh/verify-refile-target)

; Dos2unix
(defun dos2unix ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'unix 't) )
(defun unix2dos ()
  "Not exactly but it's easier to remember"
  (interactive)
  (set-buffer-file-coding-system 'utf-8-dos 't) )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "a81fbcd4c2903eca49c448680055afc1a0e534bf454f2d83edbfc5e4259aa789" default))))
