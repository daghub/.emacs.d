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

;; Treat .h files at c++ headers
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

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

;; Disable scrollbars and toolbars
(scroll-bar-mode -1)
(tool-bar-mode -1)

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

;; Disable the commander style function keys. They are used for other purposes.
;(setq sr-use-commander-keys nil)
(require 'sunrise-commander)
;(require 'sunrise-x-buttons) ; some extra help
(add-to-list 'auto-mode-alist '("\\.srvm\\'" . sr-virtual-mode))
; Open file with b in default application instead of in default browser
(defun sr-browse-file (&optional file)
  "Display the selected file with the default appication."
  (interactive)
  (setq file (or file (dired-get-filename)))
  (save-selected-window
    (sr-select-viewer-window)
    (let ((buff (current-buffer))
	  (fname (if (file-directory-p file)
		     file
		   (file-name-nondirectory file)))
	  (app (cond
		((eq system-type 'darwin)	"open %s")
		((eq system-type 'windows-nt)	"open %s")
		(t				"xdg-open %s"))))
      (start-process-shell-command "open" nil (format app file))
      (unless (eq buff (current-buffer))
        (sr-scrollable-viewer (current-buffer)))
      (message "Opening \"%s\" ..." fname))))


; Call on surise commander from ido
(defun ido-sunrise ()
  "Call `sunrise' the ido way.
    The directory is selected interactively by typing a substring.
    For details on keybindings, see `ido-find-file'."
  (interactive)
  (let ((ido-report-no-match nil)
        (ido-auto-merge-work-directories-length -1))
    (ido-file-internal 'sr-dired 'sr-dired nil "Sunrise: " 'dir)))
(define-key (cdr (assoc 'ido-mode minor-mode-map-alist)) [remap dired] 'ido-sunrise)

; Active google style guides for C/C++
(require 'google-c-style)

(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; open *help* in current frame
(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))

;;;;;;; GNU Global
; Set the environment variable that makes .h mean C++ header rather than C header
(setenv "GTAGSFORCECPP" "1")
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

(add-hook 'gtags-mode-hook
	  (lambda() (interactive)
	    (local-set-key (kbd "M-.") 'gtags-find-tag-from-here)
	    (local-set-key (kbd "M-,") 'gtags-find-with-grep)
	    ))

(add-hook 'c-mode-common-hook
	  (lambda ()
	    (require 'gtags)
	    (gtags-mode t)
	    ; add a local hook
	    (add-hook 'after-save-hook 'gtags-update-hook nil t)
	    ))
(setq gtags-mode 0) ;default
;;;;;;;

;;;;;;; auto complete
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(defun ac-clang-setup()
  (when (gtags-root-dir)
    (defvar filename)
    (setq filename (concat (gtags-root-dir) "/ac-config.el"))
    (when (file-exists-p filename)
      (load filename)
      (require 'auto-complete-clang)
      (setq sp-include-dirs
	    (mapcar
	     (lambda(d) (concat "-I" (gtags-root-dir) "/" d))
	     sp-include-dirs
	     )
	    )
      (setq ac-clang-flags
	    (append sp-compile-flags sp-include-dirs)
	    )

      (setq ac-sources (append '(ac-source-clang) ac-sources))
      )
    )
  )
(add-hook 'c-mode-common-hook 'ac-clang-setup)


; Enable jumping between cpp and header file using keyboard shortcut
(global-set-key (kbd "M-o") 'ff-find-other-file)
; Make sure we can find headers where we want them
(setq cc-search-directories
      '("." ".." "../.."))

; Enable copying between dired windows
(setq dired-dwim-target t)

; uniquify - use sensibel buffer names
(require 'uniquify)
;(uniquify-buffer-name-style (quote post-forward-angle-brackets) nil (uniquify)))

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

; Magit - Emacs interface to git
(require 'magit)
(require 'magit-blame)
(autoload 'magit-status "magit" nil t)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(setq mo-git-blame-use-ido t)
(add-hook
 'magit-mode-hook
 (lambda ()
   (require 'rebase-mode)))
(eval-after-load 'rebase-mode
  '(progn
     (define-key rebase-mode-map (kbd "E") 'toggle-read-only)))
(add-hook
 'magit-log-edit-mode-hook
 (lambda ()
   (set (make-local-variable 'whitespace-line-column) 65)
   (whitespace-mode t)
   )
 )

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
;(setq mac-option-key-is-meta t)
;(setq mac-right-option-modifier nil)
(setq mac-option-modifier 'meta)
(setq mac-right-option-modifier nil)
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
(global-set-key (kbd "C-<f6>") 'magit-status)
(global-set-key (kbd "C-<f3>") 'flymake-mode)
(global-set-key (kbd "S-<f3>") 'flymake-goto-prev-error)
(global-set-key (kbd "<f3>") 'flymake-goto-next-error)
(global-set-key (kbd "<f5>") 'gud-gdb)
(global-set-key (kbd "<f7>") 'recompile)
;(global-set-key (kbd "C-<f7>") 'compile)
(global-set-key (kbd "C-<f8>") 'multi-eshell)
(global-set-key (kbd "C-<f9>") 'sunrise-cd)
(global-set-key (kbd "<f9>") 'sunrise)
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

;; Make Ctrl-W function as backward-kill-word if region is not active
(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive
   (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

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
(global-whitespace-mode t)
;(global-whitespace-newline-mode t)
;; make whitespace-mode use just basic coloring
;(setq whitespace-style (quote (spaces tabs newline space-mark tab-mark newline-mark)))

;; Delete all trailing whitespace when saving
(add-hook 'before-save-hook 'delete-trailing-whitespace)

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
 '(flymake-errline ((((class color)) (:underline "orange"))))
 '(flymake-warnline ((((class color)) (:underline "yellow"))))
)


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
