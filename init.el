; Emacs config file
;(server-start) ;; allow emacs-client to connect

;; Hide splash-screen and startup-message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

; Add load directory
(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/apel-10.8")
(add-to-list 'load-path "~/.emacs.d/auto-complete-1.3.1")
(add-to-list 'load-path "~/.emacs.d/sunrise-commander")
(add-to-list 'load-path "~/.emacs.d/magit")
(add-to-list 'load-path "~/.emacs.d/mo-git-blame")
(add-to-list 'load-path "~/.emacs.d/color-theme-6.6.0")
;(add-to-list 'load-path "~/.emacs.d/emacs-color-theme-solarized")
(add-to-list 'load-path "~/.emacs.d/darkroom")
(add-to-list 'load-path "~/.emacs.d/emacs-google-this")
(add-to-list 'load-path "~/.emacs.d/auto-complete-etags")
(add-to-list 'load-path "~/.emacs.d/plantuml-mode")

;; Treat .h files at c++ headers
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Color theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/emacs-color-theme-solarized/")
(load-theme 'solarized-dark t)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/zenburn-emacs/")
;(require 'color-theme-solarized)
;(color-theme-solarized-dark)
;(load-theme 'zenburn t)


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
(setq ido-use-filename-at-point 'guess)
(ido-mode t)
; idomenu allows ido to search iMenu results for a buffer
;(autoload 'idomenu "idomenu" nil t)
;(add-hook 'c-mode-common-hook 'imenu-add-menubar-index)
;(global-set-key (kbd "M-i") 'idomenu)

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

;; Csharp-mode
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))
(defun my-csharp-mode-fn ()
  "function that runs when csharp-mode is initialized for a buffer."
  (turn-on-auto-revert-mode)
  (setq indent-tabs-mode nil)
  ;(setq tab-width 4)
  ;(setq c-basic-offset 4)
  (auto-complete-mode t)
  (make-local-variable 'ac-sources)
  (add-to-list 'ac-sources 'ac-source-etags)
  (require 'flymake)
  (flymake-mode 1)
  ;(require 'yasnippet)
  ;(yas/minor-mode-on)
  ;(setq auto-completion-source 'etags)
  ;(define-key csharp-mode-map (kbd "<f4>") 'complete-etags)
  ;(define-key csharp-mode-map (kbd "C-<f4>") 'auto-completion-mode)
;;  ;;(require 'rfringe)
  )
(add-hook  'csharp-mode-hook 'my-csharp-mode-fn)

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
;(require 'google-c-style)

;(add-hook 'c-mode-common-hook 'google-set-c-style)
;(add-hook 'c-mode-common-hook 'google-make-newline-indent)

;; open *help* in current frame
(setq special-display-regexps (remove "[ ]?\\*[hH]elp.*" special-display-regexps))


;;;;;;; auto complete
;(require 'completion-ui)
;(autoload 'auto-completion-mode "auto-completion-mode" "a" t)

(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)
(setq ac-stop-flymake-on-completing t)
(require 'auto-complete-etags)

;; (defun ac-clang-setup()
;;   (when (gtags-root-dir)
;;     (defvar filename)
;;     (setq filename (concat (gtags-root-dir) "/ac-config.el"))
;;     (when (file-exists-p filename)
;;       (load filename)
;;       (require 'auto-complete-clang)
;;       (setq sp-include-dirs
;; 	    (mapcar
;; 	     (lambda(d) (concat "-I" (gtags-root-dir) "/" d))
;; 	     sp-include-dirs
;; 	     )
;; 	    )
;;       (setq ac-clang-flags
;; 	    (append sp-compile-flags sp-include-dirs)
;; 	    )

;;       (add-to-list 'ac-omni-completion-sources
;;       		   (cons "\\." '(ac-source-clang)))
;;       (add-to-list 'ac-omni-completion-sources
;;       		   (cons "->" '(ac-source-clang)))
;;       (setq clang-completion-suppress-error t)
;;       (setq ac-sources '(ac-source-clang))
;;       )
;;     )
;;   )
;; (add-hook 'c-mode-common-hook 'ac-clang-setup)

; Enable objective-C mode for .mm files automatically
(add-to-list 'auto-mode-alist '("\\.mm$" . objc-mode))

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
(require 'magit-svn)
(autoload 'magit-status "magit" nil t)
(autoload 'mo-git-blame-file "mo-git-blame" nil t)
(autoload 'mo-git-blame-current "mo-git-blame" nil t)
(setq mo-git-blame-use-ido t)
(add-hook
 'magit-mode-hook
 (lambda ()
   (require 'rebase-mode)))
(add-hook
 'magit-mode-hook
 (lambda ()
   (turn-on-magit-svn)))
(eval-after-load 'rebase-mode
  '(progn
     (define-key rebase-mode-map (kbd "E") 'toggle-read-only)))
(add-hook
 'magit-log-edit-mode-hook
 (lambda ()
   (set (make-local-variable 'whitespace-line-column) 70)
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
(global-set-key (kbd "<f9>")   'sunrise)
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
(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND restore #xf120
	  (w32-send-sys-command 61728)
	(progn (set-frame-parameter nil 'width 82)
		   (set-frame-parameter nil 'fullscreen 'fullheight))))

(defun my-fullscreen ()
  (interactive)
  (if (fboundp 'w32-send-sys-command)
	  ;; WM_SYSCOMMAND maximaze #xf030
	  (w32-send-sys-command 61488)
	(set-frame-parameter nil 'fullscreen 'fullboth)))

(defun my-toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
	  (my-non-fullscreen)
	(my-fullscreen)))

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
    (global-set-key (kbd "<f11>") 'my-toggle-fullscreen)
))

(require 'google-this)
(google-this-mode 1)
(global-set-key (kbd "C-x g") 'google-this-mode-submap)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; org-mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;(define-key global-map "\C-cl" 'org-store-link)
;(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" "a81fbcd4c2903eca49c448680055afc1a0e534bf454f2d83edbfc5e4259aa789" default))))
