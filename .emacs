(defvar started (float-time))
(defun print-elapsed-time()
  (message "initialised (%f secs)" (- (float-time) started)))

;; init required packages
(setq package-requirements '())

;; repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defun install-required-packages ()
  ;; update the package list
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-requirements)
    (unless (package-installed-p package)
      (package-install package))))

(add-to-list 'package-requirements 'idomenu)
(defun enhance-ido ()
  ;; (i)nteractively (do) things
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-ubiquitous-mode t))

(defun ido-goto-symbol (&optional symbol-list)
  "Refresh imenu and jump to a place in the buffer using Ido."
  (interactive)
  (unless (featurep 'imenu)
    (require 'imenu nil t))
  (cond
   ((not symbol-list)
    (let ((ido-mode ido-mode)
	  (ido-enable-flex-matching
	   (if (boundp 'ido-enable-flex-matching)
	       ido-enable-flex-matching t))
	  name-and-pos symbol-names position)
      (unless ido-mode
	(ido-mode 1)
	(setq ido-enable-flex-matching t))
      (while (progn
	       (imenu--cleanup)
	       (setq imenu--index-alist nil)
	       (ido-goto-symbol (imenu--make-index-alist))
	       (setq selected-symbol
		     (ido-completing-read "Symbol? " symbol-names))
	       (string= (car imenu--rescan-item) selected-symbol)))
      (unless (and (boundp 'mark-active) mark-active)
	(push-mark nil t nil))
      (setq position (cdr (assoc selected-symbol name-and-pos)))
      (cond
       ((overlayp position)
	(goto-char (overlay-start position)))
       (t
	(goto-char position)))))
   ((listp symbol-list)
    (dolist (symbol symbol-list)
      (let (name position)
	(cond
	 ((and (listp symbol) (imenu--subalist-p symbol))
	  (ido-goto-symbol symbol))
	 ((listp symbol)
	  (setq name (car symbol))
	  (setq position (cdr symbol)))
	 ((stringp symbol)
	  (setq name symbol)
	  (setq position
		(get-text-property 1 'org-imenu-marker symbol))))
	(unless (or (null position) (null name)
		    (string= (car imenu--rescan-item) name))
	  (add-to-list 'symbol-names name)
	  (add-to-list 'name-and-pos (cons name position))))))))

(defun enable-jump-to-defs ()
  (global-set-key "\C-ci" 'ido-goto-symbol))

(add-to-list 'package-requirements 'smex)
(defun optimise-smex ()
  ;; optimise smex
  (global-set-key [(meta x)]
		  (lambda ()
		    (interactive)
		    (or (boundp 'smex-cache)
			(smex-initialize))
		    (global-set-key [(meta x)] 'smex)
		    (smex)))
  (global-set-key [(shift meta x)]
		  (lambda ()
		    (interactive)
		    (or (boundp 'smex-cache)
			(smex-initialize))
		    (global-set-key [(shift meta x)] 'smex-major-mode-commands)
		    (smex-major-mode-commands))))

(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun enable-osx-copy-paste ()
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(defun osx-dired ()
  (setq dired-use-ls-dired nil))

(add-to-list 'package-requirements 'multiple-cursors)
(defun enable-multiple-cursors ()
  (require 'multiple-cursors)
  (global-set-key (kbd "C-c c") 'mc/edit-lines)
  (global-set-key (kbd "M-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c g") 'mc/mark-all-like-this))

(defun customise-editor ()
  ;; disable scrollbar and toolbar
  (setq scroll-bar-mode nil)
  (setq tool-bar-mode nil)

  ;; truncate long lines
  (setq-default truncate-lines t)

  ;; no backup files
  (setq make-backup-files nil)
  (setq auto-save-default nil)

  ;; set unicode as default
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

  ;; show reverse file paths in name buffer
  (require 'uniquify)
  (setq uniquify-buffer-name-style 'reverse)

  ;; no trailing whitespace
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (if (string-equal system-type "darwin")
      (progn
	(enable-osx-copy-paste)
	(osx-dired)))

  ;; window switching
  (global-set-key (kbd "S-<right>") 'next-multiframe-window)
  (global-set-key (kbd "S-<left>") 'previous-multiframe-window)

  (enable-multiple-cursors)

  (global-set-key (kbd "C-c t") 'find-name-dired)
  (global-set-key (kbd "C-c f") 'rgrep))

(add-to-list 'package-requirements 'sml-modeline)
(defun show-modeline ()
  ;; display modeline, showing your position in the file
  (sml-modeline-mode))

(add-to-list 'package-requirements 'expand-region)
(defun config-expand-region ()
  (require 'expand-region)
  ;; C-<space> usually expands region, but it's a common key binding, so this avoids clashes
  (global-set-key (kbd "C-@") 'er/expand-region))

(add-to-list 'package-requirements 'zenburn-theme)
(defun init-themes ()
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000")))
  (load-theme 'zenburn t)
  (message "for best results `export TERM=xterm-256color`"))

(add-to-list 'package-requirements 'company)
(defun init-completion ()
  (global-company-mode))

(add-to-list 'package-requirements 'which-key)
(defun init-which-key ()
  (require 'which-key)
  (which-key-mode))

(add-to-list 'package-requirements 'avy)
(defun init-avy ()
  (require 'avy)
  (global-set-key (kbd "M-'") 'avy-goto-char-timer)
  (setq avy-background t))

(add-to-list 'package-requirements 'projectile)
(defun init-projectile ()
  (projectile-mode))

(defun shiny-emacs ()
  (message "make emacs shiny... (⊃｡•́‿•̀｡)⊃━☆ﾟ.*･｡ﾟ")
  (enhance-ido)
  (enable-jump-to-defs)
  (init-completion)
  (optimise-smex)
  (customise-editor)
  (show-modeline)
  (init-themes)
  (init-which-key)
  (init-avy)
  (init-projectile))

(defun clj-setup-specs (arg)
  (interactive "P")
  (save-buffer)
  (cider-load-current-buffer)
  (cider-interactive-eval "(speclj.core/run-specs)")
  (when arg
    (cider-switch-to-relevant-repl-buffer nil)))

(defun clj-setup-testing ()
  (eval-after-load 'clojure-mode
    '(define-key clojure-mode-map (kbd "C-x t") 'clj-setup-specs))
  (setq cider-test-show-report-on-success t))

(defun clj-setup-refactoring ()
  (require 'clj-refactor)
  (clj-refactor-mode 1)
  (yas-minor-mode 1)
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(defun clj-refactor-hook ()
  (add-hook 'clojure-mode-hook #'clj-setup-refactoring))

(defun clj-fuzzy-completion ()
  (add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
  (add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion))

(defun clj-improved-indentation ()
  (require 'clojure-mode)
  (define-clojure-indent
    ;; Compojure
    (defroutes 'defun)
    (GET 2)
    (POST 2)
    (PUT 2)
    (DELETE 2)
    (HEAD 2)
    (ANY 2)
    (context 2)

    ;; Midje
    (facts 'defun)
    (fact 'defun)))

(add-to-list 'package-requirements 'paredit)
(add-to-list 'package-requirements 'rainbow-delimiters)
(add-to-list 'package-requirements 'auto-indent-mode)
(add-to-list 'package-requirements 'cider)
(add-to-list 'package-requirements 'clj-refactor)

(defun clj-setup ()
  (remove-hook 'clojure-mode-hook 'esk-pretty-fn)
  (add-hook 'cider-mode-hook #'eldoc-mode)
  (add-hook 'cider-repl-mode-hook #'eldoc-mode)

  (clj-fuzzy-completion)
  (clj-setup-testing)
  (clj-improved-indentation)
  (clj-refactor-hook)

  (eval-after-load "cider"
    #'(define-key cider-mode-map (kbd "C-c C-d") 'ac-nrepl-popup-doc))

  (eval-after-load "paredit"
    #'(define-key paredit-mode-map (kbd "M-?") 'paredit-convolute-sexp))

  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'clojure-mode-hook 'auto-indent-mode)
  (add-to-list 'auto-mode-alist '("\\.cljs\\'" . clojure-mode))
  (add-to-list 'auto-mode-alist '("\\.cljx\\'" . clojure-mode)))

(install-required-packages)
(add-hook 'after-init-hook #'print-elapsed-time)
(add-hook 'after-init-hook #'clj-setup)
(add-hook 'after-init-hook #'shiny-emacs)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(avy-background-face ((t (:background "#3F3F3F" :foreground "#555555" :inverse-video nil))))
 '(isearch ((t (:background "Pink" :foreground "#484848" :weight bold))))
 '(isearch-fail ((t (:background "brightred" :foreground "brightwhite"))))
 '(lazy-highlight ((t (:background "#5f5f5f" :foreground "white" :weight bold)))))
