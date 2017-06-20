(defvar started (float-time))
(defun print-elapsed-time()
  (message "initialised (%f secs)" (- (float-time) started)))

;; init required packages
(setq package-list '())

;; repos
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(package-initialize)

(defun install-required-packages()
  ;; update the package list
  (or (file-exists-p package-user-dir)
      (package-refresh-contents))

  ;; install the missing packages
  (dolist (package package-list)
    (unless (package-installed-p package)
      (package-install package))))

(add-to-list 'package-list 'idomenu)
(defun enhance-ido()
  ;; (i)nteractively (do) things
  (setq ido-enable-flex-matching t)
  (setq ido-everywhere t)
  (setq ido-ubiquitous-mode t))

(add-to-list 'package-list 'auto-complete)
(defun config-autocomplete()
  (require 'auto-complete)
  ;; disable bell sound
  (setq visible-bell t))

(add-to-list 'package-list 'smex)
(defun optimise-smex()
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

(defun enable-osx-copy-paste()
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

(add-to-list 'multiple-cursors)
(defun enable-multiple-cursors()
  (require 'multiple-cursors)
  (global-set-key (kbd "C-c c") 'mc/edit-lines)
  (global-set-key (kbd "M-n") 'mc/mark-next-like-this)
  (global-set-key (kbd "M-p") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c g") 'mc/mark-all-like-this))

(defun customise-editor()
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
      (enable-osx-copy-paste))

  ;; window switching
  (global-set-key (kbd "S-<right>") 'next-multiframe-window)
  (global-set-key (kbd "S-<left>") 'previous-multiframe-window)

  (enable-multiple-cursors)

  (global-set-key (kbd "C-c t") 'find-name-dired)
  (global-set-key (kbd "C-c f") 'rgrep)


  )

(add-to-list 'package-list 'sml-modeline)
(defun show-modeline()
  ;; display modeline, showing your position in the file
  (sml-modeline-mode))

(add-to-list 'package-list 'expand-region)
(defun config-expand-region()
  (require 'expand-region)
  ;; C-<space> usually expands region, but it's a common key binding, so this avoids clashes
  (global-set-key (kbd "C-@") 'er/expand-region))

(add-to-list 'package-list 'zenburn-theme)
(defun init-themes()
  (defvar zenburn-override-colors-alist
    '(("zenburn-bg" . "#000000")))
  (load-theme 'zenburn t)
  (message "for best results `export TERM=xterm-256color`"))

(defun shiny-emacs()
  (message "make emacs shiny... (⊃｡•́‿•̀｡)⊃━☆ﾟ.*･｡ﾟ")
  (enhance-ido)
  (config-autocomplete)
  (optimise-smex)
  (customise-editor)
  (show-modeline)
  (init-themes))

(add-to-list 'package-list 'cider)
(add-to-list 'package-list 'clj-refactor)

(install-required-packages)
(add-hook 'after-init-hook #'print-elapsed-time)
(add-hook 'after-init-hook #'shiny-emacs)
