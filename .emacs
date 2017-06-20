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

(defun customise-editor()
  ;; disable scrollbar and toolbar
  (setq scroll-bar-mode nil)
  (setq tool-bar-mode nil)
  ;; truncate long lines
  (setq-default truncate-lines t)

  ;; set unicode as default
  (prefer-coding-system       'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (setq default-buffer-file-coding-system 'utf-8)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

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
