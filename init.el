;;;;;;;; gigavinyl emacs config ;;;;;;;;

;;; Setting PATH
(setenv "PATH"
  (concat
   (concat (getenv "HOME") "/.nix-profile/bin:")
   (getenv "PATH")
  )
)
(setenv "NIX_PATH" "nixpkgs=/home/gigavinyl/.nix-defexpr/channels/nixpkgs")
(add-to-list 'exec-path (concat (getenv "HOME") "/.nix-profile/bin"))


;;; Making the interface not terrible ;;;
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)


;;; Sane defaults ;;;
(setq delete-old-versions -1 )		; delete excess backup versions silently
(setq version-control t )		; use version control
(setq vc-make-backup-files t )		; make backups file even when in version controlled dir
(setq backup-directory-alist `(("." . "~/.emacs.d/backups")) ) ; which directory to put backups file
(setq vc-follow-symlinks t )				       ; don't ask for confirmation when opening symlinked file
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)) ) ;transform backups file name
(setq inhibit-startup-screen t )	; inhibit useless and old-school startup screen
(setq ring-bell-function 'ignore )	; silent bell when you make a mistake
(setq coding-system-for-read 'utf-8 )	; use utf-8 by default
(setq coding-system-for-write 'utf-8 )
(setq sentence-end-double-space nil)	; sentence SHOULD end with only a point.
(setq default-fill-column 80)		; toggle wrapping text at the 80th character
(setq-default indent-tabs-mode nil)     ; tabs to spaces
;; put fortune in scratch buffer
(setq initial-scratch-message
      (format
       ";; %s\n\n"
       (replace-regexp-in-string
        "\n" "\n;; " ; comment each line
        (replace-regexp-in-string
         "\n$" ""    ; remove trailing linebreak
         (shell-command-to-string "fortune -o | cowsay")))))


;;; Use package ;;;
(setq package-enable-at-startup nil) ; tells emacs not to load any packages before starting up
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "http://orgmode.org/elpa/")
                         ("gnu"       . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))
(package-initialize) ; guess what this one does ?

; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; updage packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package) ; guess what this one does too ?


;;; Evil!!

; Evil-leader
(use-package evil-leader
  :ensure t
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>"))

; Undo-tree
(use-package undo-tree
  :ensure t)

; Evil-mode
(use-package evil
  :ensure t
  :config
  (evil-mode 1)
  (setq evil-mode-line-format '(before . mode-line-front-space)))

; Nerd-commenter
(use-package evil-nerd-commenter
  :ensure t)
(evil-leader/set-key
  "ci" 'evilnc-comment-or-uncomment-lines
  "cl" 'evilnc-quick-comment-or-uncomment-to-the-line
  "ll" 'evilnc-quick-comment-or-uncomment-to-the-line
  "cc" 'evilnc-copy-and-comment-lines
  "cp" 'evilnc-comment-or-uncomment-paragraphs
  "cr" 'comment-or-uncomment-region
  "cv" 'evilnc-toggle-invert-comment-line-by-line
  "."  'evilnc-copy-and-comment-operator
  "\\" 'evilnc-comment-operator ; if you prefer backslash key
)

; Evil-Escape
(use-package evil-escape
  :ensure t
  :config
  (setq-default evil-escape-key-sequence "jk")
  (evil-escape-mode))

; Evil-easymotion
(use-package evil-easymotion
  :ensure t
  :config
  (evilem-default-keybindings "SPC"))

; Evil-snipe
(use-package evil-snipe
  :ensure t
  :config
  (evil-snipe-mode 1)
  (setq evil-snipe-scope 'buffer)
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode)
  (define-key evil-snipe-parent-transient-map (kbd "C-;")
    (evilem-create 'evil-snipe-repeat
                   :bind ((evil-snipe-scope 'buffer)
                          (evil-snipe-enable-highlight)
                          (evil-snipe-enable-incremental-highlight)))))


;;; Interface Plugins

; Git-gutter-fringe
(use-package fringe-helper
  :ensure t)
(use-package git-gutter-fringe
  :ensure t)

; Neotree
(use-package neotree
  :ensure t
  :commands (neotree-toggle)
  :config
  (setq projectile-switch-project-action 'neotree-projectile-action)
  (add-hook 'neotree-mode-hook
    (lambda ()
      (define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
      (define-key evil-normal-state-local-map (kbd "I") 'neotree-hidden-file-toggle)
      (define-key evil-normal-state-local-map (kbd "z") 'neotree-stretch-toggle)
      (define-key evil-normal-state-local-map (kbd "R") 'neotree-refresh)
      (define-key evil-normal-state-local-map (kbd "m") 'neotree-rename-node)
      (define-key evil-normal-state-local-map (kbd "c") 'neotree-create-node)
      (define-key evil-normal-state-local-map (kbd "d") 'neotree-delete-node)

      (define-key evil-normal-state-local-map (kbd "s") 'neotree-enter-vertical-split)
      (define-key evil-normal-state-local-map (kbd "S") 'neotree-enter-horizontal-split)

      (define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter))))
(evil-leader/set-key "n"  'neotree-toggle)

; Popup
(use-package popup
  :ensure t)


;;; Misc Utils

; Magit
(use-package magit
  :ensure t)
(use-package evil-magit
  :ensure t
  :config
  (setq evil-magit-use-y-for-yank t))

;Editorconfig
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

; Smex
(use-package smex
  :ensure t
  :config
  (evil-leader/set-key "<SPC>" 'smex))

; Flyspell
(use-package flyspell-popup
  :ensure t
  :config
  (evil-leader/set-key "fc" 'flyspell-popup-correct)
  (setq ispell-list-command "--list")
  (dolist (hook '(org-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (dolist (hook '(markdown-mode-hook))
    (add-hook hook (lambda () (flyspell-mode 1))))
  (add-hook 'haskell-mode-hook
    (lambda ()
      (flyspell-prog-mode))))

; Smartparens
(use-package smartparens
  :ensure t
  :config
  (smartparens-strict-mode 1)
  (require 'smartparens-config)
  (add-hook 'emacs-lisp-mode-hook #'smartparens-mode)
  (add-hook 'haskell-mode-hook #'smartparens-mode)
  (add-hook 'latex-lisp-mode-hook #'smartparens-mode)
  (add-hook 'org-mode-hook #'smartparens-mode)
  (add-hook 'racket-lisp-mode-hook #'smartparens-mode)
  (add-hook 'web-lisp-mode-hook #'smartparens-mode))
(use-package evil-smartparens
  :ensure t
  :config
  (add-hook 'smartparens-enabled-hook #'evil-smartparens-mode))

;;; Misc bindings

; Exiting and saving
(evil-leader/set-key
  "q" 'save-buffers-kill-terminal
  "fs" 'save-buffer
)

; Open init file
(defun open-init-file ()
  "Open the init file"
  (interactive)
  (find-file user-init-file)
)
(evil-leader/set-key "ev" 'open-init-file)

; Align Regexp
(evil-leader/set-key "a" 'align-regexp)

; Window management
(evil-leader/set-key
  "wh" 'windmove-left
  "wl" 'windmove-right
  "wk" 'windmove-up
  "wj" 'windmove-down
  "v"  'split-window-horizontally
  "hs" 'split-window-vertically
  "wd" 'delete-window)

; Buffer management
(evil-leader/set-key
  "bd" 'kill-buffer
  "bn" 'next-buffer
  "bp" 'previous-buffer)
  


;;; Making it pretty

; Font
(set-default-font "GohuFont-12")

; Theme
(use-package all-the-icons
  :ensure t)
(use-package doom-themes
  :ensure t
  :config
  (doom-themes-neotree-config)
  (load-theme 'doom-vibrant t)
  (setq doom-neotree-file-icons t)
  (setq doom-neotree-project-size 1)
  (setq doom-neotree-folder-size 0.95))

; Line numbers
(use-package nlinum-relative
  :ensure t
  :config
  (nlinum-relative-setup-evil)
  (add-hook 'prog-mode-hook 'nlinum-relative-mode)
  (doom-themes-nlinum-config)
  (setq nlinum-relative-redisplay-delay 0)
  (setq nlinum-relative-current-symbol "")
  (setq nlinum-relative-offset 0))


;;; Programming Languages

; Haskell
(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :interpreter "haskell")
(use-package intero
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

; Org-mode
(use-package org
  :ensure t
  :mode (("\\.org$'" . org-mode))
  :interpreter "org-mode"
  :config
  (evil-leader/set-key-for-mode 'org-mode "cb" 'org-cycle-list-bullet))
(use-package evil-org
  :ensure t)

; Web-mode
(use-package web-mode
  :mode (("\\.html\\'"       . web-mode)
         ("\\.html\\.erb\\'" . web-mode)
         ("\\.mustache\\'"   . web-mode)
         ("\\.tpl\\'"        . web-mode)
         ("\\.jinja\\'"      . web-mode))
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (progn
    (setq web-mode-engines-alist
          '(("\\.jinja\\'"  . "django")))))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(nlinum-relative-current-face ((t (:inherit linum :background "#22252c" :foreground "#D4D4D4" :weight bold)))))
