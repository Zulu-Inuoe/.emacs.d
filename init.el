;;-*-coding: utf-8-unix; lexical-binding: t-*-

(cd (expand-file-name "~/"))

(require 'cl)
(require 'subr-x)

;; Turn on debugging so I can fix any init breakage. Turns off at the end
;; But only turn on for graphic displays. Otherwise a daemon will be stuck
(setq debug-on-error (display-graphic-p))

;; Excessive, but what else can I do?
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)
(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING))

(defun my/add-load-if-exists (path)
  (let ((dpath (file-name-as-directory path)))
    (when (file-exists-p dpath)
      (add-to-list 'load-path path))))

(defun my/scoop-ensure (command &optional package)
  (setf package (or package command))
  (unless (executable-find command)
    (if-let ((scoop (executable-find "scoop")))
        (call-process scoop nil nil nil "install" package)
      (warn "Can't find '%s', and 'scoop' is unavailable" package))))

;; keep customize settings in their own file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(load custom-file t)

(require 'recentf)

;; Backup and auto-save settings
(let* ((emacs-tmp-dir
        (file-name-as-directory
         (expand-file-name "emacs" temporary-file-directory)))
       (backups-dir
        (file-name-as-directory
         (expand-file-name "backups" emacs-tmp-dir)))
       (auto-saves-dir
        (file-name-as-directory
         (expand-file-name "auto-saves" emacs-tmp-dir)))
       (recentf-file
        (expand-file-name "recentf" emacs-tmp-dir)))
  (setq-default
   version-control t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 8
   create-lockfiles nil
   backup-by-copying t
   backup-directory-alist `((".*" . ,backups-dir))
   auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
   auto-save-list-file-prefix auto-saves-dir
   recentf-auto-cleanup 'never
   recentf-save-file recentf-file
   ;; Make backup files even in vc'd directories
   vc-make-backup-files t))

(global-auto-revert-mode +1)
(recentf-mode +1)
(save-place-mode +1)
(delete-selection-mode +1)
(window-divider-mode +1)
(size-indication-mode +1)

;; Minor visual/input stuff
(setq-default
 inhibit-startup-screen t
 initial-major-mode 'fundamental-mode
 make-pointer-invisible nil
 echo-keystrokes 0.1
 frame-resize-pixelwise t
 frame-inhibit-implied-resize t
 read-file-name-completion-ignore-case t
 find-file-visit-truename nil
 mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))
 cursor-in-non-selected-windows nil
 scroll-conservatively 101
 scroll-step 1)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode +1)
(global-hl-line-mode +1)

;; Misc

;; Basic editing settings
(setq-default
 indent-tabs-mode nil
 indicate-empty-lines t
 mode-require-final-newline nil
 mouse-yank-at-point t
 show-trailing-whitespace t)

(when (eq system-type 'windows-nt)
  ;; Disable beeps on win32 and associate cp65001 with utf-8
  (set-message-beep 'silent)
  (define-coding-system-alias 'cp65001 'utf-8))

;; Replace "yes or no" with y or n
(defalias 'yes-or-no-p 'y-or-n-p)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'list-timers 'disabled nil)

;; Frame title bar formatting to show full path of file
(setq-default
 frame-title-format
 (list '((buffer-file-name " %f"
                           (dired-directory
                            dired-directory
                            (revert-buffer-function
                             " %b"
                             ("%b - Dir:  "
                              default-directory)))))))

(setq-default
 icon-title-format
 (list '((buffer-file-name " %f" (dired-directory
                                  dired-directory
                                  (revert-buffer-function " %b" ("%b - Dir:  " default-directory)))))))

;;;; Set up package management info

;; initialize all ELPA packages
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

(let ((lisp-dir (expand-file-name "lisp/" user-emacs-directory)))
  (when (file-exists-p lisp-dir)
    (cl-dolist (dir (directory-files lisp-dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)"))
      (let ((dirname (file-name-as-directory dir)))
        (when (file-exists-p dirname)
          (add-to-list 'load-path dirname))))))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(unless (package-installed-p 'diminish)
  (package-install 'diminish))

(unless (package-installed-p 'bind-key)
  (package-install 'bind-key))

;; Load use-package, used for loading packages everywhere else
(eval-when-compile
  (require 'use-package))

(use-package diminish)
(use-package bind-key)

;; Set to t to debug package loading
(setq use-package-verbose nil)

;; Theme
(setq-default custom-safe-themes t)

(define-advice load-theme (:before (theme &optional no-confirm no-enable)
                                   my/before-load-theme)
  (mapc #'disable-theme custom-enabled-themes))

(unless (or (package-installed-p 'doom-themes) (package-installed-p 'zenburn-theme))
  (package-install 'doom-themes))

(defun my/after-init-load-themes ()
  (cond
   ((package-installed-p 'doom-themes)
    (setq doom-themes-enable-bold t
          doom-themes-enable-italic t)
    (load-theme 'doom-one t)
    (doom-themes-visual-bell-config)
    (cond
     ((package-installed-p 'treemacs)
      ;; (doom-themes-treemacs-config)
      )
     ((package-installed-p 'neotree)
      (doom-themes-neotree-config))))
   ((package-installed-p 'zenburn-theme)
    (load-theme 'zenburn t)))

  ;; Doom will for whatever reason reset the buffer coding system..
  (setq-default buffer-file-coding-system 'utf-8-unix))

(add-hook 'after-init-hook 'my/after-init-load-themes)

(load (expand-file-name "local.el"
                        user-emacs-directory)
      t nil t t)

;; Visuals & frills

(use-package all-the-icons
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    (defun my/install-fonts ()
      (call-process "powershell"
                    nil nil nil
                    "-NoProfile"
                    "-ExecutionPolicy" "Bypass"
                    "-File" (expand-file-name "install-fonts.ps1" user-emacs-directory)))
    (defun my/uninstall-fonts ()
      (call-process "powershell"
                    nil nil nil
                    "-NoProfile"
                    "-ExecutionPolicy" "Bypass"
                    "-File"  (expand-file-name "uninstall-fonts.ps1" user-emacs-directory)))

    (my/install-fonts)
    (add-hook 'kill-emacs-hook 'my/uninstall-fonts)))

(use-package desktop
  :ensure t
  :defer nil
  :config
  (setq desktop-dirname user-emacs-directory)
  (desktop-save-mode +1))

(when (>= emacs-major-version 25)
  (use-package doom-modeline
    :custom
    (doom-modeline-icon t)
    (doom-modeline-major-mode-color-icon t)
    :ensure t
    :hook (after-init . doom-modeline-mode)))

(use-package elcord
  :ensure t
  :custom (elcord-use-major-mode-as-main-icon t)
  :hook (after-init . elcord-mode)
  :hook (elcord-mode . my/elcord-mode-hook)
  :config
  (add-to-list 'elcord-mode-icon-alist '(bat-mode . "bat-mode_icon"))
  (add-to-list 'elcord-mode-icon-alist '(cypher-mode . "cypher-mode_icon"))
  (add-to-list 'elcord-mode-icon-alist '(powershell-mode . "powershell-mode_icon"))
  (add-to-list 'elcord-mode-text-alist '(powershell-mode . "Powershell"))
  (add-to-list 'elcord-mode-icon-alist '(rjsx-mode . "react-mode_icon"))

  (defvar my/lisp-impl-icon-alist
    '(("armedbear" . "abcl-mode_icon")
      ("ccl" . "ccl-mode_icon")
      ("clisp" . "clisp-mode_icon")
      ("ECL" . "ecl-mode_icon")
      ("lispworks" . "lispworks-mode_icon")
      ("sbcl" . "sbcl-mode_icon")))

  (defun my/choose-cl-impl (mode)
    (let ((impl-name
           (and (fboundp 'sly)
                (sly-connected-p)
                (sly-lisp-implementation-name))))
      (cdr (assoc impl-name  my/lisp-impl-icon-alist))))

  (when (seq-some (lambda (elt) (stringp (car elt))) elcord-mode-icon-alist)
    (add-to-list 'elcord-mode-icon-alist '(lisp-mode . my/choose-cl-impl))
    (add-to-list 'elcord-mode-icon-alist '("^sly-.*$" . my/choose-cl-impl)))

  (defun elcord--disable-elcord-if-no-frames (f)
    (when (let ((frames (delete f (visible-frame-list))))
            (or (null frames)
                (and (null (cdr frames))
                     (eq (car frames) terminal-frame))))
      (elcord-mode -1)
      (add-hook 'after-make-frame-functions 'elcord--enable-on-frame-created)))

  (defun elcord--enable-on-frame-created (f)
    (remove-hook 'after-make-frame-functions 'elcord--enable-on-frame-created)
    (elcord-mode +1))

  (defun my/elcord-mode-hook ()
    (if elcord-mode
        (add-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames)
      (remove-hook 'delete-frame-functions 'elcord--disable-elcord-if-no-frames))))

(use-package nyan-mode
  :ensure t
  :custom
  (nyan-animate-nyancat t)
  (nyan-bar-length 10)
  (nyan-wavy-trail t)
  (nyan-animation-frame-interval 0.5)
  :hook (after-init . nyan-mode))

(use-package paren
  :hook ((lisp-mode emacs-lisp-mode) . show-paren-mode))

(use-package projectile
  :ensure t
  :config
  (projectile-mode +1))

(use-package psession
  :ensure t
  :config
  (psession-mode +1)
  (psession-savehist-mode +1)
  (psession-autosave-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :hook (lisp-mode . rainbow-delimiters-mode-enable)
  :hook (emacs-lisp-mode . rainbow-delimiters-mode-enable))

(use-package solaire-mode
  :ensure t
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package whitespace
  :diminish global-whitespace-mode
  :custom
  (whitespace-style
   '(face trailing tabs spaces newline empty indentation space-after-tab
          space-before-tab space-mark tab-mark newline-mark))
  :config (global-whitespace-mode +1))

;; Navitation & Utils
(use-package avy
  :ensure t
  :bind (("M-g c" . avy-goto-char)
         ("M-g M-g" . avy-goto-line)))

(use-package ace-window
  :ensure t
  :bind (("C-x o" . ace-window)))

(when (>= emacs-major-version 25)
  (use-package back-button
    :ensure t
    :diminish back-button-mode
    :bind (("<mouse-4>" . back-button-global-backward)
           ("<mouse-5>" . back-button-global-forward))))

(use-package which-key
  :ensure t
  :defer nil
  :diminish which-key-mode
  :config (which-key-mode +1))

(use-package window-purpose
  :ensure t
  :custom (purpose-x-popwin-position 'right)
  :config
  (purpose-mode +1)
  (require 'window-purpose-x)
  (purpose-x-popwin-setup)
  (purpose-x-kill-setup)
  (purpose-x-magit-multi-on))

(use-package windsize
  :ensure t
  :bind (("C-S-<up>" . windsize-up)
         ("C-S-<down>" . windsize-down)
         ("C-S-<left>" . windsize-left)
         ("C-S-<right>" . windsize-right)))

(use-package winner
  :ensure t
  :config
  (winner-mode +1))

;;;completion & input

(use-package company
  :ensure t
  :diminish company-mode
  :defer nil
  :bind
  (:map company-mode-map
        ("<tab>" . company-indent-or-complete-common)
        :map company-active-map
        ("<tab>" . company-complete-selection)
        ("<prior>" . company-previous-page)
        ("<next>" . company-next-page)
        ("<down>" . company-select-next)
        ("<up>" . company-select-previous)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("M-." . company-show-location)
        ("SPC" . my/company-abort-and-insert))
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0)
  (company-require-match nil)
  :config
  (defun my/company-abort-and-insert ()
    (interactive)
    (company-abort)
    (self-insert-command 1)))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook ((lisp-interaction-mode emacs-lisp-mode) . elisp-slime-nav-mode))

(use-package helm
  :ensure t
  :defer nil
  :diminish helm-mode
  :bind (("M-x" . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-y" . helm-show-kill-ring)
         ("C-h SPC" . helm-all-mark-rings)
         :map helm-map
         ("<tab>" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-find-files-map
         ("C-s" . helm-ff-run-grep-ag)
         :map helm-buffer-map
         ("<tab>" . helm-next-source)
         ("S-<tab>" . helm-previous-source)
         :map helm-generic-files-map
         ("<tab>" . helm-next-source)
         ("S-<tab>" . helm-previous-source))
  :custom
  (helm-boring-buffer-regexp-list
   '("\\*Buffer List" "\\*sly-events" "\\*Help" "\\*sly-inferior-lisp" "\\` " "\\`\\*helm" "\\`\\*Echo Area" "\\`\\*Minibuf"))
  (helm-buffers-fuzzy-matching t)
  (helm-ff-file-name-history-use-recentf t)
  (helm-ff-skip-boring-files t)
  (helm-grep-ag-command
   "rg --color=always --smart-case --no-heading --line-number %s %s %s")
  (helm-move-to-line-cycle-in-source t)
  (helm-recentf-fuzzy-match t)
  :config
  (require 'helm-files)
  (require 'helm-config)
  (helm-mode +1))

(use-package helm-company
  :after (helm company)
  :ensure t
  :defer nil
  :bind (:map company-mode-map
              ("C-S-SPC" . helm-company)
              :map company-active-map
              ("C-S-SPC" . helm-company)))

(use-package helm-posframe
  :after (helm)
  :ensure t
  :config
  (helm-posframe-enable)

  (defun my/posframe-poshandler-frame-top-center (info)
    "Like `posframe-poshandler-frame-top-center', but accounts for x-pixel-offset and y-pixel-offset"
    (cons (+ (/ (- (plist-get info :parent-frame-width)
                   (plist-get info :posframe-width))
                2)
             (plist-get info :x-pixel-offset))
          (plist-get info :y-pixel-offset)))

  (define-advice helm-posframe-display (:around (orig-function buffer &optional _resume)
                                                my/around-helm-posframe-display)
    (setq helm-posframe-buffer buffer)
    (posframe-show
     buffer
     :position (point)
     :poshandler 'my/posframe-poshandler-frame-top-center
     :max-width (- (frame-width) 5)
     :max-height (- (frame-height) 5)
     :min-width (truncate (frame-width) 1.5)
     :min-height (truncate (frame-height) 1.5)
     :y-pixel-offset 15
     :font helm-posframe-font
     :override-parameters '((internal-border-width . 5)
                            (border-width . 10))
     :respect-header-line t)))

(use-package helm-projectile
  :after helm projectile
  :ensure t
  :config
  (helm-projectile-on))

(use-package helm-purpose
  :after (helm window-purpose)
  :ensure t
  :bind (("M-x" . helm-M-x)
         :map purpose-mode-map
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files))
  :config
  (helm-purpose-setup))

(use-package helm-sly
  :after (helm sly)
  :ensure t
  :config
  (global-helm-sly-mode +1))

(use-package helm-swoop
  :after (helm)
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         :map helm-swoop-map
         ("C-s" . helm-next-line)
         ("C-r" . helm-previous-line)))

;; Misc tooling
(use-package ediff
  :after (winner)
  :ensure t
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package macrostep
  :after (elisp-mode)
  :bind (:map emacs-lisp-mode-map
              ("C-c M-e" . macrostep-expand)))

(when (and (executable-find "git")
           (>= emacs-major-version 25))
  (use-package ssh-agency
    :ensure t)

  (use-package magit
    :ensure t
    :bind ("C-x g" . magit-status)
    :defer t
    :custom (magit-set-upstream-on-push 'dontask)
    :init
    ;;Tell's git to use the TK GUI to ask for password
    ;;This is a workaround to a problem with git when pushing
    ;;over https
    (setenv "GIT_ASKPASS" "git-gui--askpass")
    (setenv "SSH_ASKPASS" "git-gui--askpass")))

(use-package rg
  :ensure t
  :init
  (when (eq system-type 'windows-nt)
    (my/scoop-ensure "rg" "ripgrep"))
  :custom
  (rg-group-result nil))

(use-package slack
  :commands (start-slack)
  :custom
  (slack-buffer-enable-emojify t)
  (slack-prefer-current-team t))

(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :bind
  (:map global-map
        ("<f8>"       . treemacs-select-window)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-all-the-icons
  :ensure t
  :after (treemacs)
  :init
  (treemacs-load-theme "all-the-icons"))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

;;;editing

(use-package clution
  :if (require 'clution nil t)
  :custom
  (clution-run-style 'term))

(use-package dotenv-mode
  :ensure t)

;;; languages & editing

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc$"
  :mode "\\.ad$"
  :mode "\\.asciidoc$")

(use-package antlr-mode
  :ensure t
  :mode "\\.g4$")

(use-package cc-mode
  :custom
  (c-default-style
   '((c-mode . "stroustrup")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk"))))

(use-package conf-mode
  :hook (conf-mode . my/conf-set-tab-width)
  :init
  (defun my/conf-set-tab-width ()
    (setq tab-width 2)))

(use-package cperl-mode
  :ensure t
  :mode "\\.pl$"
  :mode "\\.perl$"
  :custom
  (cperl-close-paren-offset -4)
  (cperl-continued-statement-offset 4)
  (cperl-indent-level 4)
  (cperl-indent-parens-as-block t))

(use-package csharp-mode
  :ensure t
  :mode "\\.cs$")

(use-package cypher-mode
  :ensure t)

(use-package doc-view
  :ensure t
  :bind (:map doc-view-mode-map
              ("M-v" . backward-page)
              ("C-v" . forward-page))
  :custom
  (doc-view-ghostscript-program "gswin64c"))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode +1))

(use-package editorconfig-charset-extras
  :ensure t)

(use-package elisp-mode
  :defer nil
  :bind (:map emacs-lisp-mode-map
              ("C-x C-e" . my/eval-dwim)
              ("C-c C-l" . my/load-file-dwim))
  :config
  (defun my/eval-dwim (prefix)
    "eval region from beg to end if active, otherwise the last sexp."
    (interactive "P")
    (if (use-region-p)
        (eval-region (region-beginning) (region-end))
      (eval-last-sexp prefix)))
  (defun my/load-file-dwim (filename)
    (interactive (list (let ((completion-ignored-extensions
                              (remove module-file-suffix
                                      (remove ".elc"
                                              completion-ignored-extensions))))
                         (read-file-name "Load file: " nil nil 'lambda (buffer-file-name)))))
    (load-file filename)))

(use-package eldoc
  :diminish eldoc-mode
  :hook (emacs-lisp-mode . turn-on-eldoc-mode)
  :custom (eldoc-echo-area-use-multiline-p t))

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  (when (package-installed-p 'flycheck)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode)))

(use-package flycheck
  :ensure t
  :bind (("M-p" . flycheck-previous-error)
         ("M-n" . flycheck-next-error)))

(use-package flycheck-package
  :ensure t)

(when (>= emacs-major-version 25)
  (use-package ggtags
    :ensure t
    :commands ggtags-mode
    :diminish ggtags-mode
    :bind (:map ggtags-mode-map
                ("C-c g s" . ggtags-find-other-symbol)
                ("C-c g h" . ggtags-view-tag-history)
                ("C-c g r" . ggtags-find-reference)
                ("C-c g f" . ggtags-find-file)
                ("C-c g c" . ggtags-create-tags)
                ("C-c g u" . ggtags-update-tags))
    :hook (c-mode-common . my/enable-ggtags-mode-in-c-like)
    :init
    (defun my/enable-ggtags-mode-in-c-like ()
      (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
        (ggtags-mode +1)))))

(use-package gitattributes-mode
  :ensure t
  :mode "^\\.gitattributes$")

(use-package gitignore-mode
  :ensure t
  :mode "^\\.gitignore$")

(use-package gitconfig-mode
  :ensure t
  :mode "^\\.gitconfig$")

(use-package help-mode
  :config
  (defun my/on-help-mode ()
    (setq truncate-lines nil
          word-wrap t))
  (add-hook 'help-mode-hook 'my/on-help-mode))

(use-package hexl
  :mode ("\\.exe$" . hexl-mode)
  :mode ("\\.dll$" . hexl-mode))

(use-package impatient-mode
  :ensure t)

(use-package js
  :custom (js-indent-line 2))

(use-package json-mode
  :custom (js-indent-level 2)
  :ensure t)

(defun my/comment-region-if-mark ()
  (interactive)
  (if (use-region-p)
      (comment-region (min (point) (mark)) (max (point) (mark)))
    (self-insert-command 1)))

(use-package js2-mode
  :ensure t
  :bind (:map js-mode-map
              ("/" . my/comment-region-if-mark))
  :mode "\\.js$"
  :interpreter "node"
  :hook (js2-mode . js2-imenu-extras-mode)
  :custom (js2-basic-offset 2))

(use-package js2-refactor
  :ensure t
  :hook (js2-mode . js2-refactor-mode)
  :bind (:map js2-mode-map
              ("C-k" . js2r-kill))
  :config
  (js2r-add-keybindings-with-prefix "C-c C-r"))

(use-package lisp-mode
  :mode "\\.eclrc$"
  :mode "\\.ros$"
  :mode "\\.sbclrc$"
  :mode "\\.slynkrc$"
  :mode "\\.lispworks$")

(use-package paredit
  :ensure t
  :diminish paredit-mode
  :hook ((lisp-mode emacs-lisp-mode) . paredit-mode)
  :config
  (defun my/paredit-delete-region-or-forward ()
    (interactive)
    (if (use-region-p)
        (paredit-delete-region (region-beginning) (region-end))
      (paredit-forward-delete)))

  (defun my/paredit-delete-region-or-backward ()
    (interactive)
    (if (use-region-p)
        (paredit-delete-region (region-beginning) (region-end))
      (paredit-backward-delete)))

  (defun my/paredit-kill-region-or-kill ()
    (interactive)
    (if (use-region-p)
        (paredit-kill-region (region-beginning) (region-end))
      (paredit-kill)))

  (defun my/paredit-comment-sexp ()
    "Comment out the sexp at point."
    (interactive)
    (save-excursion
      (mark-sexp)
      (paredit-comment-dwim)))

  (defun my/paredit-comment-line-or-sexp ()
    (interactive)
    (cond
     ((use-region-p)
      (save-excursion
        (mark-sexp 1 t)
        (paredit-comment-dwim)))
     ((or (paredit-in-string-p)
          (paredit-in-comment-p)
          (paredit-in-char-p)
          (looking-at "[[:space:]]*$")
          (looking-at "[[:space:]]*;+.*$"))
      (paredit-semicolon))
     ((looking-at "[[:space:]]*(.*$")
      (my/paredit-comment-sexp))
     (t
      (paredit-semicolon))))

  (when (and (package-installed-p 'eldoc)
             (symbol-function 'eldoc-add-command))
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round))

  (defun my/parse-dispatch-macro-backwards ()
    (let ((old-point (point)))
      (if (save-match-data
            (and (not (member (char-before) '(?\( ?#)))
                 (re-search-backward "#[0-9]*" nil t)
                 (= (match-end 0) (- old-point 1))))
          (point)
        (goto-char old-point)
        nil)))

  (defun my/space-for-dispatch-macro (endp delim)
    (save-excursion
      (or endp
          (not (my/parse-dispatch-macro-backwards)))))

  (defun my/parse-dispatch-macro ()
    (forward-comment (buffer-size))
    (save-match-data
      (when (looking-at "#[0-9]*[^(#0-9]")
        (goto-char (match-end 0)))))

  (defun my/cl-dispatch-macro-forward-sexp (n)
    (cond
     ((> n 0)
      (dotimes (i n)
        (my/parse-dispatch-macro)
        (let ((forward-sexp-function nil))
          (forward-sexp))))
     ((< n 0)
      (dotimes (i (- n))
        (let ((forward-sexp-function nil))
          (forward-sexp -1))
        (let ((old-point (point)))
          (forward-comment (- (buffer-size)))
          (unless (my/parse-dispatch-macro-backwards)
            (goto-char old-point)))))))

  (defun my/cl-mode-hook ()
    (setq-local forward-sexp-function 'my/cl-dispatch-macro-forward-sexp)
    (make-local-variable 'paredit-space-for-delimiter-predicates)
    (push 'my/space-for-dispatch-macro paredit-space-for-delimiter-predicates))

  (add-hook 'lisp-mode-hook 'my/cl-mode-hook)
  (define-key paredit-mode-map [remap paredit-semicolon] 'my/paredit-comment-line-or-sexp)
  (define-key paredit-mode-map [remap paredit-forward-delete] 'my/paredit-delete-region-or-forward)
  (define-key paredit-mode-map [remap paredit-backward-delete] 'my/paredit-delete-region-or-backward)
  (define-key paredit-mode-map [remap paredit-kill] 'my/paredit-kill-region-or-kill))

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :custom (lua-indent-level 4))

(use-package markdown-mode
  :ensure t
  :mode "\\.text$"
  :mode "\\.markdown$"
  :mode "\\.md$"
  :mode ("README\\.md$" . gfm-mode)
  :custom (markdown-command "pandoc"))

(use-package nxml-mode
  :mode "\\.proj$"
  :mode "\\.lyr$"
  :mode "\\.mtl$"
  :mode "\\.xaml$"
  :custom
  (nxml-attribute-indent 2)
  (nxml-slash-auto-complete-flag t))

(use-package omnisharp
  :ensure t
  :after company
  :init
  (when (eq system-type 'windows-nt)
    (my/scoop-ensure "pandoc"))
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(use-package prog-mode
  :hook ((lisp-mode emacs-lisp-mode) . prettify-symbols-mode)
  :config
  (defun my/on-prog-mode ()
    (setf truncate-lines t
          word-wrap nil))
  (add-hook 'prog-mode-hook 'my/on-prog-mode))

(use-package powershell
  :ensure t
  :mode ("\\.ps1$" . powershell-mode))

(use-package restclient
  :ensure t
  :after json-mode
  :mode (("\\.http$" . restclient-mode))
  :bind (:map restclient-mode-map
              ("C-c C-f" . json-mode-beautify)))

(use-package slime
  :if (package-installed-p 'slime)
  :custom (slime-contribs '(slime-fancy)))

(use-package sly
  :ensure t
  ;; :if (package-installed-p 'sly)
  :defer nil
  :bind (:map sly-mode-map
              ("C-x C-e" . my/sly-eval-dwim)
              ;; ([remap sly-eval-last-expression] . my/sly-eval-in-mrepl)
              )
  :custom
  (inferior-lisp-program "sbcl")
  (sly-command-switch-to-existing-lisp 'always)
  (sly-ignore-protocol-mismatches t)
  (sly-kill-without-query-p t)
  (sly-mrepl-history-file-name (expand-file-name ".sly-mrepl-history" user-emacs-directory))
  (sly-net-coding-system 'utf-8-unix)
  :config
  (define-advice sly-quit-lisp (:around (orig-function &optional kill interactive)
                                        my/sly-quit-lisp-no-ask-if-single)
    (if (null (cdr sly-net-processes))
        (sly-quit-lisp-internal (sly-current-connection) 'sly-quit-sentinel kill)
      (funcall orig-function kill interactive)))

  (defun my/kill-sly-buffers ()
    (interactive)
    (dolist (buffer (buffer-list))
      (when (or (eql (string-match "\\*sly" (buffer-name buffer)) 0)
                (eql (string-match " \\*sly" (buffer-name buffer)) 0))
        (kill-buffer buffer)))
    t)

  (defun my/kill-sly-buffers-on-close (process)
    (my/kill-sly-buffers))

  (defun my/sly-eval-in-mrepl ()
    (interactive)
    (let ((sexp (sly-last-expression)))
      (with-current-buffer (sly-mrepl--find-buffer)
        (goto-char (sly-mrepl--mark))
        (let ((prev-input (buffer-substring (point) (point-max))))
          (delete-region (point) (point-max))
          (insert sexp)
          (goto-char (point-max))
          (sly-mrepl-return)
          (insert prev-input)))))

  (defun my/sly-eval-dwim ()
    (interactive)
    (if (use-region-p)
        (sly-eval-region (region-beginning) (region-end))
      (sly-eval-last-expression)))

  (add-hook 'sly-net-process-close-hooks 'my/kill-sly-buffers-on-close)
  (add-hook 'sly-compilation-finished-hook 'sly-show-compilation-log t)

  (defun my/set-mrepl-bindings ()
    (define-key sly-mrepl-mode-map (kbd "C-c C-l") 'sly-load-file)
    (define-key sly-mrepl-mode-map (kbd "C-c I") 'sly-inspect)
    (define-key sly-mrepl-mode-map (kbd "C-x C-e") 'my/sly-eval-dwim))
  (add-hook 'sly-mrepl-mode-hook 'my/set-mrepl-bindings)

  ;; Change directory of the buffer when we change directory in sly
  (add-hook 'sly-change-directory-hooks 'cd)
  (remove-hook 'sly-compilation-finished-hook 'sly-show-compilation-log)
  (add-hook 'sly-compilation-finished-hook 'sly-maybe-show-compilation-log))

(when (package-installed-p 'company)
  (add-hook 'sly-mrepl-hook 'company-mode))

(use-package sly-asdf
  :after (sly)
  :ensure t
  :bind (:map sly-mode-map
              ("C-c L" . my/load-current-system-or-ask))
  :defer nil
  :config
  (defun my/load-current-system-or-ask ()
    (interactive)
    (if (sly-connected-p)
        (sly-asdf-load-system (or (sly-asdf-find-current-system) (sly-asdf-read-system-name)))
      (message "Not connected.")))

  (define-advice sly-asdf-read-system-name (:around (orig-function &optional prompt default-value)
                                                    my/sly-asdf-read-system-name-prefer-history)
    (funcall orig-function prompt (or default-value (car sly-asdf-system-history) (sly-asdf-find-current-system)))))

(use-package sly-macrostep
  :after (sly)
  :ensure t)

(use-package sly-named-readtables
  :after (sly)
  :ensure t)

(use-package sly-repl-ansi-color
  :ensure t
  :after (sly)
  :config (add-to-list 'sly-contribs 'sly-repl-ansi-color))

(use-package simple
  :hook ((text-mode help-mode) . turn-on-visual-line-mode))

(use-package sql-indent
  :ensure t
  :hook (sql-mode . sqlind-minor-mode))

(use-package text-mode
  :config
  (defun my/on-text-mode ()
    (setq truncate-lines nil
          word-wrap t))
  (add-hook 'text-mode-hook 'my/on-text-mode))

(use-package visual-fill-column
  :ensure t
  :hook (visual-line-mode . visual-fill-column-mode)
  :custom
  (visual-fill-column-center-text nil)
  (visual-fill-column-fringes-outside-margins nil)
  :config
  (advice-add 'text-scale-adjust :after 'visual-fill-column-adjust))

(use-package web-mode
  :ensure t
  :mode "\\.html?$"
  :custom
  (web-mode-code-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-enable-auto-closing t)
  (web-mode-enable-auto-indentation t)
  (web-mode-enable-auto-pairing t)
  (web-mode-enable-auto-quoting t)
  (web-mode-markup-indent-offset 2)
  (web-mode-auto-close-style 2)
  (web-mode-enable-css-colorization t)
  (web-mode-enable-current-element-highlight t)
  (web-mode-enable-current-column-highlight t)
  :init
  (when (eq system-type 'windows-nt)
    (my/scoop-ensure "tidy")))

(use-package web-mode-edit-element
  :ensure t
  :hook (web-mode . web-mode-edit-element-minor-mode))

(use-package xref-js2
  :ensure t
  :after js2-mode
  :config
  ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
  ;; unbind it.
  (define-key js-mode-map (kbd "M-.") nil)

  (defun my/add-js2-xref-backend ()
    (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))
  (add-hook 'js2-mode-hook 'my/add-js2-xref-backend))

(use-package yaml-mode
  :ensure t)

(defun my/eval-sexp-and-replace (prefix)
  (interactive "P")
  (if (and (mark) (use-region-p))
      (let ((val (eval (read (buffer-substring-no-properties (region-beginning) (region-end))))))
        (delete-region (region-beginning) (region-end))
        (save-excursion
          (goto-char (region-beginning))
          (prin1 val (current-buffer))))
    (let ((val (eval (preceding-sexp)))
          (opoint (point)))
      (backward-sexp)
      (delete-region (point) opoint)
      (prin1 val (current-buffer)))))

(global-set-key (kbd "C-c e") 'my/eval-sexp-and-replace)

(defun my/kill-region-or-kill-line ()
  (interactive)
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-line)))

(defun my/kill-region-or-kill-word (arg)
  (interactive "p")
  (if (use-region-p)
      (kill-region (region-beginning) (region-end))
    (kill-word arg)))

;; source: http://steve.yegge.googlepages.com/my-dot-emacs-file
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

;;;; Global key bindings
(global-set-key (kbd "<M-f4>") (if (or server-mode (daemonp)) 'delete-frame 'save-buffers-kill-emacs))

(defun my/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "<C-f4>") 'my/kill-current-buffer)

(global-set-key (kbd "<C-tab>") 'other-window)
(defun my/previous-window ()
  (interactive)
  (other-window -1))
(global-set-key (kbd "<C-S-tab>") 'my/previous-window)

(global-set-key [remap kill-line] 'my/kill-region-or-kill-line)
(global-set-key [remap kill-word] 'my/kill-region-or-kill-word)

(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;;Line numbers on files
(when (symbol-function 'display-line-numbers-mode)
  (add-hook 'find-file-hook 'display-line-numbers-mode)
  (setq-default display-line-numbers-width 3))

(when (and (package-installed-p 'window-purpose)
           (package-installed-p 'magit))
  (purpose-set-extension-configuration
   :magit
   (purpose-conf :mode-purposes '((magit-mode . Magit))))

  (add-to-list 'purpose-special-action-sequences
               '(Magit
                 purpose-display-reuse-window-buffer
                 purpose-display-reuse-window-purpose
                 purpose-display-pop-up-frame)))

(when (and (package-installed-p 'window-purpose)
           (package-installed-p 'sly))
  (purpose-set-extension-configuration
   :sly
   (purpose-conf :name-purposes '(("*sly-macroexpansion*" . search)
                                  ("*sly-description*" . search)
                                  (" *sly-completion doc*" . search)
                                  ("*sly-compilation*" . search))
                 :regexp-purposes '(("\\*sly-db.\**" . search)
                                    ("\\*sly-xref.\**" . search))
                 :mode-purposes '((sly-mrepl-mode . terminal)
                                  (sly-inspector-mode . search))))

  (defun my/sly-load-layout ()
    (let ((layout (purpose-find-window-layout "sly")))
      (when layout
        (purpose-load-window-layout-file layout))))
  (add-hook 'sly-connected-hook 'my/sly-load-layout t)

  (defun my/sly-reset-layout (_)
    (purpose-load-recent-window-layout 1))
  (add-hook 'sly-net-process-close-hooks 'my/sly-reset-layout t)

  (defun my/sly-inspector-mode-hook ()
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push (cons 'sly-mode
                (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "M-.") 'sly-edit-definition-other-window)
                  map))
          minor-mode-overriding-map-alist))
  (add-hook 'sly-inspector-mode-hook 'my/sly-inspector-mode-hook)

  (defun my/sly-mrepl-mode-hook ()
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push (cons 'sly-mode
                (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "M-.") 'sly-edit-definition-other-window)
                  map))
          minor-mode-overriding-map-alist))
  (add-hook 'sly-mrepl-mode-hook 'my/sly-mrepl-mode-hook)

  (defun my/sly-db-mode-hook ()
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push (cons 'sly-mode
                (let ((map (make-sparse-keymap)))
                  (define-key map (kbd "M-.") 'sly-edit-definition-other-window)
                  map))
          minor-mode-overriding-map-alist))
  (add-hook 'sly-db-mode-hook 'my/sly-db-mode-hook))

(when (package-installed-p 'window-purpose)
  (defun my/load-default-layout (frame)
    (when (file-exists-p purpose-default-layout-file)
      (with-selected-frame frame
        (purpose-load-window-layout-file))))
  (add-hook 'after-make-frame-functions 'my/load-default-layout t)

  (defun my/save-default-layout (frame)
    (with-selected-frame frame
      (purpose-save-window-layout-file)))
  (add-hook 'delete-frame-functions 'my/save-default-layout t))


(setq debug-on-error nil)
