;;-*-coding: utf-8-unix-*-
(require 'cl)

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

(defun my/disable-show-trailing-whitespace ()
  (setq show-trailing-whitespace nil))

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
   create-lockfiles nil
   backup-by-copying t
   backup-directory-alist `((".*" . ,backups-dir))
   auto-save-file-name-transforms `((".*" ,auto-saves-dir t))
   auto-save-list-file-prefix auto-saves-dir
   recentf-auto-cleanup 'never
   recentf-save-file recentf-file))

(global-auto-revert-mode +1)
(recentf-mode +1)

;; Minor visual/input stuff
(setq-default
 inhibit-startup-screen t
 make-pointer-invisible nil
 echo-keystrokes 0.1
 frame-resize-pixelwise t
 eldoc-echo-area-use-multiline-p t
 read-file-name-completion-ignore-case t
 find-file-visit-truename nil
 mouse-wheel-scroll-amount '(3 ((shift) . 1) ((control)))
 cursor-in-non-selected-windows nil
 scroll-conservatively 101
 scroll-step 1
 truncate-lines t)

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

;;;; Set up package management info

;; initialize all ELPA packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(my/add-load-if-exists (expand-file-name "lisp/" user-emacs-directory))

;; Sly hacking
(my/add-load-if-exists "~/code/sly/")

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
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

(unless (or (package-installed-p 'doom-themes) (package-installed-p 'zenburn-theme))
  (package-install 'doom-themes))

(cond
 ((package-installed-p 'doom-themes)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  (load-theme 'doom-one t)
  (doom-themes-visual-bell-config)
  (cond
   ((package-installed-p 'treemacs)
    (doom-themes-treemacs-config))
   ((package-installed-p 'neotree)
    (doom-themes-neotree-config))))
 ((package-installed-p 'zenburn-theme)
  (load-theme 'zenburn t)))

;; Doom will for whatever reason reset the buffer coding system..
(setq-default buffer-file-coding-system 'utf-8-unix)

(load "local.el" t nil t t)

(use-package clution
  :if (require 'clution nil t)
  :custom
  (clution-run-style 'term))

(use-package nyan-mode
  :ensure t
  :custom
  (nyan-animate-nyancat t)
  (nyan-bar-length 10)
  (nyan-mode nil)
  (nyan-wavy-trail t)
  :config
  (nyan-mode +1))

(use-package elcord
  :if (package-installed-p 'elcord)
  :config
  (elcord-mode))

(use-package whitespace
  :diminish global-whitespace-mode
  :custom
  (whitespace-style
   '(face trailing tabs spaces newline empty indentation space-after-tab
          space-before-tab space-mark tab-mark newline-mark))
  :config
  (global-whitespace-mode +1))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode-enable)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable))

(use-package solaire-mode
  :ensure t
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
  (solaire-mode-swap-bg))

;;;completion & input
(use-package company
  :ensure t
  :diminish company-mode
  :defer nil
  :bind
  (:map company-mode-map
        ("<tab>" . company-indent-or-complete-common)
        :map company-active-map
        ("<prior>" . company-previous-page)
        ("<next>" . company-next-page)
        ("<down>" . company-select-next)
        ("<up>" . company-select-previous)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("M-." . company-show-location)
        ("SPC" . my/company-abort-and-insert))
  :custom
  (company-idle-delay 0)
  (company-require-match nil)
  :config
  (defun my/company-abort-and-insert ()
    (interactive)
    (company-abort)
    (self-insert-command 1))
  (global-company-mode 1))

(use-package elisp-slime-nav
  :ensure t
  :diminish elisp-slime-nav-mode
  :hook ((lisp-interaction-mode emacs-lisp-mode) . elisp-slime-nav-mode))

(use-package back-button
  :ensure t
  :diminish back-button-mode
  :bind (("<mouse-4>" . back-button-global-backward)
         ("<mouse-5>" . back-button-global-forward)))

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
         ("<Tab>" . helm-execute-persistent-action)
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
  (require 'helm-config)
  (helm-mode +1))

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

(use-package helm-company
  :after (helm company)
  :ensure t
  :defer nil
  :bind (:map company-mode-map
              ("C-:" . helm-company)
              :map company-active-map
              ("C-:" . helm-company)))

(use-package shrink-path
  :ensure t
  :config
  (defun shrink-path--dirs-internal (full-path &optional truncate-all)
    "Return fish-style truncated string based on FULL-PATH.
Optional parameter TRUNCATE-ALL will cause the function to truncate the last
directory too."
    (let* ((home (expand-file-name "~"))
           (path (replace-regexp-in-string
                  (s-concat "^" home) "~" full-path))
           (split (s-split "/" path 'omit-nulls))
           (split-len (length split))
           shrunk)
      (->> split
           (--map-indexed (if (= it-index (1- split-len))
                              (if truncate-all (shrink-path--truncate it) it)
                            (shrink-path--truncate it)))
           (s-join "/")
           (setq shrunk))
      (s-concat (unless (s-matches? (rx bos (or "~" "/")) shrunk) "/")
                shrunk
                (unless (s-ends-with? "/" shrunk) "/")))))

(use-package all-the-icons
  :ensure t
  :config
  (when (eq system-type 'windows-nt)
    (defun my/install-fonts ()
      (call-process "powershell"
                    nil nil nil
                    "-NoProfile"
                    "-ExecutionPolicy" "Bypass"
                    "-Command" (expand-file-name "install-fonts.ps1" user-emacs-directory)))
    (defun my/uninstall-fonts ()
      (call-process "powershell"
                    nil nil nil
                    "-NoProfile"
                    "-ExecutionPolicy" "Bypass"
                    "-Command"  (expand-file-name "uninstall-fonts.ps1" user-emacs-directory)))

    (my/install-fonts)
    (add-hook 'kill-emacs-hook 'my/uninstall-fonts)))

(use-package doom-modeline
  :custom
  (doom-modeline-icon t)
  (doom-modeline-major-mode-color-icon t)
  :ensure t
  :hook (after-init . doom-modeline-mode))

;;; window/frame layout

(use-package psession
  :ensure t
  :config
  (psession-mode +1)
  (psession-savehist-mode +1)
  (psession-autosave-mode +1))

(use-package slack
  :commands (start-slack)
  :custom
  (slack-buffer-enable-emojify t)
  (slack-prefer-current-team t))

;; (use-package helm-purpose
;;   :ensure t
;;   :config
;;   (progn
;;     (helm-purpose-setup)))

;;;editing
(use-package avy
  :ensure t
  :bind ("M-g c" . avy-goto-char))

(use-package which-key
  :ensure t
  :defer nil
  :diminish which-key-mode
  :config (which-key-mode +1))

(use-package windsize
  :ensure t
  :bind (("C-S-<up>" . windsize-up)
         ("C-S-<down>" . windsize-down)
         ("C-S-<left>" . windsize-left)
         ("C-S-<right>" . windsize-right)))

(use-package paredit
  :ensure t
  :diminish paredit-mode
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

  (when (package-installed-p 'eldoc)
    (eldoc-add-command
     'paredit-backward-delete
     'paredit-close-round))

  (define-key paredit-mode-map [remap paredit-semicolon] 'my/paredit-comment-line-or-sexp)
  (define-key paredit-mode-map [remap paredit-forward-delete] 'my/paredit-delete-region-or-forward)
  (define-key paredit-mode-map [remap paredit-backward-delete] 'my/paredit-delete-region-or-backward)
  (define-key paredit-mode-map [remap paredit-kill] 'my/paredit-kill-region-or-kill)

  (add-hook 'lisp-mode-hook 'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(when (executable-find "git")
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

(use-package gitattributes-mode
  :ensure t
  :mode "^\\.gitattributes$")

(use-package gitignore-mode
  :ensure t
  :mode "^\\.gitignore$")

(use-package gitconfig-mode
  :ensure t
  :mode "^\\.gitconfig$")

(use-package neotree
  :ensure t
  :bind (([f8] . neotree-toggle)
         :map neotree-mode-map
         ("C-l" . my/neotree-go-up)
         ("C-j" . my/neotree-go-down))
  :custom
  (neo-dont-be-alone t)
  (neo-hidden-regexp-list
   '("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.fasl$"))
  (neo-smart-open t)
  (neo-theme 'nerd)
  :config
  (defun my/neotree-go-down ()
    (interactive)
    (let ((dst (neo-buffer--get-filename-current-line)))
      (when (file-directory-p dst)
        (neo-global--open-dir dst))))

  (defun my/neotree-go-up ()
    (interactive)
    (let ((prev-root (file-name-as-directory neo-buffer--start-node))
          (new-root (file-name-directory (directory-file-name neo-buffer--start-node))))
      (message "Prev-root: %s" prev-root)
      (message "New root: %s" new-root)
      (neo-global--open-dir new-root)
      (neotree-find prev-root)))

  (defun my/select-neotree-file (file)
    (when (and file
               (neo-global--window-exists-p)
               (neo-global--file-in-root-p file))
      (with-selected-window (selected-window)
        (neotree-find file))))

  (defadvice switch-to-buffer (after update-neotree-advice
                                     (buffer-or-name
                                      &optional
                                      norecord force-same-window) activate)
    (my/select-neotree-file (buffer-file-name (get-buffer buffer-or-name))))

  (defadvice display-buffer (after update-neotree-advice
                                   (buffer-or-name
                                    &optional action frame) activate)
    (my/select-neotree-file (buffer-file-name (get-buffer buffer-or-name)))))

(use-package rg
  :if (executable-find "rg")
  :ensure t
  :custom
  (rg-group-result nil))

;;; passive tools

(use-package doc-view
  :custom
  (doc-view-ghostscript-program "gswin64c"))

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
  :init
  (add-hook 'c-mode-common-hook
            #'(lambda ()
                (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
                  (ggtags-mode +1)))))

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

(use-package doc-view
  :ensure t
  :bind (:map doc-view-mode-map
              ("M-v" . backward-page)
              ("C-v" . forward-page)))

(defun my/comment-region-if-mark ()
  (interactive)
  (if (use-region-p)
      (comment-region (min (point) (mark)) (max (point) (mark)))
    (self-insert-command 1)))

(use-package js
  :bind (:map js-mode-map
              ("/" . my/comment-region-if-mark))
  :custom (js-indent-level 2))

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua"
  :custom (lua-indent-level 4))

(use-package nxml-mode
  :custom
  (nxml-attribute-indent 2)
  (nxml-slash-auto-complete-flag t))

(use-package powershell
  :ensure t
  :mode ("\\.ps1$" . powershell-mode))

(use-package markdown-mode
  :ensure t
  :mode "\\.text$"
  :mode "\\.markdown$"
  :mode "\\.md$"
  :mode ("README\\.md$" . gfm-mode)
  :custom (markdown-command "pandoc"))

(use-package sly
;  :if (package-installed-p 'sly)
  :custom
  (inferior-lisp-program "sbcl")
  (sly-command-switch-to-existing-lisp 'always)
  (sly-ignore-protocol-mismatches t)
  (sly-kill-without-query-p t)
  (sly-mrepl-history-file-name (expand-file-name ".sly-mrepl-history" user-emacs-directory))
  (sly-net-coding-system 'utf-8-unix)
  :config
  (defun kill-sly-buffers ()
    (interactive)
    (mapc
     (lambda (buffer)
       (when (or (eql (string-match "\\*sly" (buffer-name buffer)) 0)
                 (eql (string-match " \\*sly" (buffer-name buffer)) 0))
         (kill-buffer buffer)))
     (buffer-list))))

(use-package slime
  :if (package-installed-p 'slime)
  :custom (slime-contribs '(slime-fancy)))

(use-package omnisharp
  :ensure t
  :after company
  :config
  (add-to-list 'company-backends 'company-omnisharp)
  (add-hook 'csharp-mode-hook 'omnisharp-mode))

(when (eq system-type 'windows-nt)
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

(defun my/kill-current-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(defun my/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (eval-last-sexp prefix)))

(defun my/delete-region-or-char (arg)
  (interactive "p")
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char arg)))

(defun my/delete-region-or-forward ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (delete-char 1)))

(defun my/delete-region-or-backward ()
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (backward-delete-char 1)))

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
(global-set-key (kbd "<M-f4>") 'delete-frame)
(global-set-key (kbd "<C-f4>") 'my/kill-current-buffer)

(global-set-key [remap delete-char] 'my/delete-region-or-char)
(global-set-key [remap delete-forward-char] 'my/delete-region-or-char)
(global-set-key [remap delete-backward-char] 'my/delete-region-or-backward)

(global-set-key [remap kill-line] 'my/kill-region-or-kill-line)
(global-set-key [remap kill-word] 'my/kill-region-or-kill-word)

(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;;;; Mode-specific keybindings

(define-key emacs-lisp-mode-map (kbd "C-x C-e") 'my/eval-last-sexp-or-region)

;;;; Hooks
(add-hook 'lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(diminish 'eldoc-mode)

;;Prettify symbols
(add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
(add-hook 'lisp-mode-hook 'prettify-symbols-mode)

;;Line numbers on files
(add-hook 'find-file-hook 'display-line-numbers-mode)

;;;; auto-mode-alist
(add-to-list 'auto-mode-alist '("\\.exe$" . hexl-mode))
(add-to-list 'auto-mode-alist '("\\.dll$" . hexl-mode))

(add-to-list 'auto-mode-alist '("\\.eclrc$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.ros$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.sbclrc$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.slynkrc$" . lisp-mode))

(add-to-list 'auto-mode-alist '("\\.proj$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.lyr$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.mtl$" . nxml-mode))
(add-to-list 'auto-mode-alist '("\\.xaml$" . nxml-mode))

(setq debug-on-error nil)
