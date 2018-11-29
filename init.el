;;-*-coding: utf-8-*-

(require 'cl)

;;Turn on debugging so I can fix any init breakage. Turns off at the end
;;But only turn on for graphic displays. Otherwise a daemon will be stuck
(setq debug-on-error (display-graphic-p))

(setq-default buffer-file-coding-system 'utf-8-unix)
(setq-default default-buffer-file-coding-system 'utf-8-unix)
(set-default-coding-systems 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)
(set-terminal-coding-system 'utf-8-unix)
(set-keyboard-coding-system 'utf-8-unix)

(defun my/add-load-if-exists (path)
  (let ((dpath (file-name-as-directory path)))
    (when (file-exists-p dpath)
      (add-to-list 'load-path path))))

;; keep customize settings in their own file
(setq custom-file "~/.emacs.d/custom.el")

(load custom-file t)

;; Tell emacs where central backup directory is, and turn it on
;; Save all tempfiles in $TMPDIR/emacs$UID/
(let ((emacs-tmp-dir
       (format "%s%s/" temporary-file-directory "emacs")))
  (setq backup-directory-alist
        `((".*" . ,(concat emacs-tmp-dir "backups/"))))
  (setq auto-save-file-name-transforms
        `((".*" ,(concat emacs-tmp-dir "auto-saves/") t)))
  (setq auto-save-list-file-prefix (concat emacs-tmp-dir "auto-saves/")))

;;;; Set up package management info

;; initialize all ELPA packages
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(my/add-load-if-exists "~/.emacs.d/lisp/")

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

;; (use-package powerline
;;   :ensure t
;;   :config
;;   (progn
;;     (set-face-attribute 'mode-line nil
;;                     :foreground "Black"
;;                     :background "DarkOrange"
;;                     :box nil)
;;     (setq powerline-arrow-shape 'curve)))

(use-package nyan-mode
  :ensure t
  :config
  (progn
    (nyan-mode)))

(use-package elcord
  :ensure t
  :config
  (progn
    (elcord-mode)))

;; (use-package window-purpose
;;   :ensure t
;;   :bind (:map purpose-mode-map
;;               ("C-x C-f" . nil))
;;   :defer nil
;;   :config
;;   (progn
;;     (purpose-mode t)
;;     (defun my/display-at-bottom-and-select (buffer alist)
;;       (let ((window (purpose-display-at-bottom buffer alist)))
;;         (when window
;;           (select-window window))
;;         window))

;;     (defun purpose-display-split-sly-repl-right (buffer alist)
;;       "Display debugger window on the right side of repl."
;;       (let ((first-window (cl-find-if
;;                            (lambda (window)
;;                              (with-current-buffer (window-buffer window) (eq major-mode 'sly-mrepl-mode)))
;;                            (window-list))))
;;         (when first-window
;;           (let ((new-window (split-window first-window nil 'right)))
;;             (purpose-change-buffer buffer new-window 'window alist)
;;             new-window))))

;;     (add-to-list
;;      'purpose-special-action-sequences
;;      '(search-results
;;        purpose-display-reuse-window-purpose
;;        my/display-at-bottom-and-select))

;;     (add-to-list
;;      'purpose-special-action-sequences
;;      '(repl
;;        purpose-display-reuse-window-buffer
;;        purpose-display-reuse-window-purpose
;;        my/display-at-bottom-and-select))

;;     (add-to-list
;;      'purpose-special-action-sequences
;;      '(status
;;        purpose-display-reuse-window-buffer
;;        purpose-display-reuse-window-purpose
;;        purpose-display-split-sly-repl-right
;;        my/display-at-bottom-and-select))

;;     ;; Sly repl
;;     (add-to-list 'purpose-user-mode-purposes '(sly-mrepl-mode . repl))
;;     (add-to-list 'purpose-user-regexp-purposes '("\\*sly-mrepl" . repl))
;;     ;; Sly debugger
;;     (add-to-list 'purpose-user-mode-purposes '(sly-db-mode . status))
;;     (add-to-list 'purpose-user-regexp-purposes '("\\*sly-db" . status))
;;     ;; Sly inspector
;;     (add-to-list 'purpose-user-mode-purposes '(sly-inspector-mode . status))
;;     ;; Sly compile file
;;     (add-to-list 'purpose-user-mode-purposes '(compilation-mode . status))
;;     (purpose-compile-user-configuration)))


;; ;; (use-package window-purpose-x
;; ;;   :config
;; ;;   (progn
;; ;;     (purpose-x-kill-setup)
;; ;;     (purpose-x-magit-single-on)))

;; (use-package bs)

(use-package whitespace
  :diminish global-whitespace-mode
  :config
  (progn
    (global-whitespace-mode)))

(use-package rainbow-delimiters
  :ensure t
  :config
  (progn
    (add-hook 'lisp-mode-hook 'rainbow-delimiters-mode-enable)
    (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode-enable)))

(use-package solaire-mode
  :ensure t
  :hook ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  :config
  (progn
    (add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)
    (solaire-mode-swap-bg)))

;;;completion & input
(use-package company
  :ensure t
  :defer nil
  :diminish company-mode
  :bind
  (:map company-mode-map
        ("<tab>" . company-indent-or-complete-common)
        :map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("C-d" . company-show-doc-buffer)
        ("M-." . company-show-location)
        ("<tab>" . company-select-next)
        ("C-<tab>" . company-select-previous)
        ("RET" . company-complete-selection))
  :config
  (progn
    (company-tng-configure-default)
    (add-hook 'after-init-hook 'global-company-mode)))

(use-package company-quickhelp
  :ensure t
  :config
  (progn
    (add-hook 'after-init-hook 'company-quickhelp-mode)))

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
         ("<Tab>" . helm-execute-persistent-action)
         ("C-z" . helm-select-action)
         :map helm-find-files-map
         ("C-s" . helm-ff-run-grep-ag))
  :config
  (progn
    (require 'helm-config)
    (helm-mode t)))

(use-package helm-swoop
  :ensure t
  :bind (("M-i" . helm-swoop)
         ("M-I" . helm-swoop-back-to-last-point)
         ("C-c M-i" . helm-multi-swoop)
         ("C-x M-i" . helm-multi-swoop-all)
         :map helm-swoop-map
         ("C-s" . helm-next-line)
         ("C-r" . helm-previous-line)))

(use-package helm-company
  :ensure t
  :defer nil
  :bind (:map company-mode-map
              ("C-:" . helm-company)
              :map company-active-map
              ("C-:" . helm-company)))

;;; window/frame layout

(use-package with-editor
  :ensure t
  :hook (shell-mode . with-editor-export-editor)
  :hook (term-exec . with-editor-export-editor)
  :hook (eshell-mode . with-editor-export-editor)
  :config
  (progn
    (define-key (current-global-map)
      [remap async-shell-command] 'with-editor-async-shell-command)
    (define-key (current-global-map)
      [remap shell-command] 'with-editor-shell-command)))

(use-package psession
  :ensure t
  :config
  (progn
    (psession-mode 1)
    (psession-savehist-mode 1)
    (psession-autosave-mode 1)))

;; (use-package helm-purpose
;;   :ensure t
;;   :config
;;   (progn
;;     (helm-purpose-setup)))

;;;editing
(use-package paredit
  :ensure t
  :diminish paredit-mode
  :config
  (progn
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
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (when (package-installed-p 'sly)
      (add-hook 'sly-mode-hook 'paredit-mode))))

;; (use-package projectile
;;   :ensure t
;;   :defer nil
;;   :bind (:map projectile-mode-map
;;               ("s-p" . 'projectile-command-map)
;;               ("C-c p" . 'projectile-command-map))
;;   :config
;;   (progn
;;     (projectile-mode +1)))

(when (executable-find "git")
  (use-package magit
    :ensure t
    :bind (:map magit-status-mode-map
                ("<C-tab>" . 'my/next-window-or-buffer)
                :map magit-diff-mode-map
                ("<C-tab>" . 'my/next-window-or-buffer))
    :defer t
    :init
    (progn
      ;;Tell's git to use the TK GUI to ask for password
      ;;This is a workaround to a problem with git when pushing
      ;;over https
      (setenv "GIT_ASKPASS" "git-gui--askpass"))))

(use-package neotree
  :if (package-installed-p 'neotree)
  :bind (:map neotree-mode-map
              ("M-l" . my/neotree-go-up)
              ("C-z" . my/neotree-go-down))
  :config
  (progn
    (global-set-key [f8] 'neotree-toggle)

    (defun my/neotree-go-down ()
      (interactive)
      (let ((dst (neo-buffer--get-filename-current-line)))
        (when (file-directory-p dst)
          (neo-global--open-dir dst))))

    (defun my/neotree-go-up ()
      (interactive)
      (let ((prev-root neo-buffer--start-node)
            (new-root (file-name-directory(directory-file-name neo-buffer--start-node))))
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
      (my/select-neotree-file (buffer-file-name (get-buffer buffer-or-name))))))

(use-package rg
  :if (executable-find "rg")
  :ensure t)

;;; passive tools

;;; languages & editing

(use-package adoc-mode
  :ensure t
  :mode "\\.adoc$"
  :mode "\\.ad$"
  :mode "\\.asciidoc$")

(use-package antlr-mode
  :ensure t
  :mode "\\.g4$")

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
                  (ggtags-mode 1)))))

(use-package cperl-mode
  :ensure t
  :mode "\\.pl$"
  :mode "\\.perl$")

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
              ("/" . my/comment-region-if-mark)))

(use-package lua-mode
  :ensure t
  :mode "\\.lua$"
  :interpreter "lua")

(use-package windsize
  :ensure t
  :bind (("C-S-<up>" . windsize-up)
         ("C-S-<down>" . windsize-down)
         ("C-S-<left>" . windsize-left)
         ("C-S-<right>" . windsize-right)))

(use-package markdown-mode
  :ensure t
  :mode "\\.text$"
  :mode "\\.markdown$"
  :mode "\\.md$"
  :mode ("README\\.md$" . gfm-mode))

(use-package sly
  :if (package-installed-p 'sly)
  :config
  (progn
    (defun kill-sly-buffers ()
      (interactive)
      (mapc
       (lambda (buffer)
         (when (or (eql (string-match "\\*sly" (buffer-name buffer)) 0)
                   (eql (string-match " \\*sly" (buffer-name buffer)) 0))
           (kill-buffer buffer)))
       (buffer-list)))))

(use-package slime
  :if (package-installed-p 'slime)
  :config
  (progn
    (setq slime-contribs '(slime-fancy))))

;; automatic disassembly
(use-package autodisass-java-bytecode   ; auto-disassemble Java bytecode
  :ensure t)

(use-package autodisass-llvm-bitcode    ; auto-disassemble LLVM bitcode
  :ensure t)

(when (eq system-type 'windows-nt)
  (set-message-beep 'silent))

;; Replace "yes or no" with y or n
(defalias 'yes-or-no-p 'y-or-n-p)
(put 'erase-buffer 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

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

(defun explorer ()
  (interactive)
  (async-shell-command (format "explorer \"%s\"" (subst-char-in-string ?/ ?\\ default-directory)) 0))

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

(defun my/next-window-or-buffer ()
  (interactive)
  (cond
   ((cdr (window-list)) ; more than one window
    (other-window 1))
   (t
    (switch-to-buffer (other-buffer)))))

(defun my/prev-window-or-buffer ()
  (interactive)
  (cond
   ((cdr (window-list))
    (other-window -1))
   (t
    (switch-to-buffer (other-buffer)))))

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
(global-set-key (kbd "<C-f4>") 'my/kill-current-buffer)
(global-set-key (kbd "C-x C-k") 'my/kill-current-buffer)
(global-set-key (kbd "C-x k") 'kill-buffer)

(global-set-key [remap delete-char] 'my/delete-region-or-char)
(global-set-key [remap delete-forward-char] 'my/delete-region-or-char)
(global-set-key [remap delete-backward-char] 'my/delete-region-or-backward)

(global-set-key [remap kill-line] 'my/kill-region-or-kill-line)
(global-set-key [remap kill-word] 'my/kill-region-or-kill-word)

(global-set-key (kbd "<C-tab>") 'my/next-window-or-buffer)
(global-set-key (kbd "<C-S-tab>") 'my/prev-window-or-buffer)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "<C-wheel-up>") 'text-scale-increase)
(global-set-key (kbd "<C-wheel-down>") 'text-scale-decrease)

;;;; Mode-specific keybindings

(define-key emacs-lisp-mode-map (kbd "C-x C-e") 'my/eval-last-sexp-or-region)

;;;; Hooks
(add-hook 'lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)

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
(put 'list-timers 'disabled nil)
(setq debug-on-error nil)
