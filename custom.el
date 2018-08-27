;;-*-coding: utf-8-*-
;;Need to specify coding here because I have some unicode in this file,
;;and it gets read before I have a chance to utf-8 as the default encoding.

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(c-default-style
   (quote
    ((c-mode . "stroustrup")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (awk-mode . "awk"))))
 '(clution-frontend (quote roswell))
 '(clution-mode nil)
 '(clution-run-style (quote term))
 '(company-idle-delay nil)
 '(cperl-close-paren-offset -4)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-indent-parens-as-block t)
 '(create-lockfiles nil)
 '(current-language-environment "UTF-8")
 '(cursor-in-non-selected-windows nil)
 '(custom-safe-themes
   (quote
    ("84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "5e52ce58f51827619d27131be3e3936593c9c7f9f9f9d6b33227be6331bf9881" "ffca7ac44bfe9d585363f6bbf29f19529de216f85dce7a831dfc28883959ec05" "ff7625ad8aa2615eae96d6b4469fcc7d3d20b2e1ebc63b761a349bebbb9d23cb" "3c9d994e18db86ae397d077b6324bfdc445ecc7dc81bb9d528cd9bba08c1dac1" "fb4bf07618eab33c89d72ddc238d3c30918a501cf7f086f2edf8f4edba9bd59f" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(custom-theme-directory "~/.emacs.d/themes")
 '(delete-old-versions t)
 '(doc-view-ghostscript-program "gswin32c")
 '(echo-keystrokes 0.1)
 '(eldoc-echo-area-use-multiline-p t)
 '(enable-local-variables nil)
 '(find-file-visit-truename nil)
 '(frame-resize-pixelwise t)
 '(global-auto-revert-mode t)
 '(global-hl-line-mode t)
 '(helm-boring-buffer-regexp-list
   (quote
    ("\\*helm" "\\*Echo Area" "\\*Minibuf" "\\*Help.*" "\\*sly-inferior" "\\*sly-events")))
 '(helm-ff-skip-boring-files t)
 '(ido-ignore-buffers
   (quote
    ("\\*Pp Eval Output\\*" "\\*pu-dummy" "\\` " "\\*slime-compilation\\*" "\\*slime-events\\*" "\\*inferior-lisp\\*" "\\*Fuzzy Completions\\*" "\\*Completions\\*")))
 '(indent-tabs-mode nil)
 '(indicate-empty-lines t)
 '(inferior-lisp-program "sbcl")
 '(inhibit-startup-screen t)
 '(js-indent-level 4)
 '(kept-new-versions 6)
 '(keyboard-coding-system (quote utf-8-unix))
 '(large-file-warning-threshold 26214400)
 '(lua-indent-level 4)
 '(magit-set-upstream-on-push (quote dontask))
 '(make-pointer-invisible nil)
 '(markdown-command "pandoc")
 '(menu-bar-mode nil)
 '(mode-require-final-newline nil)
 '(mouse-wheel-scroll-amount (quote (3 ((shift) . 1) ((control)))))
 '(mouse-yank-at-point t)
 '(neo-dont-be-alone t)
 '(neo-hidden-regexp-list
   (quote
    ("^\\." "\\.pyc$" "~$" "^#.*#$" "\\.elc$" "\\.fasl$")))
 '(neo-smart-open t)
 '(neo-theme (quote nerd))
 '(nxml-attribute-indent 2)
 '(nxml-slash-auto-complete-flag t)
 '(nyan-animate-nyancat t)
 '(nyan-animation-frame-interval 0.2)
 '(nyan-bar-length 20)
 '(nyan-mode nil)
 '(nyan-wavy-trail t)
 '(package-selected-packages
   (quote
    (helm-swoop helm-ag helm-rg smart-mode-line-powerline-theme powerline solaire-mode doom-themes sly-named-readtables sly-repl-ansi-color sly-quicklisp sly-macrostep sly elcord csharp-mode adoc-mode rg neotree magit paredit helm-purpose with-editor helm-company helm elisp-slime-nav company-quickhelp company rainbow-delimiters window-purpose nyan-mode zenburn-theme diminish use-package)))
 '(pop-up-frames nil)
 '(purpose-preferred-prompt (quote helm) t)
 '(purpose-user-mode-purposes nil)
 '(purpose-user-name-purposes
   (quote
    (("*slime-compilation*" . lisp-status)
     ("*slime-error*" . lisp-status)
     ("*slime-xref*" . lisp-status)
     ("*slime-repl sbcl*" . lisp-repl)
     ("*slime-inspector*" . lisp-inspector)
     ("*slime-macroexpansion*" . lisp-inspector)
     ("*slime-threads*" . lisp-status)
     ("*slime-description*" . lisp-inspector)
     ("*slime-browser*" . lisp-inspector))))
 '(purpose-user-regexp-purposes
   (quote
    (("\\*sldb.*" . lisp-status)
     ("\\*ag search.*" . search-results)
     ("\\*rg.*" . search-results))))
 '(read-file-name-completion-ignore-case t)
 '(rg-group-result nil)
 '(scroll-bar-mode nil)
 '(scroll-conservatively 101)
 '(scroll-step 1)
 '(show-trailing-whitespace t)
 '(sly-command-switch-to-existing-lisp (quote (quote always)))
 '(sly-ignore-protocol-mismatches t)
 '(sly-kill-without-query-p t)
 '(sly-mrepl-history-file-name "~/.emacs.d/.sly-mrepl-history")
 '(sly-net-coding-system (quote utf-8-unix))
 '(smooth-scroll-margin 4)
 '(smooth-scrolling-mode nil)
 '(tool-bar-mode nil)
 '(use-dialog-box nil)
 '(use-file-dialog nil)
 '(user-full-name "Wilfredo Velázquez-Rodríguez")
 '(user-mail-address "zulu.inuoe@gmail.com")
 '(version-control t)
 '(x-select-request-type (quote (UTF8_STRING COMPOUND_TEXT TEXT STRING))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
