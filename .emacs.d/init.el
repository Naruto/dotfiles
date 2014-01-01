;; user info
(setq user-full-name "Naruto TAKAHASHI")
(setq user-mail-address "tnaruto@gmail.com")

(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

;; path
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory
              (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "elisp" "conf" "public_repos")

;; code
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(when (and (eq (window-system) 'x)
	 (>= emacs-major-version 23))

  ;; disable toolbar
  (cond ((eq emacs-major-version 24) (tool-bar-mode 0))
        (t (tool-bar-mode nil)))

  (require 'xclip)

  (set-background-color "#efefdf");background
  (set-foreground-color "#202041");foreground
  (set-cursor-color "#202041");cursor

  ;; VL Gothic
  (set-default-font "VL Gothic-8")
  (set-fontset-font (frame-parameter nil 'font)
		    'japanese-jisx0208
		    '("VL Gothic-8" . "unicode-bmp"))
  (set-fontset-font (frame-parameter nil 'font)
		    'katakana-jisx0201
		    '("VL Gothic-8" . "unicode-bmp"))
  (set-fontset-font (frame-parameter nil 'font)
		    'ascii
		    '("VL Gothic-8" . "unicode-bmp"))
  (set-fontset-font (frame-parameter nil 'font)
		    'unicode
		    '("VL Gothic-8" . "unicode-bmp"))
  )

;;; misc
(setq inhibit-startup-message t)
(menu-bar-mode t)
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'indent-relative-maybe)
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
(show-paren-mode)
(iswitchb-mode)
(require 'dired-x)

(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


;; auto-save-buffers
(when (require 'auto-save-buffers nil t)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (run-with-idle-timer 0.5 t 'auto-save-buffers)
)

;; auto-install 
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  ;; (setq url-proxy-services '(("http" . "HOST:PORT")))
  (auto-install-compatibility-setup))

;; redo+
;(when (require 'redo+ nil t)
;  (global-set-key (kbd "C-'") 'redo))

;; package.el for ELPA
(when (<= emacs-major-version 23)
  (when (require 'package nil t)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
    (package-initialize)))

;; yasnippet
(when (require 'yasnippet nil t)
  (setq yas-snippet-dirs
        '(
          "~/.emacs.d/snippets"
          "~/.emacs.d/public_repos/yasnippet/snippets"
          ))
  (yas-global-mode 1)
)

;; auto-complete mode
(when (file-exists-p
       (expand-file-name (concat user-emacs-directory
                                 "public_repos/auto-complete")))
  (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
               "~/.emacs.d/public_repos/auto-complete/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default)

  ;; auto-complete-yasnippet
  (add-to-list 'ac-sources 'ac-source-yasnippet)

  ;; auto-complete-clang
  (require 'auto-complete-clang)
  (setq ac-auto-start nil)
  (setq ac-quick-help-delay 0.5)
  (defun my-ac-config ()
    (setq-default ac-sources
                  '(ac-source-abbrev
                    ac-source-dictionary ac-source-words-in-same-mode-buffers))
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    ;; (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
    (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (global-auto-complete-mode t))
  (defun my-ac-cc-mode-setup ()
    (setq ac-sources 
          (append '(ac-source-clang ac-source-yasnippet) ac-sources)))
  (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
  ;; ac-source-gtags
  (my-ac-config)
)

;; undohist
(when (require 'undohist nil t)
  (undohist-initialize))

;; howm
(setq howm-menu-lang 'ja)
(setq howm-process-coding-system 'utf-8-unix)
(when (require 'howm-mode nil t)
  (setq howm-directory "~/howm")
  (define-key global-map (kbd "C-c ,,") 'howm-menu)
  (setq howm-refresh-after-save nil)
  (setq howm-history-limit nil)
  (cond ((executable-find "ack")
             (progn
               (setq howm-view-use-grep t)
               (setq howm-view-grep-command "ack")
               (setq howm-view-grep-option "-Hnr")
               (setq howm-view-grep-extended-option "")
               (setq howm-view-grep-fixed-option "--literal")
               (setq howm-view-grep-expr-option "--match")
               (setq howm-view-grep-file-stdin-option nil)
               ))
        ((executable-find "grep")
         (progn
           (setq howm-view-use-grep t)
           (setq howm-view-grep-command "grep")
           (setq howm-view-fgrep-command "fgrep")
           )))
)

;; cua-mode
(cua-mode t)
(setq cua-enable-cua-keys nil)

;; psvn
(when (executable-find "svn")
  (setq svn-status-verbose nil)
  (autoload 'svn-status"psvn" "Run `svn status'." t))

;; egg
(when (executable-find "git")
  (require 'egg nil t))

;; multi-term
(when (require 'multi-term nil t)
  )

;; elscren
(when (require 'elscreen nil t)
  (require 'elscreen-howm nil t)
  (require 'elscreen-gf nil t))

;; skk
(when (require 'skk-autoloads nil t)
  (define-key global-map (kbd "C-x C-j") 'skk-mode)
  (define-key global-map (kbd "C-x j") 'skk-mode)
  (setq skk-dcomp-activate t)
  (setq skk-large-jisyo "~/.emacs.d/share/skk/SKK-JISYO.L")
  ;(setq skk-server-host "HOST")
  ;(setq skk-server-portnum PORTNUM)
  )

;; cc-mode
(when (require 'cc-mode nil t)
  ;; e17-c-style
  (c-add-style "e"
               '("gnu"
                 (show-trailing-whitespace t)
                 (indent-tabs-mode . nil)
                 (tab-width . 8)
                 (c-offsets-alist .
                                  ((defun-block-intro . 3)
                                   (statement-block-intro . 3)
                                   (case-label . 1)
                                   (statement-case-intro . 3)
                                   (inclass . 3)
                                   ))))

  ;; kernel
  (defun c-lineup-arglist-tabs-only (ignored)
    "Line up argument lists by tabs, not spaces"
    (let* ((anchor (c-langelem-pos c-syntactic-element))
           (column (c-langelem-2nd-pos c-syntactic-element))
           (offset (- (1+ column) anchor))
           (steps (floor offset c-basic-offset)))
      (* (max steps 1)
         c-basic-offset)))
  (c-add-style "linux-tabs-only"
               '("linux"
                 (show-trailing-whitespace t)
                 (indent-tabs-mode . t)
                 (tab-width . 8)
                 (c-offsets-alist (arglist-cont-nonempty
                                   c-lineup-gcc-asm-reg
                                   c-lineup-arglist-tabs-only))))

  ;; set e-style as default indent style.
  (setq c-default-style
        '((c-mode . "e")
          (c++-mode . "e")))
  )

;; gtags
(autoload 'gtags-mode "gtags" "" t)
(setq gtags-mode-hook
  '(lambda ()
        (define-key gtags-mode-map "\eh" 'gtags-display-browser)
        (define-key gtags-mode-map "\C-]" 'gtags-find-tag-from-here)
        (define-key gtags-mode-map "\C-t" 'gtags-pop-stack)
        (define-key gtags-mode-map "\el" 'gtags-find-file)
;;        (define-key gtags-mode-map "\eg" 'gtags-find-with-grep)
;;        (define-key gtags-mode-map "\eI" 'gtags-find-with-idutils)
        (define-key gtags-mode-map "\es" 'gtags-find-symbol)
        (define-key gtags-mode-map "\er" 'gtags-find-rtag)
        (define-key gtags-mode-map "\et" 'gtags-find-tag)
;        (define-key gtags-mode-map "\ev" 'gtags-visit-rootdir)
        ))
(add-hook 'c-mode-common-hook
          '(lambda ()
             (progn
               (gtags-mode 1)
               ;(c-toggle-hungry-state 1)
               )))

;;; gud-mode                  
;; many widnows mode          
(setq gdb-many-windows t)     
(setq gdb-use-separate-io-buffer t)
                              
;;; window mode               
(windmove-default-keybindings)
(setq windmove-wrap-around t) 

; (require 'rust-mode)

;; markdown mode
(when (require 'markdown-mode nil t)
  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
)

;; migemo
;; http://qiita.com/catatsuy/items/c5fa34ead92d496b8a51
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-command "cmigemo")
  (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")
  (load-library "migemo")
  (migemo-init)
)

;; emacs helm
(when (file-exists-p
       (expand-file-name (concat user-emacs-directory "public_repos/helm")))
  (require 'helm-config)
  (global-set-key (kbd "C-c h") 'helm-mini)
)

;; ag
(when (file-exists-p
       (expand-file-name (concat user-emacs-directory "public_repos/ag.el")))
  (add-to-load-path "public_repos/ag.el")
  (require 'ag nil t)
  ; (setq ag-highlight-search t)
)

;; cmake
(when (require 'cmake-mode nil t)
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist))
)
