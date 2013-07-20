;; user info
(setq user-full-name "Naruto TAKAHASHI")
(setq user-mail-address "tnaruto@gmail.com")

(when (> emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d"))

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
  ;; ツールバーを表示しない
  (if (eq tool-bar-mode t)
      (tool-bar-mode nil))

  (require 'xclip)

  ;; 配色設定
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
;; タイトルを表示しない
(setq inhibit-startup-message t)
;; メニューバーは表示する
(menu-bar-mode t)
;; 時間を表示するようにする
(setq display-time-day-and-date t)
(setq display-time-24hr-format t)
(display-time)
;; インデントに tab を使わない
(setq-default indent-tabs-mode nil)
(setq indent-line-function 'indent-relative-maybe)
;; 同一ファイル名のバッファ名をわかりやすくする
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(setq uniquify-ignore-buffers-re "*[^*]+*")
;; 括弧をハイライトする
(show-paren-mode)
;; バッファの検索を便利に
(iswitchb-mode)
;; dired を便利に
(require 'dired-x)

;;; shell モードで Escape Seaquence を有効に
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

;; ELPA 用 package.el
(when (<= emacs-major-version 23)
  (when (require 'package nil t)
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives '("ELPA" . "http://tromey.com/elpa/"))
    (package-initialize)))

;; auto-complete mode
(when (require 'auto-complete-config nil t)
  (add-to-list 'ac-dictionary-directories
	       "~/.emacs.d/elisp/auto-complete/ac-dict")
  (define-key ac-mode-map (kbd "M-TAB") 'auto-complete)
  (ac-config-default))

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
  (setq howm-view-use-grep t)
  ;(setq howm-view-grep-command "PATH")
  ;(setq howm-view-fgrep-command "PATH")
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
  ;; インデントスタイルを e style をデフォルトに
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
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

