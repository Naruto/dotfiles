; user info
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

;; theme
(add-to-list 'custom-theme-load-path
             (expand-file-name (concat user-emacs-directory "themes")))
(load-theme 'zenburn t)

(when (and (eq (window-system) 'x)
	 (>= emacs-major-version 23))

  ;; disable toolbar
  (cond ((eq emacs-major-version 24) (tool-bar-mode 0))
        (t (tool-bar-mode nil)))

  (require 'xclip)

  ;(set-background-color "#efefdf");background
  ;(set-foreground-color "#202041");foreground
  ;(set-cursor-color "#202041");cursor

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
(show-paren-mode 1)
(setq show-paren-delay 0)
;(iswitchb-mode)
(require 'dired-x)
(iimage-mode)

(autoload 'ansi-color-for-comint-mode-on "ansi-color"
  "Set `ansi-color-for-comint-mode' to t." t)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; auto-install 
(setq enable-quelpa nil)
(when (require 'auto-install nil t)
  (setq auto-install-directory "~/.emacs.d/elisp/")
  (auto-install-update-emacswiki-package-name t)
  ;; (setq url-proxy-services '(("http" . "HOST:PORT")))
  (auto-install-compatibility-setup))

;; package.el for ELPA
(when (>= emacs-major-version 23)
  (when (require 'package nil t)
    (package-initialize)
    (add-to-list 'package-archives
                 '("gnu" . "http://elpa.gnu.org/packages/"))
    (add-to-list 'package-archives
                 '("marmalade" . "http://marmalade-repo.org/packages/"))
    (add-to-list 'package-archives
                 '("ELPA" . "http://tromey.com/elpa/"))
    (add-to-list 'package-archives
                 '("melpa . "http://melpa.milkbox.net/packages/))

    (when (require 'quelpa nil t)
        (quelpa-self-upgrade)
      (with-temp-buffer
        (url-insert-file-contents "https://raw.github.com/quelpa/quelpa/master/bootstrap.el")
        (eval-buffer))
      (setq enable-quelpa t)
      )
     ))

;; dash
(when enable-quelpa
  (quelpa 'dash)
  (require 'dash)
  )
;; s
(when enable-quelpa
  (quelpa 's)
  (require 's)
  )

;; auto-save-buffers-enhanced
(when enable-quelpa
  (quelpa 'auto-save-buffers-enhanced)
  (setq make-backup-files nil)
  (setq auto-save-default nil)
  (setq auto-save-buffers-enhanced-include-regexps '(".+")) ;all files
  (setq auto-save-buffers-enhanced-quiet-save-p t)  ;;; quiet Wrote messsage.
  (auto-save-buffers-enhanced t)
  ;; toggle auto-save-buffers-enhanced
  (global-set-key "\C-xas" 'auto-save-buffers-enhanced-toggle-activity)
)

;; yasnippet
(when enable-quelpa
  (quelpa 'yasnippet)
  (require 'yasnippet nil t)
  (setq yas-snippet-dirs
        '(
          "~/.emacs.d/snippets"
          "~/.emacs.d/public_repos/yasnippet/snippets"
          ))
  (setq yas/use-menu nil)
  (yas-global-mode 1)
  (defun reload-snippets ()
    (interactive)
    (yas-reload-all)
    (yas-recompile-all)
    (yas-reload-all)
    (yas-recompile-all)
    )
  (defun snippet-mode-before-save ()
    (interactive)
    (when (eq major-mode 'snippet-mode) (reload-snippets)))
  (add-hook 'after-save-hook 'snippet-mode-before-save)
)

;; fuzzy
(when enable-quelpa
  (quelpa 'fuzzy)
  )

(when enable-quelpa
  (quelpa 'popup)
  )

;; auto-complete mode
(when enable-quelpa
  (quelpa 'auto-complete)
  (require 'cl)
  (require 'auto-complete-config)
  (add-to-list 'ac-dictionary-directories
               (concat user-emacs-directory
                       "quelpa/build/auto-complete/dict/")
               (concat user-emacs-directory
                       "ac-dict"))
  (ac-config-default)

  (quelpa 'auto-complete-clang-async)
  (require 'auto-complete-clang-async)

  ;; Select candidates with C-n/C-p only when completion menu is displayed:
  (setq ac-use-menu-map t)
  (define-key ac-menu-map "C-n" 'ac-next)
  (define-key ac-menu-map "C-p" 'ac-previous)

  ;; auto-complete common setting
  (setq ac-quick-help-delay 0.5)
  (setq ac-dwim t)
  (setq ac-candidate-limit 100) 
  (setq ac-auto-start nil)
  (setq ac-auto-show-menu t)
  (setq ac-quick-help-delay 0)
  (setq ac-use-fuzzy 1.5)
  (setq ac-show-menu-immediately-on-auto-complete t)
  (setq ac-expand-on-auto-complete nil)
  (setq ac-quick-help-height 20)
  (setq ac-menu-height 20)
  (ac-set-trigger-key "TAB")
  (define-key ac-mode-map  [(control tab)] 'auto-complete)
  
  (defun my-ac-cc-mode-setup ()
    (setq ac-sources  '(ac-source-clang-async
                        ac-source-gtags))
    (setq ac-clang-complete-executable 
          (concat user-emacs-directory "bin/clang-complete"))
    (setq clang-completion-suppress-error 't)

    ;; echo "" | g++ -v -x c++ -E -
    (cond ((string= "gnu/linux" system-type)
           (setq ac-clang-flags
                 (mapcar (lambda (item)(concat "-I" item))
                         (split-string
                          "
 /usr/include/c++/4.8
 /usr/include/x86_64-linux-gnu/c++/4.8
 /usr/include/c++/4.8/backward
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include
 /usr/local/include
 /usr/lib/gcc/x86_64-linux-gnu/4.8/include-fixed
 /usr/include/x86_64-linux-gnu
 /usr/include
"
                          )))
           )
          ((string= "darwin" system-type)
           
           )
          (t t))

    (setq ac-clang-cflags (append '("-std=c++11") ac-clang-cflags)) ;; c++11
    (ac-clang-launch-completion-process)
    )

  (defun my-ac-config ()
    ;(define-key ac-complete-mode-map "M-n" 'ac-next)
    ;(define-key ac-complete-mode-map "M-p" 'ac-previous)
    (setq ac-auto-start nil)
    (setq-default ac-sources
                  '(ac-source-abbrev
                    ac-source-dictionary
                    ac-source-words-in-same-mode-buffers
                    ac-source-filename
                    ;;ac-source-yasnippet
                    ))
    (add-hook 'c++-mode-hook 'my-ac-cc-mode-setup)
    (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
    (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
    (add-hook 'css-mode-hook 'ac-css-mode-setup)
    (add-hook 'auto-complete-mode-hook 'ac-common-setup)
    (add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
    (global-auto-complete-mode t)
    (auto-complete-mode t)
    )

  (my-ac-config)
)

;; ;; Undohist
;; (when enable-quelpa
;;   (quelpa 'undohist)
;;   (undohist-initialize))

;; howm
(setq howm-menu-lang 'ja)
(setq howm-process-coding-system 'utf-8-unix)
(when (require 'howm nil t)
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
;;(cua-mode t)
;;(setq cua-enable-cua-keys nil)

;; psvn
(when (executable-find "svn")
  (setq svn-status-verbose nil)
  (autoload 'svn-status"psvn" "Run `svn status'." t))

;; multi-term
(when (require 'multi-term nil t)
  )

;; elscren
(when enable-quelpa
  (quelpa 'elscreen)
  (require 'elscreen nil t)
  (require 'elscreen-howm nil t)
  (require 'elscreen-gf nil t)
  (elscreen-start)
  (setq elscreen-tab-display-kill-screen nil)
  (setq elscreen-tab-display-control nil)
  )

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
        '((c-mode . "e")))
  ;; set google code style at c++-mode
  (require 'google-c-style)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-make-newline-indent)

  ;; switch source file and header file
  ;(global-set-key (kbd "C-x C-o") 'ff-find-other-file)
  (define-key c-mode-base-map (kbd "C-x C-o") 'ff-find-other-file)
  (defcustom cc-search-directories
    '("." "/usr/include" "/usr/local/include/*")
    "*See the description of the `ff-search-directories' variable."
    :type '(repeat directory)
    :group 'ff)
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

;; objective-c mode
(add-to-list 'auto-mode-alist '("\\.mm?$" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@implementation" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@interface" . objc-mode))
(add-to-list 'magic-mode-alist '("\\(.\\|\n\\)*\n@protocol" . objc-mode))
(setq ff-other-file-alist
      '(("\\.mm?$" (".h"))
        ("\\.cc$"  (".hh" ".h"))
        ("\\.hh$"  (".cc" ".C"))

        ("\\.c$"   (".h"))
        ("\\.h$"   (".c" ".cc" ".C" ".CC" ".cxx" ".cpp" ".m" ".mm"))

        ("\\.C$"   (".H"  ".hh" ".h"))
        ("\\.H$"   (".C"  ".CC"))

        ("\\.CC$"  (".HH" ".H"  ".hh" ".h"))
        ("\\.HH$"  (".CC"))

        ("\\.cxx$" (".hh" ".h"))
        ("\\.cpp$" (".hpp" ".hh" ".h"))

        ("\\.hpp$" (".cpp" ".c"))))
(add-hook 'objc-mode-hook
          (lambda ()
            (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
            ))

;; migemo
;; http://qiita.com/catatsuy/items/c5fa34ead92d496b8a51
(when (and (executable-find "cmigemo")
           (require 'migemo nil t))
  (setq migemo-options '("-q" "--emacs"))

  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (setq migemo-command "cmigemo")
  (cond
   ((string-match "apple-darwin" system-configuration)
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict"))
   (t
    (setq migemo-dictionary "/usr/share/cmigemo/utf-8/migemo-dict")))
  (load-library "migemo")
  (migemo-init)
)

;; emacs helm
(when enable-quelpa
  ;;  (quelpa 'helm)
  ;; (require 'helm-config)
  ;; (global-set-key (kbd "C-c h") 'helm-mini)
  ;; (custom-set-variables '(helm-ff-auto-update-initial-value nil))
  ;; ;; helm commands
  ;; (define-key global-map (kbd "M-x")     'helm-M-x)
  ;; (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  ;; (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  ;; (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  ;; (define-key global-map (kbd "C-c i")   'helm-imenu)
  ;; (define-key global-map (kbd "C-x b")   'helm-buffers-list)

  ;; ;; Emulate `kill-line' in helm minibuffer
  ;; (setq helm-delete-minibuffer-contents-from-point t)
  ;; (defadvice helm-delete-minibuffer-contents (before helm-emulate-kill-line activate)
  ;;   "Emulate `kill-line' in helm minibuffer"
  ;;   (kill-new (buffer-substring (point) (field-end))))
  ;; ;; ;; For find-file etc.
  ;; ;; (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
  ;; ;; ;; For helm-find-files etc.
  ;; ;; (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)

  ;; (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
  ;;   "Execute command only if CANDIDATE exists"
  ;;   (when (file-exists-p candidate)
  ;;     ad-do-it))

  ;; ;; helm-ag
  ;; (require 'helm-ag)
  ;; (setq helm-ag-base-command "ag --nocolor --nogroup --ignore-case")
  ;; (setq helm-ag-command-option "--all-text")
  ;; (setq helm-ag-thing-at-point 'symbol)
  ;; (global-set-key (kbd "M-g .") 'helm-ag)
  ;; (global-set-key (kbd "M-g ,") 'helm-ag-pop-stack)
  ;; (global-set-key (kbd "C-M-s") 'helm-ag-this-file)
)

;; ack-and-a-half
(when enable-quelpa
  (quelpa 'ack-and-a-half)
  (require 'ack-and-a-half)
  ;; Create shorter aliases
  (defalias 'ack 'ack-and-a-half)
  (defalias 'ack-same 'ack-and-a-half-same)
  (defalias 'ack-find-file 'ack-and-a-half-find-file)
  (defalias 'ack-find-file-same 'ack-and-a-half-find-file-same)
)

;; ag.el
(when enable-quelpa
  (quelpa 'ag)
  (require 'ag nil t)
  ;; (setq ag-highlight-search t)
)


;; cmake
(when enable-quelpa
  (quelpa 'cmake-mode)
  (require 'cmake-mode nil t)
  (setq auto-mode-alist
        (append '(("CMakeLists\\.txt\\'" . cmake-mode)
                  ("\\.cmake\\'" . cmake-mode))
                auto-mode-alist))
)

;;; cask
;(when (file-exists-p "~/.cask/cask.el")
;  (require 'cask "~/.cask/cask.el")
;  (cask-initialize)
;)

;; mark-multiple
(when enable-quelpa
  (quelpa 'mark-multiple)
  (require 'inline-string-rectangle)
  (global-set-key (kbd "C-x r t") 'inline-string-rectangle)

  (require 'mark-more-like-this)
  (global-set-key (kbd "C-<") 'mark-previous-like-this)
  (global-set-key (kbd "C->") 'mark-next-like-this)
  (global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
  (global-set-key (kbd "C-*") 'mark-all-like-this)

  (add-hook 'sgml-mode-hook
            (lambda ()
              (require 'rename-sgml-tag)
              (define-key sgml-mode-map (kbd "C-c C-r") 'rename-sgml-tag)))
  )

;; expand-region
;(when (file-exists-p
;       (expand-file-name (concat user-emacs-directory "public_repos/expand-region")))
;  (require 'expand-region)
;  (global-set-key (kbd "C-@") 'er/expand-region)
;  (global-set-key (kbd "C-M-@") 'er/contract-region)
;  (transient-mark-mode t)
;  )

;; git-modes
(when enable-quelpa
  (quelpa 'git)
  (require 'git-commit-mode)
  (require 'git-rebase-mode)
)

;; magit
(when enable-quelpa
  (quelpa 'magit)
  (require 'magit)
  )

;; epl
(when enable-quelpa
  (quelpa 'epl)
  (require 'epl)
  )

;; smartparens
(when enable-quelpa
  (quelpa 'smartparens)
  (require 'smartparens)
  (require 'smartparens-config)
  (smartparens-global-mode t)
  )

;; projectile
(when enable-quelpa
  (quelpa 'projectile)
  (require 'projectile)
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  ;; https://github.com/abesto/dotfiles/blob/master/.emacs.d/my-projectile.el
  (defun projectile-ag ()
    "Run an `ag' search in the project"
    (interactive)
    (let ((search-regexp (if (and transient-mark-mode mark-active)
                             (buffer-substring (region-beginning) (region-end))
                           (read-string (projectile-prepend-project-name "Ag for: ") (thing-at-point 'symbol))))
          (root-dir (expand-file-name (projectile-project-root))))
      (ag/search search-regexp root-dir)))

  (setq projectile-show-paths-function 'projectile-hashify-with-relative-paths)
  (global-set-key '[f1] 'helm-projectile)
  (global-set-key '[f2] 'projectile-ag)
                                        ; (global-set-key "\C-xb" 'helm-mini)
)

;; grizzl
(when enable-quelpa
  (quelpa 'grizzl)
  (require 'grizzl)
  ; (setq projectile-completion-system 'grizzl)
)

;; undo-tree
(when enable-quelpa
  (quelpa 'undo-tree)
  (require 'undo-tree)
  (global-undo-tree-mode)
  (global-set-key (kbd "M-/") 'undo-tree-redo)
  )

;; glsl mode
(when enable-quelpa
  (quelpa 'glsl-mode)
  (autoload 'glsl-mode "glsl-mode" nil t)
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.vert\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.frag\\'" . glsl-mode))
  (add-to-list 'auto-mode-alist '("\\.geom\\'" . glsl-mode))
)


;; ;; ace-jump-mode
;; (when (file-exists-p
;;        (expand-file-name (concat user-emacs-directory "public_repos/ace-jump-mode")))
;;   (autoload
;;     'ace-jump-mode
;;     "ace-jump-mode"
;;     "Emacs quick move minor mode"
;;     t)
;;   ;; you can select the key you prefer to
;;   (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

;;   ;; 
;;   ;; enable a more powerful jump back function from ace jump mode
;;   ;;
;;   (autoload
;;     'ace-jump-mode-pop-mark
;;     "ace-jump-mode"
;;     "Ace jump back:-)"
;;     t)
;;   (eval-after-load "ace-jump-mode"
;;     '(ace-jump-mode-enable-mark-sync))
;;   (define-key global-map (kbd "C-x SPC") 'ace-jump-mode-pop-mark)
;;   )

;; ;; helm-swoop
;; (when (file-exists-p
;;        (expand-file-name (concat user-emacs-directory "public_repos/helm-swoop")))
;;   (require 'helm-swoop)
;;   ;; Change the keybinds to whatever you like :)
;;   (global-set-key (kbd "M-i") 'helm-swoop)
;;   (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;;   (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;;   (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

;;   ;; When doing isearch, hand the word over to helm-swoop
;;   (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
;;   ;; From helm-swoop to helm-multi-swoop-all
;;   (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
;;   ;; When doing evil-search, hand the word over to helm-swoop
;;   ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

;;   ;; Save buffer when helm-multi-swoop-edit complete
;;   (setq helm-multi-swoop-edit-save t)

;;   ;; If this value is t, split window inside the current window
;;   (setq helm-swoop-split-with-multiple-windows nil)

;;   ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
;;   (setq helm-swoop-split-direction 'split-window-vertically)

;;   ;; If nil, you can slightly boost invoke speed in exchange for text color
;;   (setq helm-swoop-speed-or-color nil)

;;   ;; ;; Go to the opposite side of line from the end or beginning of line
;;   (setq helm-swoop-move-to-line-cycle t)

;;   ;; Optional face for line numbers
;;   ;; Face name is `helm-swoop-line-number-face`
;;   (setq helm-swoop-use-line-number-face t)
;; )



;; ;; Create Header Guards with f12
;; (global-set-key [f12] 
;;   		'(lambda () 
;;   		   (interactive)
;;   		   (if (buffer-file-name)
;;   		       (let*
;;   			   ((fName (upcase (file-name-nondirectory (file-name-sans-extension buffer-file-name))))
;;   			    (ifDef (concat "#ifndef " fName "_H" "\n#define " fName "_H" "\n"))
;;   			    (begin (point-marker))
;;   			    )
;;   			 (progn
;;   					; If less then 5 characters are in the buffer, insert the class definition
;;   			   (if (< (- (point-max) (point-min)) 5 )
;;   			       (progn
;;   				 (insert "\nclass " (capitalize fName) "{\npublic:\n\nprivate:\n\n};\n")
;;   				 (goto-char (point-min))
;;   				 (next-line-nomark 3)
;;   				 (setq begin (point-marker))
;;   				 )
;;   			     )
  			   
;;   					;Insert the Header Guard
;;   			   (goto-char (point-min))
;;   			   (insert ifDef)
;;   			   (goto-char (point-max))
;;   			   (insert "\n#endif" " //" fName "_H")
;;   			   (goto-char begin))
;;   			 )
;;                                         ;else
;;   		     (message (concat "Buffer " (buffer-name) " must have a filename"))
;;   		     )
;;   		   )
;;   		)

