;;; init.el --- My init.el  -*- lexical-binding: t; -*-

;; this enables this running method
;;   emacs -q -l ~/.debug.emacs.d/init.el
(eval-and-compile
  (when (or load-file-name byte-compile-current-file)
    (setq user-emacs-directory
          (expand-file-name
           (file-name-directory (or load-file-name byte-compile-current-file))))))

(eval-and-compile
  (customize-set-variable
   'package-archives '(("gnu"   . "https://elpa.gnu.org/packages/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("org"   . "https://orgmode.org/elpa/")))
  (package-initialize)
  (unless (package-installed-p 'leaf)
    (package-refresh-contents)
    (package-install 'leaf))

  (leaf leaf-keywords
    :ensure t
    :init
    ;; optional packages if you want to use :hydra, :el-get, :blackout,,,
    (leaf hydra :ensure t)
    (leaf el-get :ensure t)
    (leaf blackout :ensure t)

    :config
    ;; initialize leaf-keywords.el
    (leaf-keywords-init)))

(leaf cus-edit
  :doc "tools for customizing Emacs and Lisp packages"
  :tag "builtin" "faces" "help"
  :custom `((custom-file . ,(locate-user-emacs-file "custom.el"))))

(leaf cus-start
  :doc "define customization properties of builtins"
  :tag "builtin" "internal"
  :preface
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))

  :bind (("M-ESC ESC" . c/redraw-frame))
  :custom '((user-full-name . "Naruto TAKAHASHI")
            (user-mail-address . "tnaruto@gmail.com")
            (create-lockfiles . nil)
            (debug-on-error . t)
            (init-file-debug . t)
            (frame-resize-pixelwise . t)
            (enable-recursive-minibuffers . t)
            (history-length . 1000)
            (history-delete-duplicates . t)
            ;; (scroll-preserve-screen-position . t)
            ;; (scroll-conservatively . 100)
            (mouse-wheel-scroll-amount . '(1 ((control) . 5)))
            (ring-bell-function . 'ignore)
            (text-quoting-style . 'straight)
            (truncate-lines . t)
            ;; (use-dialog-box . nil)
            ;; (use-file-dialog . nil)
            ;; (menu-bar-mode . t)
            ;; (tool-bar-mode . nil)
            (scroll-bar-mode . nil)
            (indent-tabs-mode . nil))
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)
  (keyboard-translate ?\C-h ?\C-?))

(leaf better-defaults :ensure t)

(leaf autorevert
  :doc "revert buffers when files on disk change"
  :tag "builtin"
  :custom ((auto-revert-interval . 1))
  :global-minor-mode global-auto-revert-mode)

;; (leaf undohist :ensure t
;;   :config
;;   (setq undohist-ignored-files '("/tmp" "COMMIT_EDITMSG"))
;;   (undohist-initialize)
;;   )

;; efl-c-style
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
                                 (inclass . 3)))))
;; kernel c-style
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
(leaf cc-mode
  :doc "major mode for editing C and similar languages"
  :tag "builtin"
  :defvar (c-basic-offset)
  :bind (c-mode-base-map
         ("C-c c" . compile)
         )
  :config
  ;; switch source file and header file
  (add-hook 'c-initialization-hook (lambda ()
                                     (define-key c-mode-base-map
                                       (kbd "C-x C-o") 'ff-find-other-file)))
  (defcustom cc-search-directories
    '("." "/usr/include" "/usr/local/include/*")
    "*See the description of the `ff-search-directories' variable."
    :type '(repeat directory)
    :group 'ff)
  :mode-hook
  (c-mode-hook . ((c-set-style "bsd")
                  (setq c-basic-offset 4)))
  (c++-mode-hook . ((c-set-style "bsd")
                    (setq c-basic-offset 4))))
(leaf google-c-style :ensure t
  :config
  (add-hook 'c-mode-hook 'google-set-c-style)
  (add-hook 'c-mode-hook 'google-make-newline-indent)
  (add-hook 'c++-mode-hook 'google-set-c-style)
  (add-hook 'c++-mode-hook 'google-make-newline-indent)
  )

(leaf delsel
  :doc "delete selection if you insert"
  :tag "builtin"
  :global-minor-mode delete-selection-mode)

;; (leaf paren
;;   :doc "highlight matching paren"
;;   :tag "builtin"
;;   :custom ((show-paren-delay . 0.1))
;;   :global-minor-mode show-paren-mode)

;; (leaf simple
;;   :doc "basic editing commands for Emacs"
;;   :tag "builtin" "internal"
;;   :custom ((kill-ring-max . 100)
;;            (kill-read-only-ok . t)
;;            (kill-whole-line . t)
;;            (eval-expression-print-length . nil)
;;            (eval-expression-print-level . nil)))


(leaf auto-save-buffers-enhanced
  :doc "auto save buffers"
  :ensure t
  ;;; :bind (("\C-xas" . auto-save-buffers-enhanced-toggle-activity))
  :custom '(
      (make-backup-files . nil)
      (auto-save-default . nil)
      (auto-save-buffers-enhanced-include-regexps . '(".+")) ;;; all files
      (auto-save-buffers-enhanced-quiet-save-p . t) ;;; quiet Wrote message
  )
  :config
  (auto-save-buffers-enhanced t)
)

(leaf smartparens :ensure t
  :require smartparens-config
  :config
  ;; (sp-use-smartparens-bindings)
  ;; (smartparens-global-strict-mode t)
  (show-smartparens-global-mode t)
  (smartparens-global-mode t))

(leaf magit :ensure t)
(leaf editorconfig :ensure t)

;; major modes
(leaf markdown-mode :ensure t)
(leaf json-mode :ensure t)
(leaf typescript-mode :ensure t)
(leaf git :ensure t)
(leaf python-mode :ensure t)
(leaf csharp-mode :ensure t)
(leaf robe :ensure t)
(leaf swift-mode :ensure t)
(leaf kotlin-mode :ensure t)
(leaf rust-mode :ensure t)
(leaf go-mode :ensure t)
(leaf cmake-mode :ensure t)

(leaf multiple-cursors :ensure t
  :bind (
         ("C-S-c C-S-c" . mc/edit-lines)
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-c C-<" . mc/mark-all-like-this))
  )

(leaf company
  :doc "Modular text completion framework"
  :req "emacs-24.3"
  :tag "matching" "convenience" "abbrev" "emacs>=24.3"
  :url "http://company-mode.github.io/"
  :emacs>= 24.3
  :ensure t
  :blackout t
  :leaf-defer nil
  :bind ((company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-s" . company-filter-candidates)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("<tab>" . company-complete-selection))
         (company-search-map
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)))
  :custom ((company-idle-delay . 0)
           (company-minimum-prefix-length . 1)
           (company-transformers . '(company-sort-by-occurrence)))
  :global-minor-mode global-company-mode)

(leaf company-c-headers
  :doc "Company mode backend for C/C++ header files"
  :req "emacs-24.1" "company-0.8"
  :tag "company" "development" "emacs>=24.1"
  :added "2020-03-25"
  :emacs>= 24.1
  :ensure t
  :after company
  :defvar company-backends
  :config
  (add-to-list 'company-backends 'company-c-headers))

(leaf ivy
  :doc "Incremental Vertical completYon"
  :req "emacs-24.5"
  :tag "matching" "emacs>=24.5"
  :url "https://github.com/abo-abo/swiper"
  :emacs>= 24.5
  :ensure t
  :blackout t
  :leaf-defer nil
  :custom ((ivy-initial-inputs-alist . nil)
           (ivy-use-selectable-prompt . t))
  :global-minor-mode t
  :config
  (leaf swiper
    :doc "Isearch with an overview. Oh, man!"
    :req "emacs-24.5" "ivy-0.13.0"
    :tag "matching" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :bind (("C-s" . swiper)))

  (leaf counsel
    :doc "Various completion functions using Ivy"
    :req "emacs-24.5" "swiper-0.13.0"
    :tag "tools" "matching" "convenience" "emacs>=24.5"
    :url "https://github.com/abo-abo/swiper"
    :emacs>= 24.5
    :ensure t
    :blackout t
    :bind (("C-S-s" . counsel-imenu)
           ("C-x C-r" . counsel-recentf))
    :custom `((counsel-yank-pop-separator . "\n----------\n")
              (counsel-find-file-ignore-regexp . ,(rx-to-string '(or "./" "../") 'no-group)))
    :global-minor-mode t)

  )

(leaf prescient
  :doc "Better sorting and filtering"
  :req "emacs-25.1"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :custom ((prescient-aggressive-file-save . t))
  :global-minor-mode prescient-persist-mode)
  
(leaf ivy-prescient
  :doc "prescient.el + Ivy"
  :req "emacs-25.1" "prescient-4.0" "ivy-0.11.0"
  :tag "extensions" "emacs>=25.1"
  :url "https://github.com/raxod502/prescient.el"
  :emacs>= 25.1
  :ensure t
  :after prescient ivy
  :custom ((ivy-prescient-retain-classic-highlighting . t))
  :global-minor-mode t)

(leaf counsel-gtags :ensure t
  :after ivy counsel cc-mode
  :config
  (counsel-gtags-mode 1)
  (add-hook 'c-mode-hook 'counsel-gtags-mode)
  (add-hook 'c++-mode-hook 'counsel-gtags-mode)
  (with-eval-after-load 'counsel-gtags
    (define-key counsel-gtags-mode-map (kbd "M-t") 'counsel-gtags-find-definition)
    (define-key counsel-gtags-mode-map (kbd "M-r") 'counsel-gtags-find-reference)
    (define-key counsel-gtags-mode-map (kbd "M-s") 'counsel-gtags-find-symbol)
    (define-key counsel-gtags-mode-map (kbd "M-,") 'counsel-gtags-go-backward))
  )

(provide 'init)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(blackout el-get hydra leaf-keywords leaf)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Local Variables:
;; indent-tabs-mode: nil
;; End:
