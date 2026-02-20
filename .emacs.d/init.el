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

(leaf corfu
  :doc "COmpletion in Region FUnction"
  :ensure t
  :global-minor-mode global-corfu-mode
  :custom ((corfu-auto . t)
           (corfu-cycle . t)
           (corfu-quit-at-boundary . t)
           (corfu-quit-no-match . t)
           (corfu-preselect . 'prompt))
  :config
  ;; Enable indentation+completion using the TAB key.
  (setq tab-always-indent 'completion))

(leaf cape
  :doc "Completion At Point Extensions"
  :ensure t
  :init
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(leaf which-key
  :doc "Display available keybindings in popup"
  :ensure t
  :global-minor-mode t)

(leaf embark
  :doc "Contextual actions for candidates"
  :ensure t
  :bind (("C-." . embark-act)         ;; pick some comfortable binding
         ("C-;" . embark-dwim)        ;; good for quick actions
         ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings`
  :init
  ;; Option: minibuffer-completion-help is often redundant with Vertico
  (setq prefix-help-command #'embark-prefix-help-command))

(leaf helpful
  :doc "A better help buffer"
  :ensure t
  :bind (("C-h f" . helpful-callable)
         ("C-h v" . helpful-variable)
         ("C-h k" . helpful-key)
         ("C-h x" . helpful-command)
         ("C-c C-d" . helpful-at-point)))

(leaf doom-themes
  :doc "A collection of user-submitted themes"
  :ensure t
  :config
  (load-theme 'doom-one t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(leaf doom-modeline
  :doc "A fancy and fast mode-line"
  :ensure t
  :global-minor-mode t
  :custom ((doom-modeline-buffer-file-name-style . 'truncate-with-project)
           (doom-modeline-icon . t)
           (doom-modeline-major-mode-icon . t)))

(leaf gcmh
  :doc "Garbage Collector Magic Hack"
  :ensure t
  :global-minor-mode gcmh-mode)

(leaf vundo
  :doc "Visual undo tree"
  :ensure t
  :bind (("C-x u" . vundo)))

(leaf nerd-icons-dired
  :doc "Icons for dired"
  :ensure t
  :hook (dired-mode-hook . nerd-icons-dired-mode))

(leaf savehist
  :doc "persist history"
  :tag "builtin"
  :global-minor-mode t)

(leaf vertico
  :doc "VERTical Interactive COmpletion"
  :ensure t
  :global-minor-mode t
  :custom ((vertico-count . 20)
           (vertico-cycle . t)))

(leaf orderless
  :doc "completion style for matching regexps"
  :ensure t
  :custom ((completion-styles . '(orderless basic))
           (completion-category-overrides . '((file (styles basic partial-completion))))))

(leaf marginalia
  :doc "Enrich existing commands with completion annotations"
  :ensure t
  :global-minor-mode t)

(leaf consult
  :doc "Consulting completing-read"
  :ensure t
  :bind (("C-s" . consult-line)
         ("C-S-s" . consult-imenu)
         ("C-x C-r" . consult-recent-file)
         ("M-y" . consult-yank-pop)
         ("C-x b" . consult-buffer)
         ;; Add project-wide search
         ("M-s r" . consult-ripgrep)
         ("M-s g" . consult-grep))
  :custom ((consult-project-root-function .
            (lambda ()
              (when-let (project (project-current))
                (project-root project))))))

(leaf projectile :ensure t
  :require projectile
  :global-minor-mode t
  :config
  (with-eval-after-load 'projectile
    ;; Recommended keymap prefix on macOS
    (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
    (setq projectile-completion-system 'default)
    )
  )

(leaf consult-projectile
  :doc "Consult integration for projectile"
  :ensure t
  :after consult projectile)

(provide 'init)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:
