;; Annenpolka init.el -*- lexical-binding: t; -*-

;; ==============================
;; Package Management/initialization
;; ==============================
;; Install elpaca
(setq elpaca-core-date '(20240303))
(setq elpaca-menu-functions '(elpaca-menu-extensions elpaca-menu-gnu-devel-elpa))

(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable :elpaca use-package keyword.
  (elpaca-use-package-mode)
  ;; Assume :ensure t unless otherwise specified.
  (setq use-package-always-ensure t)
  ;; limit queue number for too many open files error
  (setq elpaca-queue-limit 10))
(elpaca-wait)

;; :diminish support
(use-package diminish)

;; hydra dependency
(use-package hydra)
(use-package pretty-hydra)


;; double-key binding support
(use-package key-chord
  :config
  (setq key-chord-two-keys-delay 0.08
        key-chord-one-keys-delay 0.2)
  (key-chord-mode 1))

;; consitent custom file paths
(use-package no-littering
  :init
  (setq no-littering-etc-directory
        (expand-file-name "config/" user-emacs-directory))
  (setq no-littering-var-directory
        (expand-file-name "data/" user-emacs-directory))
  (setq auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t))))
(elpaca-wait)

(defun install-package-with-scoop (command package bucket)
  "コマンドがシステムに存在しない場合に、Scoopを使って指定したPACKAGEをインストールする。必要ならBUCKETを追加する。"
  (when IS-WINDOWS
    (unless (executable-find command)
      (let ((search-result (shell-command-to-string (format "scoop search %s" package))))
        (when (string-match "No matches found" search-result)
          (shell-command (format "scoop bucket add %s" bucket)))
        (shell-command (format "scoop install %s" package))))))

(defun get-scoop-app-path (app-name)
  "Scoopのappsディレクトリで指定されたアプリ名を検証し、存在すればそのパスを返す。"
  (let* ((scoop-env (getenv "SCOOP"))
         (scoop-path (if scoop-env scoop-env (expand-file-name "~/scoop")))
         (app-path (expand-file-name (concat "apps/" app-name) scoop-path)))
    (when (file-directory-p app-path)
      app-path)))

(defun my-get-auth-info (service keyword)
  "特定のサービスに対する認証情報を `auth-source` から取得する関数。
   もし見つからない場合はユーザーに入力を求め、`auth-sources` に基づいてファイルに書き出す。"
  (let ((match (car (auth-source-search :host service :require `(,keyword))))
        (source-file (car auth-sources)))
    (if match
        ;; 認証情報が見つかった場合
        (funcall (plist-get match keyword))
      ;; 認証情報が見つからなかった場合、ユーザーに入力を求める
      (let ((new-value (read-passwd (format "Enter your %s for %s: " keyword service))))
        (when source-file
          ;; ファイルに書き出す
          (with-temp-buffer
            (insert "machine " service " " (substring (symbol-name keyword) 1) " " new-value "\n")
            (if (string-suffix-p ".gpg" source-file)
                (epa-encrypt-file (buffer-string) source-file)
              (append-to-file (point-min) (point-max) source-file))))
        new-value))))

;; set builtin configs
(use-package emacs
  :ensure nil
  :bind* (("M-ESC ESC" . c/redraw-frame)
	  ("C-w" . 'backward-kill-word))
  :init
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-key key-translation-map [?\C-h] [?\C-?])
  (global-set-key (kbd "C-?") 'help-for-help)
  (define-key input-decode-map [?\C-i] [C-i])
  (show-paren-mode 1)
  (global-display-line-numbers-mode 1)
  (setq user-full-name "annenpolka"
        user-mail-address "lancelbb@gmail.com"
        user-login-name "annenpolka"
        default-directory "~/"
        backup-directory-alist '((".*" . "~/.backup"))
        create-lockfiles nil
        debug-on-error nil
        init-file-debug nil
        frame-resize-pixelwise t
        enable-recursive-minibuffers t
        history-length 1000
        history-delete-duplicates t
        scroll-preserve-screen-position t
        scroll-conservatively 100
        mouse-wheel-scroll-amount '(1 ((control) . 5))
        ring-bell-function 'ignore
        text-quoting-style 'straight
        truncate-lines t
        completion-cycle-threshold 3
        tab-always-indent 'complete
        default-tab-width 4
        scroll-bar-mode nil
        indent-tabs-mode nil
        vc-follow-symlinks t
        vc-handled-backends '(Git SVN)
        show-paren-style 'parenthesis
        show-paren-delay 0
        bookmark-watch-bookmark-file 'silent)
  (when IS-WINDOWS
    ;; use cp932 for windows processes
    (setq default-process-coding-system '(utf-8-dos . cp932))
    )
  )

(use-package recentf
  :ensure nil
  :init
  (setq recentf-save-file "~/.emacs.d/recentf"
        recentf-max-saved-items 2000
        recentf-auto-cleanup 'never
	recentf-keep          '(file-remote-p file-readable-p))
  :config
  (setq recentf-exclude '("recentf"
                          "COMMIT_EDITMSG"
                          "bookmarks"
                          "\\.gitignore"
                          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\)$"
                          "^/tmp/"
                          "^/scp:"
                          "~/.emacs.d/straight/.*"
                          "~/howm/.*"
                          (lambda (file) (file-in-directory-p file package-user-dir))))
  (recentf-mode 1))

;; save final place on each file
(use-package saveplace
  :ensure nil
  :config
  (save-place-mode 1))

;; "revert buffers when files on disk change"
(use-package autorevert
  :ensure nil
  :custom ((auto-revert-interval 1))
  :init
  (global-auto-revert-mode 1))

;; move-or-create-window functions
(use-package emacs
  :ensure nil
  :init
  (defun move-or-create-window-above nil
    "Move to the window above the current one, or create a new split if none exists."
    (interactive)
    (unless (window-in-direction 'above)
      (split-window-below))
    (windmove-up))

  (defun move-or-create-window-below nil
    "Move to the window below the current one, or create a new split if none exists."
    (interactive)
    (unless (window-in-direction 'below)
      (split-window-vertically))
    (windmove-down))

  (defun move-or-create-window-left nil
    "Move to the window to the left of the current one, or create a new split if none exists."
    (interactive)
    (unless (window-in-direction 'left)
      (split-window-right))
    (windmove-left))

  (defun move-or-create-window-right nil
    "Move to the window to the right of the current one, or create a new split if none exists."
    (interactive)
    (unless (window-in-direction 'right)
      (split-window-horizontally))
    (windmove-right)))

(use-package savehist
  :ensure nil
  :init
  (savehist-mode 1)
  :config
  (setq savehist-coding-system 'utf-8-emacs))

;; garbage collection
(use-package gcmh
  :diminish gcmh-mode
  :hook (windows-startup-hook . gcmh-mode)
  :config
  (setq gcmh-verbose t
        gcmh-idle-delay 'auto  ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024)))  ; 16mb

;; ==============================
;; IME
;; ==============================

;; IME Patch
(use-package tr-ime
  :if (when IS-WINDOWS)
  :hook
  (w32-ime-on-hook . (lambda() (key-chord-mode 1)))
  (w32-ime-off-hook . (lambda() (key-chord-mode 1)))
  :config
  (tr-ime-advanced-install)
  (setq default-input-method "W32-IME")
  (modify-all-frames-parameters '((ime-font . "Migu 1P-12")))
  (w32-ime-initialize)
  ;; IME 制御（yes/no などの入力の時に IME を off にする）
  (wrap-function-to-control-ime 'universal-argument t nil)
  (wrap-function-to-control-ime 'read-string nil nil)
  (wrap-function-to-control-ime 'read-char nil nil)
  (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
  (wrap-function-to-control-ime 'y-or-n-p nil nil)
  (wrap-function-to-control-ime 'yes-or-no-p nil nil)
  (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
  (wrap-function-to-control-ime 'register-read-with-preview nil nil))

;; japanese input method
(use-package mozc
  :if (when IS-LINUX)
  :demand t
  :bind*
  (("<zenkaku-hankaku>" . toggle-input-method)
   ("<eisu-toggle>" . toggle-input-method))
  :hook
  (input-method-deactivate-hook . (lambda() (key-chord-mode 1)))
  :config
  (setq default-input-method "japanese-mozc-im")
  ;; (setq mozc-candidate-style 'popup)
  ;; workaround for win
  (advice-add 'mozc-session-execute-command
	      :after (lambda (&rest args)
		       (when (eq (nth 0 args) 'CreateSession)
                         (mozc-session-sendkey '(Hankaku/Zenkaku))))))

(use-package mozc-im
  :after mozc)
(use-package mozc-popup
  :after mozc)
;; (use-package mozc-cand-posframe
;;   :after mozc
;;   :config
;;    (setq mozc-candidate-style 'posframe))
(use-package mozc-temp
  :bind*
  ("C-j" . mozc-temp-convert-dwim)
  :after mozc)

;; gpt-based ime
;; (use-package sumibi
;;   :custom
;;   (sumibi-current-model "gpt-4-1106-preview")
;;   (sumibi-model-list '("gpt-3.5-turbo" "gpt-4-1106-preview"))
;;   :init
;;   (setq emacs-minor-version 1)
;;   (setenv "OPENAI_API_KEY" (my-get-auth-info "api.openai.com" :secret))
;;   :config
;;   (global-sumibi-mode 1))

;; romaji library
(use-package migemo
  :if (executable-find "cmigemo")
  :init
  (let ((scoop-path (and IS-WINDOWS
                         (executable-find "scoop")
                         (concat (getenv "USERPROFILE") "/scoop/apps/cmigemo/current/cmigemo-default-win64/dict/utf-8/migemo-dict")))
        (default-path (cond (IS-WINDOWS
                             (concat (file-name-directory (executable-find "cmigemo")) "dict/utf-8/migemo-dict"))
                            (IS-LINUX
                             "/usr/share/cmigemo/utf-8/migemo-dict"))))
    (setq migemo-dictionary (or scoop-path default-path)))
  :config
  (when (file-exists-p migemo-dictionary)
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "-e"))
    (setq migemo-coding-system 'utf-8-unix)
    (migemo-init)))

;; ==============================
;; UI
;; ==============================

(use-package fontaine
  :init
  (setq fontaine-presets
        '((tiny
           :default-height 70)
          (small
           :default-height 90)
          (regular
           :default-height 120)
          (medium
           :default-height 130)
          (large
           :default-weight semilight
           :default-height 140
           :bold-weight extrabold)
          (presentation
           :default-weight semilight
           :default-height 170
           :bold-weight extrabold)
          (jumbo
           :default-weight semilight
           :default-height 220
           :bold-weight extrabold)
          (t ; shared fallback properties
           ;; I keep all properties for didactic purposes, but most can be
           ;; omitted.  See the fontaine manual for the technicalities:
           ;; <https://protesilaos.com/emacs/fontaine>.
           :default-family "PlemolJP Console NF"
           :default-weight regular
           :default-height 120
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "PlemolJP Console NF"
           :variable-pitch-weight nil
           :variable-pitch-height 1.0
           :bold-family nil ; use whatever the underlying face has
           :bold-weight bold
           :italic-family nil
           :italic-slant italic
           :line-spacing nil)))
  ;; Recover last preset or fall back to desired style from
  ;; `fontaine-presets'.
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

  ;; The other side of `fontaine-restore-latest-preset'.
  (add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

  ;; set japanese font manually
  (defun set-japanese-font-face nil
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family "migu 1M")))
  ;; (set-japanese-font-face)
  ;; :hook
  ;; hook for daemon mode
  ;; (server-after-make-frame-hook . set-japanese-font-face)
  :bind
  ("C-c F" . fontaine-set-preset))

;; emoji configured
(use-package emojify
  :hook (after-init . global-emojify-mode)
  :config
  ;; 絵文字フォントをsetするリスト
  (let ((fonts '("Segoe UI Emoji" "Noto Color Emoji")))
    (dolist (font fonts)
      (when (member font (font-family-list))
	(set-fontset-font
	 t 'symbol (font-spec :family font) nil 'prepend))))
  :custom
  (emojify-display-style 'unicode))

;; variable-pitch-mode workaround
(use-package mixed-pitch
  :hook
  ;; If you want it in all text modes:
  (text-mode . mixed-pitch-mode))

;; theme
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-molokai t)
  ;; Enable flashing mode-line on errors
  ;; (doom-themes-visual-bell-config)
  ;; ;; Enable custom neotree theme (all-the-icons must be installed!)
  ;; (doom-themes-neotree-config)
  ;; ;; or for treemacs users
  ;; (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  ;; (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package whitespace
  :ensure nil
  :diminish whitespace-mode
  :init
  (setq whitespace-style '(face           ; faceで可視化
                           trailing       ; 行末
                           tabs           ; タブ
                           spaces         ; スペース
                           empty          ; 先頭/末尾の空行
                           space-mark     ; 表示のマッピング
                           tab-mark
                           ))

  (setq whitespace-display-mappings
	'((space-mark ?\u3000 [?\u25a1])
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))

  ;; スペースは全角のみを可視化
  (setq whitespace-space-regexp "\\(\u3000+\\)")

  (global-whitespace-mode 1)

  (defvar my/bg-color "#232323")
  (set-face-attribute 'whitespace-trailing nil
		      :background my/bg-color
		      :foreground "DeepPink"
		      :underline t)
  (set-face-attribute 'whitespace-tab nil
		      :background my/bg-color
		      :foreground "LightSkyBlue"
		      :underline t)
  (set-face-attribute 'whitespace-space nil
		      :background my/bg-color
		      :foreground "GreenYellow"
		      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
		      :background my/bg-color)
  )

;; modeline
(use-package moody
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode)
  (moody-replace-eldoc-minibuffer-message-function))

;; window highlighting
(use-package solaire-mode
  :init
  (solaire-global-mode))

(use-package highlight-indent-guides
  :hook ((prog-mode . highlight-indent-guides-mode)
         (yaml-mode . highlight-indent-guides-mode))
  :diminish highlight-indent-guides-mode
  :config
  (setq highlight-indent-guides-method 'fill
        highlight-indent-guides-auto-enabled t
        highlight-indent-guides-responsive "stack"))

(use-package rainbow-identifiers
  :hook
  (prog-mode . rainbow-identifiers-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode org-mode) . rainbow-delimiters-mode))
;; ==============================
;; Major Modes
;; ==============================
;; lisp
(use-package parinfer-rust-mode
  :disabled
  :hook (emacs-lisp-mode . parinfer-rust-mode)
  :init
  (setq parinfer-rust-auto-download t
        parinfer-rust-library-directory (no-littering-expand-var-file-name "parinfer-rust/")))

;;markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . gfm-mode)
  :hook ((markdown-mode . turn-off-auto-fill)
	 (markdown-mode . visual-line-mode))
  :init
  (use-package word-wrap-mode
    :ensure nil
    :hook (visual-line-mode . word-wrap-whitespace-mode)
    :config
    (add-to-list 'word-wrap-whitespace-characters ?\]))

  (use-package visual-fill-column
    :hook (visual-line-mode . visual-fill-column-mode)
    :init
    (setq visual-line-fringe-indicators '(left-curly-arrow nil))
    :config
    (setq visual-fill-column-width 78))

  (use-package adaptive-wrap
    :hook (visual-line-mode . adaptive-wrap-prefix-mode))

  (setq markdown-command "pandoc --from=markdown --to=html5"
        markdown-fontify-code-blocks-natively t
        markdown-header-scaling t
        markdown-hide-url t
        markdown-hide-markup nil
	markdown-list-indent-width 4
        markdown-indent-on-enter 'indent-and-new-item)
  :bind (:map markdown-mode-map
	      ("<S-tab>" . markdown-shifttab)))
;; ==============================
;; minibuffer
;; ==============================
;; vertical completion minibuffer ui
(use-package vertico
  :init
  (setq vertico-cycle t)
  :config
  (vertico-mode t)
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args))))

;; Enable rich annotations using the Marginalia package
(use-package marginalia
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
	      ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

(use-package embark
  :bind*
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim))        ;; good alternative: M-.
  :bind
  ("C-h B" . embark-bindings) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  ;; Show the Embark target at point via Eldoc. You may adjust the
  ;; Eldoc strategy, if you want to see the documentation from
  ;; multiple providers. Beware that using this can be a little
  ;; jarring since the message shown in the minibuffer can be more
  ;; than one line, causing the modeline to move up and down:

  ;; (add-hook 'eldoc-documentation-functions #'embark-eldoc-first-target)
  ;; (setq eldoc-documentation-strategy #'eldoc-documentation-compose-eagerly)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; complition-styles
(use-package orderless
  :config
  (icomplete-mode)
  (defun orderless-migemo (component)
    (let ((pattern (migemo-get-pattern component)))
      (condition-case nil
          (progn (string-match-p pattern "") pattern)
        (invalid-regexp nil))))

  (orderless-define-completion-style orderless-default-style
    (orderless-matching-styles '(orderless-initialism
				 orderless-literal
                                 orderless-regexp)))

  (orderless-define-completion-style orderless-migemo-style
    (orderless-matching-styles '(orderless-initialism
				 orderless-literal
                                 orderless-regexp
                                 orderless-migemo)))

  (orderless-define-completion-style orderless-fuzzy-style
    (orderless-matching-styles '(orderless-initialism
				 orderless-literal
				 orderless-flex
                                 orderless-regexp
                                 orderless-migemo)))
  (setq completion-category-overrides
        '((command (styles orderless-default-style))
          (file (styles orderless-migemo-style))
          (buffer (styles orderless-migemo-style))
          (symbol (styles orderless-default-style))
          (consult-location (styles orderless-migemo-style)) ; category `consult-location' は `consult-line' などに使われる
          (consult-multi (styles orderless-migemo-style)) ; category `consult-multi' は `consult-buffer' などに使われる
          (org-roam-node (styles orderless-migemo-style)) ; category `org-roam-node' は `org-roam-node-find' で使われる
          (howm-fuzzy (styles orderless-fuzzy-style))
          (unicode-name (styles orderless-migemo-style))
          (variable (styles orderless-default-style))))

  ;;
  (add-to-list 'marginalia-prompt-categories '("\\<Keyword\\>" . howm-fuzzy))
  ;; (setq orderless-matching-styles '(orderless-literal orderless-regexp orderless-migemo))
  :custom
  (completion-styles '(orderless basic))
  )

;; ==============================
;; suggestion/autocompletion
;; =============================
(use-package corfu
  ;; Optional customizations
  :custom
  (corfu-cycle t)           ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt) ;; Always preselect the prompt
  (corfu-auto t)
  (corfu-auto-delay 0.01)
  (corfu-auto-prefix 3)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  (tab-always-indent 'complete)
  ;; Use TAB for cycling, default is `corfu-complete'.
  :bind
  (:map corfu-map
	("TAB" . corfu-next)
	([tab] . corfu-next)
	("S-TAB" . corfu-previous)
	([backtab] . corfu-previous))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :hook
  (prog-mode . corfu-mode))

;; Add extensions
(use-package cape
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :bind (("C-c p p" . completion-at-point) ;; capf
         ("C-c p t" . complete-tag)        ;; etags
         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :hook
  (((prog-mode
     text-mode) . my/set-super-capf))
  :init
  (defun my/set-super-capf (&optional arg)
    (setq-local completion-at-point-functions
                (list (cape-capf-noninterruptible
		       (cape-capf-buster
                        (cape-capf-properties
                         (cape-capf-super
                          (if arg
			      arg
                            (car completion-at-point-functions))
                          ;; #'tempel-complete
                          ;; #'tabnine-completion-at-point
                          #'cape-dabbrev
                          #'cape-file)
                         :sort t
                         :exclusive 'no))))))
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  )
(use-package prescient
  :config
  (setq prescient-aggressive-file-save t)
  (prescient-persist-mode +1))

(use-package corfu-prescient
  :after corfu
  :config
  (with-eval-after-load 'orderless
    (setq corfu-prescient-enable-filtering nil))
  (corfu-prescient-mode +1))

;; ==============================
;; Linting/Formatting
;; ==============================

;; formatter
(use-package apheleia
  :diminish apheleia-mode
  :config
  (apheleia-global-mode t))

;; ==============================
;; Editor
;; ==============================

;; save on idle
(use-package super-save
  :diminish super-save-mode
  :custom
  (super-save-auto-save-when-idle t)
  :config
  ;; add integration with ace-window
  (add-to-list 'super-save-triggers 'ace-window)
  ;; save on find-file
  (add-to-list 'super-save-hook-triggers 'find-file-hook)
  (super-save-mode +1))

;; show key guides
(use-package which-key
  :diminish which-key-mode
  :custom (which-key-idle-delay 0.25)
  :config
  (which-key-setup-side-window-right-bottom)
  (which-key-mode t))

;; restore window layout
(use-package winner
  :ensure nil
  :bind
  (("C-z" . winner-undo)
   ("C-S-z" . winner-redo))
  :config
  (winner-mode 1))

;; window switching
(use-package ace-window)

;; zoom focused window
(use-package zoom
  :diminish
  :config
  (setq zoom-size '(0.525 . 0.525)
        zoom-ignored-major-modes '(dirvish-mode))
  (zoom-mode 1))

;; scroll with cursor
(use-package centered-cursor-mode
  :diminish centered-cursor-mode
  :config
  (global-centered-cursor-mode t)
  (setq ccm-step-size 2)
  ;; exclude on vterm
  (add-to-list 'ccm-ignored-commands 'vterm--self-insert))

;; workspace
(use-package perspective
  :bind
  ;; ("C-x C-b" . persp-list-buffers)         ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c p"))  ; pick your own prefix key here
  :init
  (persp-mode))

(use-package activities
  :disabled
  :ensure (activities :host github :repo "alphapapa/activities.el")
  :config
  (activities-mode)
  (activities-tabs-mode)
  ;; Prevent `edebug' default bindings from interfering.
  (setq edebug-inhibit-emacs-lisp-mode-bindings t)

  :bind
  (("C-x C-a C-n" . activities-new)
   ;; As resuming is expected to be one of the most commonly used
   ;; commands, this binding is one of the easiest to press.
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ;; This binding mirrors, e.g. "C-x t RET".
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list)))

;; zen writing mode
(use-package writeroom-mode)

;; search/narrow
(use-package consult
  :init
  ;; *で始まるバッファ名を候補から削除 -> たとえば*scratch*に変えられない副作用
  (setq ido-ignore-buffers (append '("\\`\\*") ido-ignore-buffers))

  :custom
  (consult-buffer-sources
   '
   (
    ;; persp-consult-source
    consult--source-buffer
    consult--source-hidden-buffer
    consult--source-recent-file))
  )
;; dir extension
(use-package consult-dir
  :after consult
  :bind
  (("C-x C-d" . consult-dir)
   (:map vertico-map
	 ("C-x C-d" . consult-dir)
	 ("C-x C-j" . consult-dir-jump-file)))
  :config)
;; (setq consult-dir-project-list-function #'consult-dir-projectile-dirs)

;; autocomplete/delete parens
(use-package puni
  :defer t
  :init
  (electric-pair-mode 1)
  ;; The autoloads of Puni are set up so you can enable `puni-mode` or
  ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
  ;; any key that calls Puni commands, it's loaded.
  (puni-global-mode)
  (add-hook 'term-mode-hook #'puni-disable-puni-mode))

;; non-lsp xref source
(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read))

;; better undo/redo
(use-package undo-fu
  :ensure t
  :bind
  ([remap undo] . undo-fu-only-undo)
  ([remap redo] . undo-fu-only-redo))
(use-package undo-fu-session
  :disabled t ; HELP: too slow on save
  :ensure t
  :config
  (global-undo-fu-session-mode 1))

;; better surround
(use-package embrace
  :config
  ;; wikilink
  (lambda nil (embrace-add-pair ?l "\\[\\[" "\\]\\]"))
  )

;; vim-like historical locate navigation
(use-package backward-forward
  :ensure t
  :init
  ;; reference: https://emacs-china.org/t/emacs/19171/17
  (defun my/backward-forward-previous-location ()
    "A `backward-forward-previous-location' wrap for skip invalid locations."
    (interactive)
    (let ((purge (< backward-forward-mark-ring-traversal-position (1- (length backward-forward-mark-ring))))
          (recent (point-marker)))
      (backward-forward-previous-location)
      (when (and (equal recent (point-marker)) purge)
        (my/backward-forward-previous-location))))

  (defun my/backward-forward-next-location ()
    "A `backward-forward-next-location' wrap for skip invalid locations."
    (interactive)
    (let ((purge (> backward-forward-mark-ring-traversal-position 0))
          (recent (point-marker)))
      (backward-forward-next-location)
      (when (and (equal recent (point-marker)) purge)
        (my/backward-forward-next-location))))
  :config
  (setq backward-forward-mark-ring-max 100)
  (backward-forward-mode 1))

;; blockman-like block highlighting
(use-package hl-block-mode
  :commands (hl-block-mode)
  :config
  (setq hl-block-color-tint "#010101"
	hl-block-delay 0.1
	hl-block-single-level nil)
  :hook ((prog-mode) . hl-block-mode))

(use-package meow
  :init
  ;; command functions
  (defun meow-save-line nil
    (interactive)
    (meow-line 1)
    (call-interactively #'meow-save))
  (defun meow-insert-at-first-non-whitespace nil
    (interactive)
    (back-to-indentation)
    (meow-insert))
  (defun meow-insert-at-end-of-line nil
    (interactive)
    (move-end-of-line 1)
    (meow-insert))
  (defun meow-find-backward nil
    (interactive)
    (let ((current-prefix-arg -1))
      (call-interactively 'meow-find)))
  (defun meow-search-backward nil
    (interactive)
    (let ((current-prefix-arg -1))
      (call-interactively 'meow-search)))
  (defun meow-close-window-or-buffer ()
    (interactive)
    (if (one-window-p)
	(kill-this-buffer)
      (delete-window)))

  ;; setup keymap
  (defun meow-setup nil
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("C-u" . ccm-scroll-down)
     '("C-d" . ccm-scroll-up)
     '("C-o" . my/backward-forward-previous-location)
     '("<C-i>" . my/backward-forward-next-location)
     '("C-w" . meow-close-window-or-buffer)
     '("/" . consult-line)
     '("<escape>" . ignore))
    (meow-leader-define-key
     '("j" . "H-j")
     '("k" . "H-k")
     '("C-u" . "H-C-u")
     '("C-d" . "H-C-d")
     '("C-o" . "H-C-o")
     '("<C-i>" . "H-C-i")
     '("C-w" . "H-C-w")
     '("/" . "H-/")
     '("r e" . restart-emacs)
     '("w j" . move-or-create-window-below)
     '("w k" . move-or-create-window-above)
     '("w h" . move-or-create-window-left)
     '("w l" . move-or-create-window-right)
     '("w w" . window-swap-states)
     '("f" . "s-f")
     '("v" . my-git-actions/body)
     '("z g" . flyspell-correct-at-point)
     '("l" . "s-l")
     '("p" . "C-c M-p")
     '("K" . helpful-at-point)
     '("SPC" . consult-buffer)
     '("s f" . affe-find)
     '("s p" . affe-grep)
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("9" . meow-expand-9)
     '("8" . meow-expand-8)
     '("7" . meow-expand-7)
     '("6" . meow-expand-6)
     '("5" . meow-expand-5)
     '("4" . meow-expand-4)
     '("3" . meow-expand-3)
     '("2" . meow-expand-2)
     '("1" . meow-expand-1)
     '("-" . negative-argument)
     '("=" . meow-query-replace)
     '("+" . meow-stellar-replace-regexp)
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-insert-at-end-of-line)
     '("o" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     ;; '("d" . meow-delete)
     '("d" . meow-kill)
     '("D" . kill-line)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("F" . avy-goto-char-timer)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-insert-at-first-non-whitespace)
     '("O" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("N" . meow-search-backward)
     '("(" . meow-block)
     '(")" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . repeat)
     ;; '("s" . meow-meow)
     '("s" . embrace-commander)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . undo-redo)
     '("C-r" . undo-redo)
     ;; '("v" . meow-visit)
     '("v" . er/expand-region)
     '("V" . er/contract-region)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("\"" . consult-yank-pop)
     '("/" . consult-line)
     '("C-u" . ccm-scroll-down)
     '("C-d" . ccm-scroll-up)
     '("C-o" . my/backward-forward-previous-location)
     '("<C-i>" . my/backward-forward-next-location)
     '("C-j" . move-or-create-window-below)
     '("C-k" . move-or-create-window-above)
     '("DEL" . move-or-create-window-left) ; C-h translated to DEL
     '("C-l" . move-or-create-window-right)
     '("C-f" . consult-line)
     ;; '("C-t" . burly-perspective-init-project-persp)
     '("C-s" . save-buffer)
     '("C-w" . meow-close-window-or-buffer)
     ;;  (cons "S-SPC" kurumi-utility-map)
     '("C-s" . save-buffer)
     '("<escape>" . ignore))
    ;; readline-style keymap in global map
    (bind-key "C-w" 'backward-kill-word)
    ;; remap universal-argument
    (bind-key "C-M-u" 'universal-argument)
    ;; insert state keymap
    (meow-define-keys
	'insert
      '("C-g" . meow-insert-exit))
    ;; key-chord shortcuts
    (key-chord-define meow-insert-state-keymap "jk" 'meow-insert-exit)
    (key-chord-define meow-normal-state-keymap "gd" 'xref-find-definitions))

  :hook
  (meow-insert-exit . (lambda nil (deactivate-input-method)))
  (meow-insert-exit . (lambda nil (call-interactively 'corfu-quit)))
  (meow-insert-exit . (lambda nil (key-chord-mode 1)))
  :config
  (setq meow-use-clipboard t
	meow-keypad-self-insert-undefined nil
	meow-mode-state-list '((helpful-mode . normal)
			       (help-mode . normal)
			       (Man-mode . normal)
			       (eww-mode . normal)
			       (devdocs-mode . normal)
			       (howm-menu-mode . motion)
			       (howm-view-summary-mode . motion)
			       (vterm-mode . insert)
			       (eshell-mode . insert))
	meow--kbd-forward-char "<right>"
	;; (meow--kbd-forward-line . "<down>")
	;; (meow--kbd-backward-line . "<up>")
	meow--kbd-delete-char "<deletechar>"
	meow--kbd-kill-region "S-<delete>"
	meow--kbd-kill-line "<deleteline>"
	meow-selection-command-fallback '((meow-reverse . back-to-indentation)
                                          (meow-change . meow-change-char)
                                          (meow-save . meow-save-line)
                                          (meow-kill . meow-delete)
                                          ;;  (meow-kill . meow-kill-whole-line)
                                          (meow-pop-selection . meow-pop-grab)
                                          (meow-beacon-change . meow-beacon-change-char)
                                          (meow-cancel . keyboard-quit)
                                          (meow-delete . meow-C-d))
	meow-char-thing-table '((?r . round)
				(?\( . round)
				(?b . anyblock) ;; `b' for bracket
				(?c . curly)
				(?s . string)
				(?\" . string)
				(?e . symbol)
				(?w . window)
				(?B . buffer)
				(?g . buffer)
				(?p . paragraph)
				(?\[ . square)
				(?l . line)
				(?d . defun)
				(?. . sentence)))
  (meow-setup)
  (meow-global-mode 1))
;; ==============================
;; Git
;; ==============================

(use-package magit
  :ensure t
  :bind (:map magit-status-mode-map
	      ("p" . magit-pull)
	      ("x" . magit-delete-thing)))
(use-package forge
  :after magit
  :disabled
  ;; :if (unless IS-WINDOWS) ; forge doesn't work on windows
  :custom
  (bug-reference-mode 0)
  (forge-add-default-bindings t))

(use-package git-gutter
  :diminish git-gutter-mode
  :custom
  (git-gutter:ask-p nil)
  :init
  (global-git-gutter-mode))

(use-package git-auto-commit-mode
  :hook
  (howm-mode . git-auto-commit-mode)
  :custom
  (gac-silent-message-p t)
  (gac-automatically-push-p t))

;; ==============================
;; notetaking/writing
;; ==============================

  (use-package howm
  :init
  ;; (define-key global-map [katakana] 'howm-menu) ; [カタカナ] キーでメニュー
  (setq howm-file-name-format "%Y/%m/%Y-%m-%d.md") ; 1 日 1 ファイル
  (setq howm-keyword-case-fold-search t) ; <<< で大文字小文字を区別しない
  (setq howm-list-title t) ; 一覧時にタイトルを表示
  (setq howm-history-file "~/howm/.howm-history")
  (setq howm-keyword-file "~/howm/.howm-keys")
  ;; (setq howm-menu-refresh-after-save nil) ; save 時にメニューを自動更新せず
  ;; (setq howm-refresh-after-save nil) ; save 時に下線を引き直さない
  ;; (setq howm-menu-expiry-hours 2) ; メニューを 2 時間キャッシュ

  ;; Use ripgrep as grep
  (setq howm-view-use-grep t)
  (setq howm-view-grep-command "rg")
  (setq howm-view-grep-option "-nH --no-heading --color never")
  (setq howm-view-grep-extended-option nil)
  (setq howm-view-grep-fixed-option "-F")
  (setq howm-view-grep-expr-option nil)
  (setq howm-view-grep-file-stdin-option nil)

  :config
  (define-key howm-menu-mode-map "\C-h" nil)
  (define-key riffle-summary-mode-map "\C-h" nil)
  (define-key howm-view-contents-mode-map "\C-h" nil)
  )

;; ==============================
;; External Services/Integrations
;; ==============================

(use-package atomic-chrome
  :config
  (atomic-chrome-start-server))

;;End;


(put 'howm-narrow-to-memo 'disabled nil)
