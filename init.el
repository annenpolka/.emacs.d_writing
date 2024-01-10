;; Annenpolka init.el -*- lexical-binding: t; -*-

;; Install elpaca
(setq elpaca-core-date '(20231228))
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
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
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
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
  ;; Assume :elpaca t unless otherwise specified.
  (setq elpaca-use-package-by-default t))
(elpaca-wait)

;; :diminish support
(use-package diminish)
(elpaca-wait)

;; garbage collection
(use-package gcmh
  :ensure t
  :diminish gcmh-mode
  :hook (windows-startup-hook . gcmh-mode)
  :custom
  (gcmh-verbose . t)
  :config
  (setq gcmh-idle-delay 'auto  ; default is 15s
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024)))  ; 16mb

;; IME Patch
;; (use-package tr-ime
;;   :if (eq window-system 'w32)
;;   :config
;;   (tr-ime-advanced-install)
;;   (setq default-input-method "W32-IME")
;;   (modify-all-frames-parameters '((ime-font . "Migu 1P-12")))
;;   (w32-ime-initialize)
;;   ;; IME 制御（yes/no などの入力の時に IME を off にする）
;;   (wrap-function-to-control-ime 'universal-argument t nil)
;;   (wrap-function-to-control-ime 'read-string nil nil)
;;   (wrap-function-to-control-ime 'read-char nil nil)
;;   (wrap-function-to-control-ime 'read-from-minibuffer nil nil)
;;   (wrap-function-to-control-ime 'y-or-n-p nil nil)
;;   (wrap-function-to-control-ime 'yes-or-no-p nil nil)
;;   (wrap-function-to-control-ime 'map-y-or-n-p nil nil)
;;   (wrap-function-to-control-ime 'register-read-with-preview nil nil))

(defun install-package-with-scoop-if-not-exist (command package bucket)
  "コマンドがシステムに存在しない場合に、Scoopを使って指定したPACKAGEをインストールする。必要ならBUCKETを追加する。"
  (unless (executable-find command)
    (let ((search-result (shell-command-to-string (format "scoop search %s" package))))
      (when (string-match "No matches found" search-result)
        (shell-command (format "scoop bucket add %s" bucket)))
      (shell-command (format "scoop install %s" package)))))


(use-package mozc
  :demand t
  :bind*
  (("<zenkaku-hankaku>" . toggle-input-method)
  ("<eisu-toggle>" . toggle-input-method))
  :config
  (setq default-input-method "japanese-mozc-im")
  (setq mozc-candidate-style 'popup)
  (advice-add 'mozc-session-execute-command
              :after (lambda (&rest args)
                       (when (eq (nth 0 args) 'CreateSession)
                         (mozc-session-sendkey '(Hankaku/Zenkaku)))))
  )
(use-package mozc-im
  :after mozc)
(use-package mozc-popup
  :after mozc)


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
  :elpaca nil
  :ensure nil
  :bind (("M-ESC ESC" . c/redraw-frame))
  :init
  (defun c/redraw-frame nil
    (interactive)
    (redraw-frame))
  (defalias 'yes-or-no-p 'y-or-n-p)
  (define-key key-translation-map [?\C-h] [?\C-?])
  (global-set-key (kbd "C-?") 'help-for-help)
  (define-key input-decode-map [?\C-i] [C-i])
  (show-paren-mode 1)
  :config
  (setopt user-full-name "annenpolka"
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
        scroll-bar-mode nil
        indent-tabs-mode nil
        vc-follow-symlinks t
        select-enable-primary nil
        show-paren-style 'parenthesis
        show-paren-delay 0
        bookmark-watch-bookmark-file 'silent))

;; move-or-create-window functions
(use-package emacs
  :elpaca nil
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
  (windmove-right))
)

(use-package savehist
  :elpaca nil
  :init
  (savehist-mode 1)
  :config
  (setq savehist-coding-system 'utf-8-emacs))

;; japanese input method
(use-package mozc
  :demand t
  :bind*
  (("<zenkaku-hankaku>" . toggle-input-method)
  ("<eisu-toggle>" . toggle-input-method))
  :config
  (setq default-input-method "japanese-mozc-im")
  (setq mozc-candidate-style 'popup)
  (advice-add 'mozc-session-execute-command
              :after (lambda (&rest args)
                       (when (eq (nth 0 args) 'CreateSession)
                         (mozc-session-sendkey '(Hankaku/Zenkaku)))))
  )
(use-package mozc-im
  :after mozc)
(use-package mozc-popup
  :after mozc)
(use-package mozc-temp
  :bind
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

(elpaca-wait)

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
           :default-family "Iosevka Term"
           :default-weight regular
           :default-height 120
           :fixed-pitch-family nil ; falls back to :default-family
           :fixed-pitch-weight nil ; falls back to :default-weight
           :fixed-pitch-height 1.0
           :fixed-pitch-serif-family nil ; falls back to :default-family
           :fixed-pitch-serif-weight nil ; falls back to :default-weight
           :fixed-pitch-serif-height 1.0
           :variable-pitch-family "Iosevka"
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
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family "migu 1P")))
  (set-japanese-font-face)
  :hook
  ;; hook for daemon mode
  (server-after-make-frame-hook . set-japanese-font-face)
  :bind
  ("C-c F" . fontaine-set-preset))

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
;; ==============================
;; minibuffer/completion
;; ==============================


;; vertical completion ui
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

;; restore window layout
(use-package winner
  :elpaca nil
  :bind
  (("C-z" . winner-undo)
   ("C-S-z" . winner-redo))
  :config
  (winner-mode 1))

;; zoom focused window
(use-package zoom
  :config
  (setq zoom-size '(0.618 . 0.618)
        zoom-ignored-major-modes '(dirvish-mode))
  (zoom-mode 1))

;; scroll with cursor
(use-package centered-cursor-mode
  :ensure t
  :diminish centered-cursor-mode
  :config
  (global-centered-cursor-mode t)
  (setq ccm-step-size 2)
  ;; exclude on vterm
  (add-to-list 'ccm-ignored-commands 'vterm--self-insert))

(use-package meow
  :init
  (defun meow-setup ()
    (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
    (meow-motion-overwrite-define-key
     '("j" . meow-next)
     '("k" . meow-prev)
     '("<escape>" . ignore))
    (meow-leader-define-key
     ;; SPC j/k will run the original command in MOTION state.
     '("j" . "H-j")
     '("k" . "H-k")
     ;; Use SPC (0-9) for digit arguments.
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
     '(";" . meow-reverse)
     '("," . meow-inner-of-thing)
     '("." . meow-bounds-of-thing)
     '("[" . meow-beginning-of-thing)
     '("]" . meow-end-of-thing)
     '("a" . meow-append)
     '("A" . meow-open-below)
     '("b" . meow-back-word)
     '("B" . meow-back-symbol)
     '("c" . meow-change)
     '("d" . meow-delete)
     '("D" . meow-backward-delete)
     '("e" . meow-next-word)
     '("E" . meow-next-symbol)
     '("f" . meow-find)
     '("g" . meow-cancel-selection)
     '("G" . meow-grab)
     '("h" . meow-left)
     '("H" . meow-left-expand)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("j" . meow-next)
     '("J" . meow-next-expand)
     '("k" . meow-prev)
     '("K" . meow-prev-expand)
     '("l" . meow-right)
     '("L" . meow-right-expand)
     '("m" . meow-join)
     '("n" . meow-search)
     '("o" . meow-block)
     '("O" . meow-to-block)
     '("p" . meow-yank)
     '("q" . meow-quit)
     '("Q" . meow-goto-line)
     '("r" . meow-replace)
     '("R" . meow-swap-grab)
     '("s" . meow-kill)
     '("t" . meow-till)
     '("u" . meow-undo)
     '("U" . meow-undo-in-selection)
     '("v" . meow-visit)
     '("w" . meow-mark-word)
     '("W" . meow-mark-symbol)
     '("x" . meow-line)
     '("X" . meow-goto-line)
     '("y" . meow-save)
     '("Y" . meow-sync-grab)
     '("z" . meow-pop-selection)
     '("'" . repeat)
     '("<escape>" . ignore)))
  :config
  (meow-setup)
  (meow-global-mode 1))

;; better undo/redo
(use-package undo-fu
  :ensure t
  :bind
  ([remap undo] . undo-fu-only-undo)
  ([remap redo] . undo-fu-only-redo))
(use-package undo-fu-session
  :ensure t
  :config
  (global-undo-fu-session-mode 1))
;;End;
