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
  :config
  (setopt user-full-name "annenpolka"
        user-mail-address "lancelbb@gmail.com"
        user-login-name "annenpolka"
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
;;End;
