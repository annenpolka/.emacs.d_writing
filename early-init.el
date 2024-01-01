;; Annenpolka early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq native-comp-async-report-warnings-errors nil)
;; suppress cl deprecation warnings
(setq byte-compile-warnings '(not cl-functions obsolete))
;; suppress ad-redefinition warnings
(setq ad-redefinition-action 'accept)

(setq
 gc-cons-threshold most-positive-fixnum
 gc-cons-percentage 0.5)

(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq use-dialog-box nil)

(setq frame-resize-pixelwise t)
(setq frame-inhibit-implied-resize t)
(setq inhibit-splash-screen t)


;;End: