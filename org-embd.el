;;; org-embd.el --- Embed various content in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: multimedia, hypermedia, outlines
;; Version: 0.1
;; Package-Requires: ((emacs "25.1") (org "9.0"))

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This package provides functionality to embed various content
;; in org-mode using xwidget-webkit, including videos, webpages, and PDF files.
;;
;; Usage:
;;   M-x org-embd-video - Embed a video from various platforms or local file
;;   M-x org-embd-webpage - Embed any webpage
;;   M-x org-embd-pdf - Embed a PDF file
;;
;; Supported video platforms:
;;   - YouTube
;;   - Bilibili
;;   - Vimeo
;;   - Dailymotion
;;   - TED Talks
;;   - Local video files
;; Supported PDF functionality:
;;   - Embedding PDF files from URLs or local files
;; This is the main entry point for the package, which loads the core
;; functionality and the default rules.

;;; Code:

(require 'org)
(require 'org-embd-core)
(require 'org-embd-rules)

;; Register the org-embd link type with org-mode
(org-link-set-parameters "org-embd"
                         :follow #'org-embd-follow-link
                         :face '(:foreground "green" :underline t))

;; Define interactive commands for cleaning xwidgets
(defun org-embd-clean-all ()
  "Clean up all xwidgets in the current buffer."
  (interactive)
  (org-embd-clean-xwidgets))

(defun org-embd-clean-at-point ()
  "Clean up the xwidget at or after point."
  (interactive)
  (org-embd-clean-xwidget-at-point))

(provide 'org-embd)
;;; org-embd.el ends here
