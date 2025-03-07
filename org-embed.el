;;; org-embed.el --- Embed various content in org-mode -*- lexical-binding: t; -*-

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
;;   M-x org-embed-video - Embed a video from various platforms or local file
;;   M-x org-embed-webpage - Embed any webpage
;;   M-x org-embed-pdf - Embed a PDF file
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
(require 'org-embed-core)
(require 'org-embed-rules)

;; Register the org-embed link type with org-mode
(org-link-set-parameters "org-embed"
                         :follow #'org-embed-follow-link
                         :face '(:foreground "green" :underline t))

;; Define interactive commands for cleaning xwidgets
(defun org-embed-clean-all ()
  "Clean up all xwidgets in the current buffer."
  (interactive)
  (org-embed-clean-xwidgets))

(defun org-embed-clean-at-point ()
  "Clean up the xwidget at or after point."
  (interactive)
  (org-embed-clean-xwidget-at-point))

;; Define follow link function
(defun org-embed-follow-link (link)
  "Follow an org-embed LINK.
Based on the link type, call the appropriate embedding function."
  (if (string-match "^\\([^:]+\\):\\(.*\\)" link)
      (let ((type (match-string 1 link))
            (url (match-string 2 link)))
        (cond ((string= type "video") (org-embed-video url nil nil nil))
              ((string= type "local-video") (org-embed-local-video url nil nil nil))
              ((string= type "webpage") (org-embed-webpage url nil nil nil))
              ((string= type "pdf") (org-embed-pdf url nil nil nil))
              (t (message "Unknown org-embed link type: %s" type))))
    (message "Invalid org-embed link format: %s" link)))

(provide 'org-embed)
;;; org-embed.el ends here
