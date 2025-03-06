;;; org-embd-rules.el --- Rules for embedding content in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: multimedia, hypermedia, outlines
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file provides rules for embedding various types of content
;; in org-mode using the org-embd framework.

;;; Code:

(require 'org-embd-core)

;; YouTube rules
(defun org-embd-youtube-matcher (url)
  "Check if URL is a YouTube video."
  (string-match "\\(?:youtube\\.com/watch\\?v=\\|youtu\\.be/\\)\\([^&?]*\\)" url))

(defun org-embd-youtube-handler (url width height)
  "Generate HTML for embedding YouTube video from URL with WIDTH and HEIGHT."
  (let* ((id (progn
               (string-match "\\(?:youtube\\.com/watch\\?v=\\|youtu\\.be/\\)\\([^&?]*\\)" url)
               (match-string 1 url))))
    (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>YouTube Video</title>
</head>
<body style=\"margin: 0; padding: 0;\">
  <iframe width=\"%d\" height=\"%d\" 
    src=\"https://www.youtube.com/embed/%s\" 
    frameborder=\"0\" allowfullscreen>
  </iframe>
</body>
</html>" width height id)))

;; Bilibili rules
(defun org-embd-bilibili-matcher (url)
  "Check if URL is a Bilibili video."
  (string-match "\\(?:bilibili\\.com/video/\\)\\(BV[0-9a-zA-Z]+\\)" url))

(defun org-embd-bilibili-handler (url width height)
  "Generate HTML for embedding Bilibili video from URL with WIDTH and HEIGHT."
  (let* ((id (progn
               (string-match "\\(?:bilibili\\.com/video/\\)\\(BV[0-9a-zA-Z]+\\)" url)
               (match-string 1 url))))
    (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>Bilibili Video</title>
</head>
<body style=\"margin: 0; padding: 0;\">
  <iframe width=\"%d\" height=\"%d\" 
    src=\"https://player.bilibili.com/player.html?bvid=%s&page=1\" 
    frameborder=\"0\" allowfullscreen>
  </iframe>
</body>
</html>" width height id)))

;; Vimeo rules
(defun org-embd-vimeo-matcher (url)
  "Check if URL is a Vimeo video."
  (string-match "\\(?:vimeo\\.com/\\)\\([0-9]+\\)" url))

(defun org-embd-vimeo-handler (url width height)
  "Generate HTML for embedding Vimeo video from URL with WIDTH and HEIGHT."
  (let* ((id (progn
               (string-match "\\(?:vimeo\\.com/\\)\\([0-9]+\\)" url)
               (match-string 1 url))))
    (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>Vimeo Video</title>
</head>
<body style=\"margin: 0; padding: 0;\">
  <iframe width=\"%d\" height=\"%d\" 
    src=\"https://player.vimeo.com/video/%s\" 
    frameborder=\"0\" allowfullscreen>
  </iframe>
</body>
</html>" width height id)))

;; Dailymotion rules
(defun org-embd-dailymotion-matcher (url)
  "Check if URL is a Dailymotion video."
  (string-match "\\(?:dailymotion\\.com/video/\\)\\([a-zA-Z0-9]+\\)" url))

(defun org-embd-dailymotion-handler (url width height)
  "Generate HTML for embedding Dailymotion video from URL with WIDTH and HEIGHT."
  (let* ((id (progn
               (string-match "\\(?:dailymotion\\.com/video/\\)\\([a-zA-Z0-9]+\\)" url)
               (match-string 1 url))))
    (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>Dailymotion Video</title>
</head>
<body style=\"margin: 0; padding: 0;\">
  <iframe width=\"%d\" height=\"%d\" 
    src=\"https://www.dailymotion.com/embed/video/%s\" 
    frameborder=\"0\" allowfullscreen>
  </iframe>
</body>
</html>" width height id)))

;; TED rules
(defun org-embd-ted-matcher (url)
  "Check if URL is a TED talk."
  (string-match "\\(?:ted\\.com/talks/\\)\\([a-zA-Z0-9_]+\\)" url))

(defun org-embd-ted-handler (url width height)
  "Generate HTML for embedding TED talk from URL with WIDTH and HEIGHT."
  (let* ((id (progn
               (string-match "\\(?:ted\\.com/talks/\\)\\([a-zA-Z0-9_]+\\)" url)
               (match-string 1 url))))
    (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>TED Talk</title>
</head>
<body style=\"margin: 0; padding: 0;\">
  <iframe width=\"%d\" height=\"%d\" 
    src=\"https://embed.ted.com/talks/%s\" 
    frameborder=\"0\" allowfullscreen>
  </iframe>
</body>
</html>" width height id)))

;; Local video file rules
(defun org-embd-local-video-matcher (url)
  "Check if URL is a local video file."
  (or (string-prefix-p "/" url)
      (string-prefix-p "file://" url)
      (string-match "\\.[a-zA-Z0-9]+$" url)))

(defun org-embd-local-video-handler (url width height)
  "Generate HTML for embedding local video file from URL with WIDTH and HEIGHT."
  (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>Video Player</title>
</head>
<body style=\"margin: 0; padding: 0;\">
  <video width=\"%d\" height=\"%d\" controls>
    <source src=\"%s\" type=\"video/mp4\">
    Not Supported.
  </video>
</body>
</html>" width height url))

;; PDF file rules
(defun org-embd-pdf-matcher (url)
  "Check if URL is a PDF file."
  (or (string-match "\\.pdf$" url)
      (and (string-match "^https?://" url)
           (string-match "\\.pdf" url))))

(defun org-embd-pdf-handler (url width height)
  "Generate HTML for embedding PDF file from URL with WIDTH and HEIGHT."
  (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>PDF Viewer</title>
  <style>
    body, html { margin: 0; padding: 0; height: 100%%; overflow: hidden; }
    #pdf-container { width: 100%%; height: 100%%; }
  </style>
</head>
<body>
  <object id=\"pdf-container\" width=\"%d\" height=\"%d\" 
    data=\"%s\" type=\"application/pdf\">
    <p>It appears your browser doesn't support embedded PDFs. 
    <a href=\"%s\">Click here to download the PDF</a>.</p>
  </object>
</body>
</html>" width height url url))



;; Webpage rules
(defun org-embd-webpage-matcher (url)
  "Check if URL is a webpage."
  (string-match "^https?://" url))

(defun org-embd-webpage-handler (url width height)
  "Generate HTML for embedding webpage from URL with WIDTH and HEIGHT.
Returns nil to allow direct URL opening."
  nil)

;; Register all rules
(defun org-embd-register-default-rules ()
  "Register all default embedding rules."
  (org-embd-register-rule 'youtube 
                          #'org-embd-youtube-matcher 
                          #'org-embd-youtube-handler)
  
  (org-embd-register-rule 'bilibili 
                          #'org-embd-bilibili-matcher 
                          #'org-embd-bilibili-handler)
  
  (org-embd-register-rule 'vimeo 
                          #'org-embd-vimeo-matcher 
                          #'org-embd-vimeo-handler)
  
  (org-embd-register-rule 'dailymotion 
                          #'org-embd-dailymotion-matcher 
                          #'org-embd-dailymotion-handler)
  
  (org-embd-register-rule 'ted 
                          #'org-embd-ted-matcher 
                          #'org-embd-ted-handler)
  
  (org-embd-register-rule 'local-video 
                          #'org-embd-local-video-matcher 
                          #'org-embd-local-video-handler)
  
  (org-embd-register-rule 'pdf
                          #'org-embd-pdf-matcher
                          #'org-embd-pdf-handler)
  

  
  (org-embd-register-rule 'webpage
                          #'org-embd-webpage-matcher
                          #'org-embd-webpage-handler))

;; Register default rules when this file is loaded
(org-embd-register-default-rules)

(provide 'org-embd-rules)
;;; org-embd-rules.el ends here
