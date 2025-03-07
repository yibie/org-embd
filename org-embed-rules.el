;;; org-embed-rules.el --- Rules for embedding content in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: multimedia, hypermedia, outlines
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file provides rules for embedding various types of content
;; in org-mode using the org-embed framework.

;;; Code:

(require 'org-embed-core)

;; YouTube rules
(defun org-embed-youtube-matcher (url)
  "Check if URL is a YouTube video."
  (string-match "\\(?:youtube\\.com/watch\\?v=\\|youtu\\.be/\\)\\([^&?]*\\)" url))

(defun org-embed-youtube-handler (url width height)
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
(defun org-embed-bilibili-matcher (url)
  "Check if URL is a Bilibili video."
  (string-match "\\(?:bilibili\\.com/video/\\)\\(BV[0-9a-zA-Z]+\\)" url))

(defun org-embed-bilibili-handler (url width height)
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
(defun org-embed-vimeo-matcher (url)
  "Check if URL is a Vimeo video."
  (string-match "\\(?:vimeo\\.com/\\)\\([0-9]+\\)" url))

(defun org-embed-vimeo-handler (url width height)
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
(defun org-embed-dailymotion-matcher (url)
  "Check if URL is a Dailymotion video."
  (string-match "\\(?:dailymotion\\.com/video/\\)\\([a-zA-Z0-9]+\\)" url))

(defun org-embed-dailymotion-handler (url width height)
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
(defun org-embed-ted-matcher (url)
  "Check if URL is a TED talk."
  (string-match "\\(?:ted\\.com/talks/\\)\\([a-zA-Z0-9_]+\\)" url))

(defun org-embed-ted-handler (url width height)
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
(defun org-embed-local-video-matcher (url)
  "Check if URL is a local video file.
Returns true if the URL represents a local video file with a video extension."
  (and (or (string-prefix-p "/" url)
           (string-prefix-p "file://" url)
           (string-prefix-p "./" url)
           (string-match-p "^[A-Za-z]:" url)) ; Windows 路径
       (string-match-p "\\.\\(mp4\\|webm\\|ogv\\|mov\\|mkv\\|avi\\)$" url)))

(defun org-embed-local-video-handler (url width height)
  "Generate HTML for embedding local video file from URL with WIDTH and HEIGHT."
  (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>Video Player</title>
  <style>
    body, html { margin: 0; padding: 0; height: 100%%; overflow: hidden; background-color: #000; }
    video { width: 100%%; height: 100%%; max-width: %dpx; max-height: %dpx; }
  </style>
</head>
<body>
  <video controls autoplay>
    <source src=\"%s\" type=\"video/mp4\">
    <source src=\"%s\" type=\"video/webm\">
    <source src=\"%s\" type=\"video/ogg\">
    Your browser does not support the video tag.
  </video>
</body>
</html>" width height url url url))

;; PDF file rules
(defun org-embed-pdf-matcher (url)
  "Check if URL is a PDF file."
  (or (string-match "\\.pdf$" url)
      (and (string-match "^https?://" url)
           (string-match "\\.pdf" url))))

(defun org-embed-pdf-handler (url width height)
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
(defun org-embed-webpage-matcher (url)
  "Check if URL is a webpage."
  (string-match "^https?://" url))

(defun org-embed-webpage-handler (url width height)
  "Generate HTML for embedding webpage from URL with WIDTH and HEIGHT.
Returns nil to allow direct URL opening."
  nil)

;; Register all rules
(defun org-embed-register-default-rules ()
  "Register all default embedding rules."
  (org-embed-register-rule 'youtube 
                          #'org-embed-youtube-matcher 
                          #'org-embed-youtube-handler)
  
  (org-embed-register-rule 'bilibili 
                          #'org-embed-bilibili-matcher 
                          #'org-embed-bilibili-handler)
  
  (org-embed-register-rule 'vimeo 
                          #'org-embed-vimeo-matcher 
                          #'org-embed-vimeo-handler)
  
  (org-embed-register-rule 'dailymotion 
                          #'org-embed-dailymotion-matcher 
                          #'org-embed-dailymotion-handler)
  
  (org-embed-register-rule 'ted 
                          #'org-embed-ted-matcher 
                          #'org-embed-ted-handler)
  
  (org-embed-register-rule 'local-video 
                          #'org-embed-local-video-matcher 
                          #'org-embed-local-video-handler)
  
  (org-embed-register-rule 'pdf
                          #'org-embed-pdf-matcher
                          #'org-embed-pdf-handler)
  

  
  (org-embed-register-rule 'webpage
                          #'org-embed-webpage-matcher
                          #'org-embed-webpage-handler))

;; Register default rules when this file is loaded
(org-embed-register-default-rules)

(provide 'org-embed-rules)
;;; org-embed-rules.el ends here
