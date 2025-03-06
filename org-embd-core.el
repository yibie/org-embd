;;; org-embd-core.el --- Core functionality for embedding content in org-mode -*- lexical-binding: t; -*-

;; Copyright (C) 2025

;; Author: User
;; Keywords: multimedia, hypermedia, outlines
;; Version: 0.1

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; This file provides the core functionality for embedding various
;; types of content in org-mode using xwidget-webkit.

;;; Code:

(require 'xwidget)
(require 'org)
(require 'org-element)

(defgroup org-embd nil
  "Embedding content in org-mode."
  :group 'org
  :prefix "org-embd-")

(defcustom org-embd-video-width 800
  "Default width for embedded videos."
  :type 'integer
  :group 'org-embd)

(defcustom org-embd-video-height 600
  "Default height for embedded videos."
  :type 'integer
  :group 'org-embd)

(defcustom org-embd-webpage-width 800
  "Default width for embedded webpages."
  :type 'integer
  :group 'org-embd)

(defcustom org-embd-webpage-height 600
  "Default height for embedded webpages."
  :type 'integer
  :group 'org-embd)

(defcustom org-embd-pdf-width 800
  "Default width for embedded PDF files."
  :type 'integer
  :group 'org-embd)

(defcustom org-embd-pdf-height 600
  "Default height for embedded PDF files."
  :type 'integer
  :group 'org-embd)

(defvar org-embd-rules-alist nil
  "Alist of rules for embedding content.
Each rule is a cons cell (MATCHER . HANDLER) where:
- MATCHER is a function that takes a URL and returns non-nil if it can handle it
- HANDLER is a function that takes a URL, width, height and returns HTML")

(defun org-embd-register-rule (name matcher handler)
  "Register a new embedding rule.
NAME is a symbol identifying the rule.
MATCHER is a function that takes a URL and returns non-nil if it can handle it.
HANDLER is a function that takes a URL, width, height and returns HTML."
  (setq org-embd-rules-alist
        (cons (cons name (cons matcher handler))
              (assq-delete-all name org-embd-rules-alist))))

(defun org-embd-find-handler (url)
  "Find a handler for URL from registered rules."
  (let ((result nil))
    (dolist (rule org-embd-rules-alist result)
      (let ((matcher (cadr rule))
            (handler (cddr rule)))
        (when (and (not result) (funcall matcher url))
          (setq result handler))))))

(defun org-embd-create-html-file (html-content)
  "Create a temporary HTML file with HTML-CONTENT."
  (let ((temp-file (make-temp-file "emacs-embed-" nil ".html")))
    (with-temp-file temp-file
      (insert html-content))
    temp-file))

(defun org-embd-insert-content (html-content width height title)
  "Insert content using xwidget-webkit.
HTML-CONTENT is the HTML to display.
WIDTH and HEIGHT specify the dimensions.
TITLE is the title of the widget."
  (let* ((start-point (point))
         (buffer-size (buffer-size))
         (valid-point (min start-point (max 1 buffer-size))))
    (condition-case err
        (let ((object (xwidget-insert valid-point 'webkit title width height)))
          (xwidget-webkit-goto-uri object (concat "file://" 
                                                  (org-embd-create-html-file html-content))))
      (error
       (message "Error in org-embd-insert-content: %s" err)
       (message "Debug info - point: %d, buffer-size: %d, valid-point: %d" 
                start-point buffer-size valid-point)))))

(defun org-embd-ensure-directory (dir)
  "确保目录 DIR 存在，如果不存在则创建它。"
  (unless (file-exists-p dir)
    (make-directory dir t)))

(defun org-embd-local-path-to-url (path)
  "将本地文件路径 PATH 转换为 URL 格式。"
  (concat "file://" (expand-file-name path)))

(defun org-embd-direct-url-p (url)
  "判断 URL 是否应该直接打开而不是通过 HTML 包装。
返回 t 如果是普通网页 URL，nil 如果是特殊内容类型。"
  (and (string-match-p "^https?://" url)
       (not (or (string-match-p "\\.pdf$" url)
                (string-match-p "\\.mp4$" url)
                (string-match-p "\\.webm$" url)
                (string-match-p "\\.ogg$" url)))))

(defun org-embd-ensure-new-line ()
  "确保当前位置在一个新行上，适合插入内容。
在链接后添加新行。"
  (end-of-line)
  (newline))

(defun org-embd-insert-link (type url)
  "插入一个 org-embd 链接，并确保光标位于下一行准备插入内容。
TYPE 是链接类型，如 'video'、'webpage' 或 'pdf'。
URL 是要嵌入的内容地址。"
  ;; 插入链接
  (insert (format "[[org-embd:%s:%s]]" type url))
  ;; 确保内容显示在新行
  (org-embd-ensure-new-line))

(defun org-embd-content (url &optional width height title)
  "Embed content from URL in org-mode.
Optional WIDTH and HEIGHT specify dimensions.
Optional TITLE specifies the widget title."
  (interactive "sURL or file address: ")
  (let* ((width (or width org-embd-video-width))
         (height (or height org-embd-video-height))
         (title (or title "Embedded Content"))
         (handler (org-embd-find-handler url)))
    ;; 确保内容显示在新行
    (org-embd-ensure-new-line)
    (if (and (org-embd-direct-url-p url)
             (or (not handler) 
                 (eq handler #'org-embd-webpage-handler)))
        ;; 直接打开网页URL
        (let* ((start-point (point))
               (buffer-size (buffer-size))
               (valid-point (min start-point (max 1 buffer-size))))
          (condition-case err
              (let ((object (xwidget-insert valid-point 'webkit title width height)))
                (xwidget-webkit-goto-uri object url))
            (error
             (message "Error opening URL directly: %s" err)
             (message "Debug info - point: %d, buffer-size: %d, valid-point: %d" 
                      start-point buffer-size valid-point))))
      ;; 使用HTML包装
      (let ((html-content
             (if handler
                 (funcall handler url width height)
               ;; Default fallback for unrecognized URLs
               (org-embd-default-handler url width height))))
        (if (and html-content (stringp html-content) (not (string-empty-p html-content)))
            (org-embd-insert-content html-content width height title)
          (message "Failed to generate HTML content for URL: %s" url))))))

(defun org-embd-video (url &optional width height insert-link)
  "Embed video from URL in org-mode.
Optional WIDTH and HEIGHT specify dimensions.
Optional INSERT-LINK controls whether to insert a link (defaults to t)."
  (interactive "sVideo URL or file address: ")
  ;; 插入链接，除非明确指定不插入
  (when (or insert-link (null insert-link))
    (insert (format "[[org-embd:video:%s]]" url))
    (end-of-line)
    (newline))
  ;; 如果不插入链接，确保内容显示在新行
  (when (eq insert-link nil)
    (end-of-line)
    (newline))
  ;; 处理视频URL，确保直接显示视频内容
  (let ((processed-url url))
    ;; 处理YouTube视频
    (when (string-match "\\(youtube\\.com/watch\\?v=\\|youtu\\.be/\\)\\([^&]+\\)" url)
      (setq processed-url (concat "https://www.youtube.com/embed/" (match-string 2 url))))
    ;; 处理Bilibili视频
    (when (string-match "bilibili\\.com/video/\\(BV[0-9a-zA-Z]+\\)" url)
      (setq processed-url (concat "https://player.bilibili.com/player.html?bvid=" (match-string 1 url))))
    ;; 处理Vimeo视频
    (when (string-match "vimeo\\.com/\\([0-9]+\\)" url)
      (setq processed-url (concat "https://player.vimeo.com/video/" (match-string 1 url))))
    ;; 处理Dailymotion视频
    (when (string-match "dailymotion\\.com/video/\\([a-z0-9]+\\)" url)
      (setq processed-url (concat "https://www.dailymotion.com/embed/video/" (match-string 1 url))))
    ;; 处理TED视频
    (when (string-match "ted\\.com/talks/\\([_a-z0-9]+\\)" url)
      (setq processed-url (concat "https://embed.ted.com/talks/" (match-string 1 url))))
    
    (org-embd-content processed-url 
                      (or width org-embd-video-width) 
                      (or height org-embd-video-height)
                      "Video Player")))

(defun org-embd-webpage (url &optional width height insert-link)
  "Embed webpage from URL in org-mode.
Optional WIDTH and HEIGHT specify dimensions.
Optional INSERT-LINK controls whether to insert a link (defaults to t)."
  (interactive "sWebpage URL: ")
  ;; 插入链接，除非明确指定不插入
  (when (or insert-link (null insert-link))
    (insert (format "[[org-embd:webpage:%s]]" url))
    (end-of-line)
    (newline))
  ;; 如果不插入链接，确保内容显示在新行
  (when (eq insert-link nil)
    (end-of-line)
    (newline))
  ;; 确保URL有正确的协议前缀
  (unless (string-match-p "^https?://" url)
    (setq url (concat "https://" url)))
  (org-embd-content url 
                    (or width org-embd-webpage-width) 
                    (or height org-embd-webpage-height)
                    "Embedded Webpage"))

(defun org-embd-pdf (url &optional width height insert-link)
  "Embed PDF from local file in org-mode.
Optional WIDTH and HEIGHT specify dimensions.
Optional INSERT-LINK controls whether to insert a link (defaults to t)."
  (interactive "fPDF file path: ")
  ;; 插入链接，除非明确指定不插入
  (when (or insert-link (null insert-link))
    (insert (format "[[org-embd:pdf:%s]]" url))
    (end-of-line)
    (newline))
  ;; 如果不插入链接，确保内容显示在新行
  (when (eq insert-link nil)
    (end-of-line)
    (newline))
  ;; 检查文件是否存在
  (if (file-exists-p url)
      (progn
        ;; 将本地文件路径转换为 URL 格式
        (let ((pdf-url (org-embd-local-path-to-url url)))
          (org-embd-content pdf-url
                          (or width org-embd-pdf-width)
                          (or height org-embd-pdf-height)
                          "PDF Viewer")))
    (message "PDF file not found: %s" url)))

(defun org-embd-default-handler (url width height)
  "Default handler for unrecognized URLs.
URL is the content address.
WIDTH and HEIGHT specify dimensions."
  (format "<!DOCTYPE html>
<html lang=\"en\">
<head>
  <meta charset=\"UTF-8\">
  <meta name=\"viewport\" content=\"width=device-width\">
  <title>Embedded Content</title>
  <style>
    body, html { margin: 0; padding: 0; height: 100%%; overflow: hidden; }
    iframe { border: none; width: 100%%; height: 100%%; }
  </style>
</head>
<body>
  <iframe width=\"%d\" height=\"%d\" src=\"%s\" allowfullscreen></iframe>
</body>
</html>" width height url))

;; Function to clean up xwidgets in the buffer
(defun org-embd-clean-xwidgets ()
  "Clean up all xwidgets in the current buffer."
  (interactive)
  (let ((count 0))
    (dolist (xw (xwidget-list))
      (when (eq (current-buffer) (xwidget-buffer xw))
        (setq count (1+ count))
        (delete-xwidget-view (get-buffer-window (xwidget-buffer xw)))
        (kill-xwidget xw)))
    (message "Cleaned up %d xwidgets" count)))

;; Function to clean up a specific xwidget at point
(defun org-embd-clean-xwidget-at-point ()
  "Clean up the xwidget at or after point."
  (interactive)
  (let ((found nil))
    (save-excursion
      (let ((xw (xwidget-at (point))))
        (if xw
            (progn
              (delete-xwidget-view (get-buffer-window (xwidget-buffer xw)))
              (kill-xwidget xw)
              (setq found t))
          ;; If not found at point, search forward for the next xwidget
          (let ((next-pos (next-single-property-change (point) 'xwidget)))
            (when next-pos
              (goto-char next-pos)
              (let ((xw (xwidget-at (point))))
                (when xw
                  (delete-xwidget-view (get-buffer-window (xwidget-buffer xw)))
                  (kill-xwidget xw)
                  (setq found t)))))))
    (if found
        (message "Cleaned up xwidget at point")
      (message "No xwidget found at or after point")))))

;; Function to follow embd links
(defun org-embd-follow-link (path)
  "Follow the embd link with PATH."
  (let* ((parts (split-string path ":"))
         (type (car parts))
         (url (mapconcat #'identity (cdr parts) ":")))
    ;; 确保内容显示在链接下方
    (end-of-line)
    (newline)
    
    ;; 根据链接类型处理内容
    (cond
     ((string= type "video")
      ;; 处理视频URL，确保直接显示视频内容
      (let ((processed-url url))
        ;; 处理YouTube视频
        (when (string-match "\\(youtube\\.com/watch\\?v=\\|youtu\\.be/\\)\\([^&]+\\)" url)
          (setq processed-url (concat "https://www.youtube.com/embed/" (match-string 2 url))))
        ;; 处理Bilibili视频
        (when (string-match "bilibili\\.com/video/\\(BV[0-9a-zA-Z]+\\)" url)
          (setq processed-url (concat "https://player.bilibili.com/player.html?bvid=" (match-string 1 url))))
        ;; 处理Vimeo视频
        (when (string-match "vimeo\\.com/\\([0-9]+\\)" url)
          (setq processed-url (concat "https://player.vimeo.com/video/" (match-string 1 url))))
        ;; 处理Dailymotion视频
        (when (string-match "dailymotion\\.com/video/\\([a-z0-9]+\\)" url)
          (setq processed-url (concat "https://www.dailymotion.com/embed/video/" (match-string 1 url))))
        ;; 处理TED视频
        (when (string-match "ted\\.com/talks/\\([_a-z0-9]+\\)" url)
          (setq processed-url (concat "https://embed.ted.com/talks/" (match-string 1 url))))
        
        (org-embd-content processed-url org-embd-video-width org-embd-video-height "Video Player")))
     
     ((string= type "webpage")
      ;; 确保URL有正确的协议前缀
      (unless (string-match-p "^https?://" url)
        (setq url (concat "https://" url)))
      (org-embd-content url org-embd-webpage-width org-embd-webpage-height "Embedded Webpage"))
     
     ((string= type "pdf")
      ;; 检查文件是否存在
      (if (file-exists-p url)
          (progn
            ;; 将本地文件路径转换为 URL 格式
            (let ((pdf-url (org-embd-local-path-to-url url)))
              (org-embd-content pdf-url org-embd-pdf-width org-embd-pdf-height "PDF Viewer")))
        (message "PDF file not found: %s" url)))
     
     (t (message "Unknown org-embd link type: %s" type)))))

(provide 'org-embd-core)
;;; org-embd-core.el ends here
