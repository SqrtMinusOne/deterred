;;; deterred-messengers-telega-export.el --- Export telega chats -*- lexical-binding: t -*-

;; Copyright (C) 2026 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Homepage: https://github.com/SqrtMinusOne/deterred.el

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This file has been mostly AI-generated.
;;
;; Export a single telega chat into an Org file with local attachments.

;;; Code:

(require 'color)
(require 'subr-x)

;; Telega functions (optional dependency)
(declare-function telega--getChatHistory "telega-tdlib")
(declare-function telega-chat-for-interactive "telega-core")
(declare-function telega-completing-read-chat "telega-util")
(declare-function telega-chat-title "telega-chat")
(declare-function org-element-property "org-element")
(declare-function telega-msg-content-text "telega-msg")
(declare-function telega-msg--content-file "telega-msg")
(declare-function telega-msg-sender "telega-msg")
(declare-function telega-msg-sender-title "telega-msg")
(declare-function telega-msg-sender-username "telega-msg")
(declare-function telega-msg-sender-palette "telega-msg")
(declare-function telega-file-get "telega-media")
(declare-function telega-file--download "telega-media")
(declare-function telega-file--downloaded-p "telega-core")
(declare-function telega-file--can-download-p "telega-core")
(declare-function telega--tl-get "telega-core")

(defvar telega--me-id)
(defvar telega-chatbuf--chat)
(defvar hfy-user-sheet-assoc)

(defun deterred-messengers--telega-safe-file-name (name)
  "Convert NAME into a filesystem-friendly file name."
  (let ((safe (or name "")))
    (setq safe (replace-regexp-in-string "[[:space:]\n\r\t]+" "-" safe))
    (setq safe (replace-regexp-in-string "[][\\\\/:*?\"<>|]" "_" safe))
    (setq safe (replace-regexp-in-string "_+" "_" safe))
    (setq safe (replace-regexp-in-string "-+" "-" safe))
    (setq safe (replace-regexp-in-string "\\`[-_.]+" "" safe))
    (setq safe (replace-regexp-in-string "[-_.]+\\'" "" safe))
    (if (equal safe "")
        "attachment"
      safe)))

(defun deterred-messengers--telega-message-type (msg)
  "Return telega message type for MSG."
  (plist-get (plist-get msg :content) :@type))

(defun deterred-messengers--telega-image-file-p (file-name)
  "Return non-nil if FILE-NAME looks like an image."
  (let ((ext (downcase (or (file-name-extension file-name) ""))))
    (member ext '("png" "jpg" "jpeg" "gif" "webp" "svg" "bmp" "tiff" "tif"))))

(defun deterred-messengers--telega-sender-color (sender)
  "Return exported header color for telega SENDER."
  (when-let* ((palette (telega-msg-sender-palette sender))
              (foreground-spec (assq :foreground palette)))
    (cadr foreground-spec)))

(defun deterred-messengers--telega-css-color (color)
  "Return COLOR normalized to CSS hex when possible."
  (cond
   ((null color) nil)
   ((string-match-p "\\`#" color) color)
   ((color-defined-p color)
    (apply #'color-rgb-to-hex
           (append (color-name-to-rgb color) '(2))))
   (t color)))

(defun deterred-messengers--telega-odt-style-name (color)
  "Return ODT character style name for COLOR."
  (concat "DeterredSenderColor"
          (replace-regexp-in-string "[^0-9A-Fa-f]" "" color)))

(defun deterred-messengers--telega-odt-style-xml (style-name color)
  "Return ODT character style XML for STYLE-NAME and COLOR."
  (format
   "
<style:style style:name=\"%s\" style:family=\"text\">
  <style:text-properties fo:color=\"%s\"/>
</style:style>"
   style-name color))

(defun deterred-messengers--telega-odt-register-style (style-name color)
  "Ensure ODT style STYLE-NAME exists in `hfy-user-sheet-assoc'."
  (when (boundp 'hfy-user-sheet-assoc)
    (let ((entry-key (intern style-name)))
      (unless (assoc entry-key hfy-user-sheet-assoc)
        (push (cons entry-key
                    (cons style-name
                          (deterred-messengers--telega-odt-style-xml
                           style-name color)))
              hfy-user-sheet-assoc)))))

(defun deterred-messengers--telega-odt-snippet-advice
    (orig-fun export-snippet contents info)
  "Register ODT text styles for sender-colored EXPORT-SNIPPET."
  (let ((value (org-element-property :value export-snippet)))
    (when (and value
               (string-match
                "text:style-name=\"\\(DeterredSenderColor\\([0-9A-Fa-f]\\{6\\}\\)\\)\""
                value))
      (deterred-messengers--telega-odt-register-style
       (match-string 1 value)
       (concat "#" (match-string 2 value)))))
  (funcall orig-fun export-snippet contents info))

(with-eval-after-load 'ox-odt
  (advice-add 'org-odt-export-snippet :around
              #'deterred-messengers--telega-odt-snippet-advice))

(defun deterred-messengers--telega-headline-title (sender sender-name sender-handle)
  "Return Org headline title for message SENDER."
  (let* ((sender-color
          (deterred-messengers--telega-css-color
           (deterred-messengers--telega-sender-color sender)))
         (headline-text
          (concat sender-name
                  (when sender-handle
                    (concat " • " sender-handle)))))
    (if sender-color
        (concat
         "@@html:<span style=\"color:" sender-color "; font-weight:700;\">@@"
         "@@odt:<text:span text:style-name=\""
         (deterred-messengers--telega-odt-style-name sender-color)
         "\">@@"
         headline-text
         "@@odt:</text:span>@@"
         "@@html:</span>@@")
      headline-text)))

(defun deterred-messengers--telega-fetch-chat-history (chat)
  "Fetch all messages from telega CHAT ordered from oldest to newest."
  (let ((from-msg-id 0)
        (offset -1)
        (oldest-msg-id nil)
        (messages nil)
        continue)
    (setq continue t)
    (while continue
      (let* ((history (telega--getChatHistory chat from-msg-id offset 100 nil))
             (batch (append (plist-get history :messages) nil))
             (older-batch
              (if oldest-msg-id
                  (seq-drop-while
                   (lambda (msg)
                     (>= (plist-get msg :id) oldest-msg-id))
                   batch)
                batch)))
        (setq continue (and older-batch t))
        (when older-batch
          (setq messages (nconc messages older-batch))
          (setq oldest-msg-id (plist-get (car (last older-batch)) :id))
          (setq from-msg-id oldest-msg-id)
          (setq offset 0))))
    (nreverse messages)))

(defun deterred-messengers--telega-download-file-sync (file)
  "Ensure telega FILE is downloaded and return its freshest version."
  (let* ((file-id (plist-get file :id))
         (current-file (or (telega-file-get file-id 'locally) file))
         (done (telega-file--downloaded-p current-file)))
    (cond
     (done current-file)
     ((not (telega-file--can-download-p current-file)) nil)
     (t
      (telega-file--download
       current-file
       :priority 32
       :update-callback
       (lambda (updated-file)
         (setq current-file updated-file)
         (setq done (telega-file--downloaded-p updated-file))))
      (while (not done)
        (accept-process-output nil 0.1)
        (setq current-file (or (telega-file-get file-id 'locally) current-file))
        (setq done (telega-file--downloaded-p current-file)))
      current-file))))

(defun deterred-messengers--telega-export-message-text (msg attachment-rel)
  "Return Org-ready textual representation for telega MSG.

ATTACHMENT-REL is non-nil if MSG has an exported attachment."
  (let* ((content (plist-get msg :content))
         (text (telega-msg-content-text msg t))
         (text (and text (substring-no-properties text))))
    (cond
     ((and text (not (equal text "")))
      text)
     ((not attachment-rel)
      (pcase (plist-get content :@type)
        ("messageLocation"
         (format "Location: %s, %s"
                 (or (telega--tl-get content :location :latitude) "?")
                 (or (telega--tl-get content :location :longitude) "?")))
        ("messageContact"
         (format "Contact: %s %s"
                 (or (telega--tl-get content :contact :first_name) "")
                 (or (telega--tl-get content :contact :last_name) "")))
        (_ (format "[%s]" (plist-get content :@type)))))
     (t nil))))

(defun deterred-messengers--telega-insert-org-text (text)
  "Insert TEXT as readable Org prose without changing document structure."
  (let ((lines (split-string text "\n")))
    (dolist (line lines)
      ;; Prevent message text from becoming Org structure.
      (when (string-match-p "\\`\\(?:\\*+\\|#\\+\\|:\\(?:PROPERTIES\\|END\\):\\||\\)" line)
        (setq line (concat "," line)))
      (insert line "\n"))))

(defun deterred-messengers--telega-export-attachment
    (msg index export-dir attachments-dir copied-files)
  "Export telega attachment from MSG.

INDEX is the message number in the export.  EXPORT-DIR is used to
compute the relative Org link.  ATTACHMENTS-DIR is where files are
copied.  COPIED-FILES caches already copied files by source path."
  (when-let* ((file (telega-msg--content-file msg))
              (downloaded-file (deterred-messengers--telega-download-file-sync file))
              (source-path (telega--tl-get downloaded-file :local :path))
              ((file-exists-p source-path)))
    (or (gethash source-path copied-files)
        (let* ((source-name (file-name-nondirectory source-path))
               (base-name (if (equal source-name "")
                              (deterred-messengers--telega-message-type msg)
                            source-name))
               (dest-name (format "%06d-%s-%s"
                                  index
                                  (plist-get msg :id)
                                  (deterred-messengers--telega-safe-file-name base-name)))
               (dest-path (expand-file-name dest-name attachments-dir))
               (relative-path (file-relative-name dest-path export-dir)))
          (unless (and (file-exists-p dest-path)
                       (file-equal-p source-path dest-path))
            (copy-file source-path dest-path t t))
          (puthash source-path relative-path copied-files)
          relative-path))))

(defun deterred-messengers--telega-write-org-message
    (msg index export-dir attachments-dir copied-files)
  "Write telega MSG to current Org export buffer."
  (let* ((sender-obj (telega-msg-sender msg))
         (sender (or (telega-msg-sender-title sender-obj)
                     "Unknown"))
         (sender (replace-regexp-in-string "[\n\r]+" " " sender))
         (sender-handle (telega-msg-sender-username sender-obj 'with-@))
         (content-type (deterred-messengers--telega-message-type msg))
         (timestamp (format-time-string
                     "[%Y-%m-%d %a %H:%M:%S]"
                     (seconds-to-time (plist-get msg :date))))
         (attachment-rel
          (condition-case err
              (deterred-messengers--telega-export-attachment
               msg index export-dir attachments-dir copied-files)
            (error
             (format "ERROR: %s" (error-message-string err)))))
         (text (deterred-messengers--telega-export-message-text
                msg
                (and attachment-rel
                     (not (string-prefix-p "ERROR: " attachment-rel))))))
    (insert "** "
            (deterred-messengers--telega-headline-title
             sender-obj sender sender-handle)
            "\n")
    (insert ":PROPERTIES:\n")
    (insert ":MESSAGE_ID: " (number-to-string (plist-get msg :id)) "\n")
    (insert ":CONTENT_TYPE: " content-type "\n")
    (insert ":END:\n")
    (insert timestamp "\n\n")
    (when text
      (deterred-messengers--telega-insert-org-text text)
      (insert "\n"))
    (when attachment-rel
      (if (string-prefix-p "ERROR: " attachment-rel)
          (insert attachment-rel "\n\n")
        (let ((attachment-name (file-name-nondirectory attachment-rel)))
          (if (deterred-messengers--telega-image-file-p attachment-name)
            (progn
                (insert "#+attr_org: :width 600\n")
                (insert "#+attr_html: :style max-width: min(100%, 720px); height: auto;\n")
                (insert "#+attr_odt: :width 14\n")
                ;; No description so Org exports it as an inline image.
                (insert "[[file:" attachment-rel "]]\n")
                (unless (and text (not (string-empty-p text)))
                  (insert "\n/" attachment-name "/\n")))
            (insert "Attachment: [[file:" attachment-rel "]["
                    attachment-name "]]\n")))))
    (insert "\n")))

;;;###autoload
(defun deterred-messengers-telega-export (chat directory)
  "Export telega CHAT into DIRECTORY as Org with local attachments."
  (interactive
   (progn
     (unless (featurep 'telega)
       (user-error "Telega is not loaded.  Please load telega.el first"))
     (unless telega--me-id
       (user-error "Telega is not initialized.  Please start telega first"))
     (let* ((chat (or (telega-chat-for-interactive)
                      telega-chatbuf--chat
                      (telega-completing-read-chat "Export chat: ")))
            (default-name
             (concat (deterred-messengers--telega-safe-file-name
                      (telega-chat-title chat t))
                     "/")))
       (list chat
             (read-directory-name "Export to directory: "
                                  default-directory default-name nil)))))
  (unless (featurep 'telega)
    (user-error "Telega is not loaded.  Please load telega.el first"))
  (unless telega--me-id
    (user-error "Telega is not initialized.  Please start telega first"))
  (unless chat
    (user-error "CHAT is required"))

  (let* ((export-dir (file-name-as-directory (expand-file-name directory)))
         (attachments-dir (expand-file-name "attachments" export-dir))
         (org-file (expand-file-name "messages.org" export-dir))
         (chat-title (telega-chat-title chat t))
         (messages (deterred-messengers--telega-fetch-chat-history chat))
         (copied-files (make-hash-table :test #'equal))
         (total (length messages))
         (index 0))
    (make-directory attachments-dir t)
    (with-temp-file org-file
      (insert "#+title: " chat-title "\n")
      (insert "#+options: toc:nil num:nil \\n:t\n")
      (insert "#+startup: showall inlineimages\n\n")
      (insert "* Metadata\n")
      (insert "- Chat: " chat-title "\n")
      (insert "- Exported at: "
              (format-time-string "[%Y-%m-%d %a %H:%M:%S]")
              "\n")
      (insert "- Messages: " (number-to-string total) "\n\n")
      (insert "* Messages\n\n")
      (dolist (msg messages)
        (setq index (1+ index))
        (deterred-messengers--telega-write-org-message
         msg index export-dir attachments-dir copied-files)
        (when (or (= index total) (= (% index 50) 0))
          (message "Exported %d/%d telega messages..." index total))))
    (message "Exported telega chat \"%s\" to %s" chat-title export-dir)
    org-file))

(provide 'deterred-messengers-telega-export)
;;; deterred-messengers-telega-export.el ends here
