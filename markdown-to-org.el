;;; markdown-to-org.el --- Convert markdown to org-mode automatically -*- lexical-binding: t -*-

;; Copyright (C) 2024 Seijiro Ikehata

;; Author: Seijiro Ikehata <modeverv@g-m.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, markdown, org
;; URL: https://github.com/modeverv/markdown-to-org
;; License: MIT

;;; Commentary:

;; This package provides functionality to convert Markdown text to Org format
;; when yanking in org-mode buffers.  It can work in three ways:
;;
;; 1. On-demand conversion from clipboard:
;;    M-x org-yank-from-clipboard-from-md-to-org
;;
;; 2. On-demand conversion from kill-ring:
;;    M-x org-yank-from-kill-ring-from-md-to-org
;;
;; 3. Automatic conversion on yank (when enabled):
;;    (markdown-to-org-advice-mode 1)
;;
;; The automatic conversion can be toggled globally or per-buffer.

;;; Code:

(require 'org)
(require 'cl-lib)

(defgroup markdown-to-org nil
  "Convert markdown to org-mode when yanking."
  :group 'convenience
  :prefix "markdown-to-org-")

(defcustom markdown-to-org-convert-on-yank nil
  "When non-nil, automatically convert markdown to org when yanking in org-mode."
  :type 'boolean
  :group 'markdown-to-org)

;;;###autoload
(define-minor-mode markdown-to-org-advice-mode
  "Toggle automatic markdown to org conversion on yank."
  :global t
  :group 'markdown-to-org
  :lighter " MD->Org"
  (if markdown-to-org-advice-mode
      (progn
        (advice-add 'yank :around #'markdown-to-org--smart-yank-advice)
        (advice-add 'yank-pop :around #'markdown-to-org--smart-yank-advice))
    (advice-remove 'yank #'markdown-to-org--smart-yank-advice)
    (advice-remove 'yank-pop #'markdown-to-org--smart-yank-advice)))

(defun markdown-to-org--looks-like-markdown-p (text)
  "Check if TEXT looks like markdown content."
  (and text
       (or
        ;; Check for markdown headers
        (string-match-p "^#+ " text)
        ;; Check for code blocks
        (string-match-p "^```" text)
        ;; Check for markdown links
        (string-match-p "\\[.*\\](.*)" text)
        ;; Check for markdown checkboxes
        (string-match-p "^- \\[[ x]\\] " text)
        ;; Check for inline code
        (string-match-p "`[^`]+`" text))))


(defun markdown-to-org--escaped-p (position)
  "Return non-nil if the character at POSITION follows an odd backslash count."
  (save-excursion
    (goto-char position)
    (let ((count 0))
      (while (eq (char-before) ?\\)
        (setq count (1+ count))
        (backward-char))
      (= (% count 2) 1))))

(defun markdown-to-org--protect-latex ()
  "Replace complete LaTeX fragments in the current buffer with placeholders.
Recognize unescaped \\[...\\] and \\(...\\) delimiter pairs.
Return an alist mapping placeholders to the original fragments.
Keep newlines in placeholders so Markdown conversions retain line boundaries.
Unmatched delimiters are treated as ordinary Markdown text."
  (let ((prefix "MDTORGMATH")
        (index 0)
        fragments)
    ;; Choose a prefix absent from the input, including ordinary prose or code.
    (while (save-excursion
             (goto-char (point-min))
             (search-forward prefix nil t))
      (setq prefix (concat prefix "X")))
    (goto-char (point-min))
    (while (re-search-forward (regexp-opt '("\\[" "\\(")) nil t)
      (let* ((begin (match-beginning 0))
             (closer (if (equal (match-string 0) "\\[") "\\]" "\\)"))
             (end (unless (markdown-to-org--escaped-p begin)
                    (save-excursion
                      (catch 'end
                        (while (search-forward closer nil t)
                          (unless (markdown-to-org--escaped-p (- (point) 2))
                            (throw 'end (point))))
                        nil)))))
        (when end
          (let* ((original (buffer-substring-no-properties begin end))
                 (placeholder (replace-regexp-in-string
                               "[^\n]+" (format "%s%dEND" prefix index)
                               original t t)))
            (delete-region begin end)
            (goto-char begin)
            (insert placeholder)
            (push (cons placeholder original) fragments)
            (setq index (1+ index))))))
    fragments))

(defun markdown-to-org--protect-literals ()
  "Protect code, link destinations and escaped emphasis in the current buffer.
Return an alist of placeholders and literal Org replacements.
Preserve newlines so hiding a block cannot join separate lines of emphasis."
  (let ((prefix "MDTORGLITERAL")
        (index 0)
        fragments)
    (while (save-excursion
             (goto-char (point-min))
             (search-forward prefix nil t))
      (setq prefix (concat prefix "X")))
    (cl-labels
        ((protect (begin end replacement)
           (let ((placeholder
                  (replace-regexp-in-string
                   "[^\n]+" (format "%s%dEND" prefix index)
                   (buffer-substring-no-properties begin end) t t)))
             (delete-region begin end)
             (goto-char begin)
             (insert placeholder)
             (push (cons placeholder replacement) fragments)
             (setq index (1+ index)))))
      ;; Convert the fences now, but keep their contents out of every pass.
      (goto-char (point-min))
      (while (re-search-forward "^```\\([[:alnum:]]+\\)?$" nil t)
        (let* ((begin (match-beginning 0))
               (body-begin (match-end 0))
               (lang (match-string-no-properties 1))
               (header (if lang (concat "#+BEGIN_SRC " lang) "#+BEGIN_SRC"))
               (closed (re-search-forward "^```$" nil t))
               (body-end (if closed (match-beginning 0) (point-max)))
               (end (if closed (match-end 0) (point-max)))
               (body (buffer-substring-no-properties body-begin body-end)))
          (protect begin end
                   (concat header body
                           (unless (or closed (string-suffix-p "\n" body))
                             "\n")
                           "#+END_SRC" (unless closed "\n")))))
      ;; Store the Org code delimiters together with the literal content.
      (goto-char (point-min))
      (while (re-search-forward "`\\([^`\n]+\\)`" nil t)
        (unless (markdown-to-org--escaped-p (match-beginning 0))
          (protect (match-beginning 0) (match-end 0)
                   (concat "=" (match-string-no-properties 1) "="))))
      ;; Only hide the destination: link labels still support emphasis.
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
        (protect (match-beginning 2) (match-end 2)
                 (match-string-no-properties 2)))
      (goto-char (point-min))
      (while (re-search-forward "[*_]" nil t)
        (when (markdown-to-org--escaped-p (match-beginning 0))
          (protect (match-beginning 0) (match-end 0)
                   (match-string-no-properties 0)))))
    fragments))

(defun markdown-to-org--convert (text)
  "Convert Markdown TEXT to Org while preserving LaTeX and literal content."
  (with-temp-buffer
    (insert text)
    (let* ((math (markdown-to-org--protect-latex))
           (literals (markdown-to-org--protect-literals))
           (converted (markdown-to-org--convert-markdown (buffer-string))))
      (erase-buffer)
      (insert converted)
      ;; Literal replacements may themselves contain protected math tokens.
      (dolist (fragment (append literals math))
        (goto-char (point-min))
        (search-forward (car fragment))
        ;; Backslashes in code, URLs and LaTeX are not replacement escapes.
        (replace-match (cdr fragment) t t))
      (buffer-string))))

(defun markdown-to-org--convert-markdown (text)
  "Convert markdown TEXT to org format.
Convert in the following order to handle nested structures correctly:
Code and other literals must already be protected by the caller.
Convert lists, inline formatting, then headings so generated Org heading
stars cannot be mistaken for Markdown emphasis."
  (with-temp-buffer
    (insert text)
    ;; debug
    ;; (message "Initial buffer:\n%s" (buffer-string))

    ;; List structure conversions

    ;; First, handle checkbox lists with proper indentation
    (goto-char (point-min))
    (while (re-search-forward "^\\([ ]*\\)\\(-\\) \\(\\[[ xX]\\]\\) " nil t)
      (let* ((indent (match-string 1))
             (indent-level (/ (length indent) 2))
             (org-indent (make-string (* indent-level 2) ?\s))
             (checkbox (match-string 3))
             (checked (member (substring checkbox 1 2) '("x" "X"))))
        (replace-match (concat org-indent "- ["
                               (if checked "X" " ")
                               "] "))))

    ;; Then handle numbered lists (preserve numbers)
    (goto-char (point-min))
    (while (re-search-forward "^\\([ ]*\\)\\([0-9]+\\)\\. " nil t)
      (let* ((indent-level (/ (length (match-string 1)) 2))
             (org-indent (make-string (* indent-level 2) ?\s))
             (number (match-string 2)))
        (replace-match (concat org-indent number ". "))))

    ;; Finally handle regular nested lists
    (goto-char (point-min))
    (while (re-search-forward "^\\([ ]*\\)\\(-\\) " nil t)
      (let* ((indent (match-string 1))
             (indent-level (/ (length indent) 2))
             (org-indent (make-string (* indent-level 2) ?\s)))
        (replace-match (concat org-indent "- "))))

    ;; Convert single-star emphasis before generating Org bold stars.
    ;; Inspect the surrounding characters without consuming them: they may
    ;; also be the boundary of the next match, or the end of the buffer.
    (goto-char (point-min))
    (while (re-search-forward
            (rx "*" (group (not (any space "*"))
                           (? (seq (* (not (any "*" ?\n)))
                                   (not (any space "*")))))
                "*")
            nil t)
      (unless (or (eq (char-before (match-beginning 0)) ?*)
                  (eq (char-after (match-end 0)) ?*))
        (replace-match "/\\1/" t)))

    (goto-char (point-min))
    (while (re-search-forward
            (rx "**" (group (not (any space "*")) (* (not (any "*" ?\n)))) "**")
            nil t)
      (replace-match "*\\1*"))
    (goto-char (point-min))
    (while (re-search-forward "_\\([^_\n]+\\)_" nil t)
      (replace-match "/\\1/"))

    ;; Convert links [text](url) to [[url][text]]
    (goto-char (point-min))
    (while (re-search-forward "\\[\\([^]]+\\)\\](\\([^)]+\\))" nil t)
      (replace-match "[[\\2][\\1]]"))

    ;; Convert headers last so their stars never enter the emphasis passes.
    (goto-char (point-min))
    (while (re-search-forward "^\\(#+\\)\\([ \t]*\\)\\(.+\\)$" nil t)
      (replace-match (concat (make-string (length (match-string 1)) ?*)
                             (match-string 2) (match-string 3)) t t))

    ;; debug
    ;; (message "Final conversion result:\n%s" (buffer-string))
    (buffer-string)))

(defun markdown-to-org--smart-yank-advice (orig-fun &rest args)
  "Advice for yank that converts markdown to org-mode when appropriate."
  (if (eq major-mode 'org-mode)
      (let ((start (point)))
        ;; 一旦普通にyankする
        (apply orig-fun args)
        (let* ((end (point))
               (text (buffer-substring-no-properties start end)))
          ;; マークダウンっぽければ変換
          (when (and text (markdown-to-org--looks-like-markdown-p text))
            (delete-region start end)
            (insert (markdown-to-org--convert text)))))
    ;; org-mode以外は普通にyank
    (apply orig-fun args)))

;;;###autoload
(defun org-yank-from-clipboard-from-md-to-org ()
  "Convert markdown from clipboard to org and insert at point."
  (interactive)
  (let ((start (point)))
    (yank)  ; 通常のyankを実行
    (let* ((end (point))
           (md-text (buffer-substring-no-properties start end)))
      (delete-region start end)  ; yankした内容を一旦削除
      (when (and md-text (stringp md-text))
        (insert (markdown-to-org--convert md-text))
        (message "Converted and inserted markdown")))))

;;;###autoload
(defun org-yank-from-kill-ring-from-md-to-org ()
  "Convert markdown from last kill to org and insert at point."
  (interactive)
  (let* ((md-text (current-kill 0 t))
         (org-text (markdown-to-org--convert md-text)))
    (insert org-text)
    (message "Converted and inserted markdown from kill-ring")))

(defvar markdown-to-org-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-y c") 'org-yank-from-clipboard-from-md-to-org)
    (define-key map (kbd "C-c C-y k") 'org-yank-from-kill-ring-from-md-to-org)
    map)
  "Keymap for markdown-to-org-mode commands.")

;;;###autoload
(define-minor-mode markdown-to-org-mode
  "Minor mode for markdown to org conversion features."
  :lighter " MD->Org"
  :keymap markdown-to-org-mode-map
  (if markdown-to-org-mode
      (progn
        (setq-local markdown-to-org-convert-on-yank t)
        (advice-add 'yank :around #'markdown-to-org--smart-yank-advice)
        (advice-add 'yank-pop :around #'markdown-to-org--smart-yank-advice))
    (progn
      (setq-local markdown-to-org-convert-on-yank nil)
      (advice-remove 'yank #'markdown-to-org--smart-yank-advice)
      (advice-remove 'yank-pop #'markdown-to-org--smart-yank-advice))))

(provide 'markdown-to-org)

;;; markdown-to-org.el ends here
