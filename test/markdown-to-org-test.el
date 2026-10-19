;;; markdown-to-org-test.el --- Conversion regression tests -*- lexical-binding: t; -*-

(require 'ert)
(require 'markdown-to-org)

(defconst markdown-to-org-test--issue-2-equation
  "\\[
\\begin{aligned}
kx - fy ={}& -\\Delta_2\\left(5\\Delta_2 + \\Delta_3\\right)p^{-1} \\\\
&+ \\left(-(4N-2)\\Delta_2^2 - 9N\\Delta_2 + 3N\\Delta_3\\right) \\\\
&+ N\\left(-(5N-17)\\Delta_2 + (N-1)\\Delta_3\\right)p \\\\
&+ 6N(N-1)\\,\\Delta_2\\,p^2
\\end{aligned}
\\]"
  "Original Markdown equation from GitHub issue #2.
https://github.com/modeverv/markdown-to-org/issues/2")

(ert-deftest markdown-to-org-preserves-issue-2-equation ()
  (should (equal (markdown-to-org--convert
                  markdown-to-org-test--issue-2-equation)
                 markdown-to-org-test--issue-2-equation)))

(ert-deftest markdown-to-org-preserves-inline-latex ()
  (let ((text "The value is \\(\\Delta_2 + \\Delta_3\\), then \\(x_i + y_j\\)."))
    (should (equal (markdown-to-org--convert text) text))))

(ert-deftest markdown-to-org-preserves-all-markup-inside-latex ()
  ;; Every conversion pass must leave the protected region alone.
  (dolist (delimiters '(("\\[" . "\\]") ("\\(" . "\\)")))
    (let ((text (concat (car delimiters)
                        "\n# parameter\n- [x] item\n1. item\n"
                        "**bold** _italic_ `code` [label](url)\n"
                        (cdr delimiters))))
      (should (equal (markdown-to-org--convert text) text)))))

(ert-deftest markdown-to-org-converts-markdown-around-latex ()
  (let ((math "\\(x_i + y_j\\)"))
    (should
     (equal (markdown-to-org--convert
             (concat "# Heading\n_before_ " math " **after**\n"
                     "- [x] task\n[link](https://example.com)\n`code`"))
            (concat "* Heading\n/before/ " math " *after*\n"
                    "- [X] task\n[[https://example.com][link]]\n=code=")))))

(ert-deftest markdown-to-org-converts-emphasis-enclosing-inline-latex ()
  (should (equal (markdown-to-org--convert "_value \\(x_i + y_j\\)_")
                 "/value \\(x_i + y_j\\)/")))

(ert-deftest markdown-to-org-preserves-multiple-latex-regions ()
  (let ((text "\\(a_b + c_d\\)\\[e_f + g_h\\]\n\\[i_j\\]\n\\(k_l\\)"))
    (should (equal (markdown-to-org--convert text) text))))

(ert-deftest markdown-to-org-keeps-latex-line-boundaries ()
  ;; Hiding a multiline fragment must not create a single-line emphasis match.
  (let ((text "_before\\[x_i\n\ny_j\\]after_\n# Heading"))
    (should (equal (markdown-to-org--convert text)
                   "_before\\[x_i\n\ny_j\\]after_\n* Heading"))))

(ert-deftest markdown-to-org-ignores-escaped-latex-openers ()
  (dolist (delimiter '("[" "("))
    (let ((prefix (concat (make-string 2 ?\\) delimiter " ")))
      (should (equal (markdown-to-org--convert (concat prefix "_italic_"))
                     (concat prefix "/italic/"))))))

(ert-deftest markdown-to-org-ignores-escaped-latex-closers ()
  (dolist (delimiters '(("\\[" . "]") ("\\(" . ")")))
    (let ((text (concat (car delimiters) "a_b "
                        (make-string 2 ?\\) (cdr delimiters)
                        " c_d + e_f " (make-string 1 ?\\) (cdr delimiters))))
      (should (equal (markdown-to-org--convert text) text)))))

(ert-deftest markdown-to-org-handles-even-backslashes-before-latex ()
  ;; A delimiter following an escaped backslash is still active.
  (let ((text (concat (make-string 3 ?\\) "[a_b + c_d"
                      (make-string 3 ?\\) "]")))
    (should (equal (markdown-to-org--convert text) text))))

(ert-deftest markdown-to-org-does-not-hide-text-after-unmatched-openers ()
  (should
   (equal (markdown-to-org--convert
           "\\[ unclosed _italic_\n# Heading\n\\(a_b + c_d\\)\n\\( unclosed **bold**")
          "\\[ unclosed /italic/\n* Heading\n\\(a_b + c_d\\)\n\\( unclosed *bold*")))

(ert-deftest markdown-to-org-leaves-unmatched-closers-alone ()
  (should (equal (markdown-to-org--convert "\\] _italic_ \\)")
                 "\\] /italic/ \\)")))

(ert-deftest markdown-to-org-preserves-text-resembling-placeholders ()
  (let ((text "MDTORGMATH0END MDTORGMATHX0END \\(x_i + y_j\\)"))
    (should (equal (markdown-to-org--convert text) text))))

(ert-deftest markdown-to-org-kill-ring-command-preserves-latex ()
  (with-temp-buffer
    (org-mode)
    (let ((kill-ring (list markdown-to-org-test--issue-2-equation))
          (kill-ring-yank-pointer nil)
          (interprogram-paste-function nil))
      (call-interactively #'org-yank-from-kill-ring-from-md-to-org)
      (should (equal (buffer-string) markdown-to-org-test--issue-2-equation)))))

(ert-deftest markdown-to-org-clipboard-command-preserves-latex ()
  (with-temp-buffer
    (org-mode)
    ;; Use the real yank path without accessing the system clipboard.
    (let ((kill-ring (list markdown-to-org-test--issue-2-equation))
          (kill-ring-yank-pointer nil)
          (interprogram-paste-function nil))
      (call-interactively #'org-yank-from-clipboard-from-md-to-org)
      (should (equal (buffer-string) markdown-to-org-test--issue-2-equation)))))

(ert-deftest markdown-to-org-yank-advice-preserves-latex ()
  (with-temp-buffer
    (org-mode)
    (let* ((text (concat "# Heading\n" markdown-to-org-test--issue-2-equation))
           (kill-ring (list text))
           (kill-ring-yank-pointer nil)
           (interprogram-paste-function nil))
      (markdown-to-org--smart-yank-advice #'yank)
      (should (equal (buffer-string)
                     (concat "* Heading\n" markdown-to-org-test--issue-2-equation))))))

;;; markdown-to-org-test.el ends here
