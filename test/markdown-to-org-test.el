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

(ert-deftest markdown-to-org-preserves-bold-heading-levels ()
  ;; Inline bold delimiters must never consume the converted heading stars.
  (dotimes (index 6)
    (let ((level (1+ index)))
      (dolist (ending '("" "\n"))
        (should
         (equal (markdown-to-org--convert
                 (concat (make-string level ?#) " **Heading**" ending))
                (concat (make-string level ?*) " *Heading*" ending)))))))

(ert-deftest markdown-to-org-converts-asterisk-italic-at-end-of-input ()
  (should (equal (markdown-to-org--convert "*italic*") "/italic/"))
  (should (equal (markdown-to-org--convert "before *italic*")
                 "before /italic/")))

(ert-deftest markdown-to-org-converts-italic-next-to-punctuation ()
  (should (equal (markdown-to-org--convert "(*one*), *two*. *three*!")
                 "(/one/), /two/. /three/!")))

(ert-deftest markdown-to-org-converts-repeated-italic-on-one-line ()
  (dolist (ending '("" "\n"))
    (should (equal (markdown-to-org--convert
                    (concat "*one* *two* *three*" ending))
                   (concat "/one/ /two/ /three/" ending)))))

(ert-deftest markdown-to-org-converts-italic-on-adjacent-lines ()
  (dolist (ending '("" "\n"))
    (should (equal (markdown-to-org--convert
                    (concat "*one*\n*two*\n*three*" ending))
                   (concat "/one/\n/two/\n/three/" ending)))))

(ert-deftest markdown-to-org-preserves-whitespace-adjacent-asterisks ()
  ;; Whitespace immediately inside a delimiter cannot open/close emphasis.
  (dolist (text '("* leading*" "*trailing *" "before *trailing * after"))
    (should (equal (markdown-to-org--convert text) text))))

(ert-deftest markdown-to-org-preserves-markup-in-inline-code ()
  (let ((code "*pointer* **bold** _name_ [label](https://example.com/a_b)"))
    (should (equal (markdown-to-org--convert
                    (concat "`" code "` *outside*"))
                   (concat "=" code "= /outside/")))))

(ert-deftest markdown-to-org-preserves-markup-in-fenced-code ()
  (let ((code (concat "# Heading\n"
                      "a*b*c **bold** _name_\n"
                      "`code` [label](https://example.com/a_b)\n"
                      "- [x] task\n")))
    (should (equal (markdown-to-org--convert
                    (concat "```text\n" code "```\n*outside*"))
                   (concat "#+BEGIN_SRC text\n" code
                           "#+END_SRC\n/outside/")))))

(ert-deftest markdown-to-org-preserves-separate-fenced-code-blocks ()
  (should (equal (markdown-to-org--convert
                  "```\n*one*\n```\n*between*\n```\n_two_\n```")
                 (concat "#+BEGIN_SRC\n*one*\n#+END_SRC\n"
                         "/between/\n#+BEGIN_SRC\n_two_\n#+END_SRC"))))

(ert-deftest markdown-to-org-preserves-empty-fenced-code-blocks ()
  (dolist (case '(("```\n```" . "#+BEGIN_SRC\n#+END_SRC")
                  ("```\n```\n" . "#+BEGIN_SRC\n#+END_SRC\n")
                  ("```text\n```" . "#+BEGIN_SRC text\n#+END_SRC")))
    (should (equal (markdown-to-org--convert (car case)) (cdr case)))))

(ert-deftest markdown-to-org-closes-unfinished-fenced-code-blocks ()
  ;; Preserve the existing auto-close behavior, including a newline at EOF.
  (dolist (case '(("```" . "#+BEGIN_SRC\n#+END_SRC\n")
                  ("```\n" . "#+BEGIN_SRC\n#+END_SRC\n")
                  ("```text\n" . "#+BEGIN_SRC text\n#+END_SRC\n")
                  ("```\n*literal*" . "#+BEGIN_SRC\n*literal*\n#+END_SRC\n")
                  ("```\n*literal*\n" . "#+BEGIN_SRC\n*literal*\n#+END_SRC\n")))
    (should (equal (markdown-to-org--convert (car case)) (cdr case)))))

(ert-deftest markdown-to-org-preserves-text-resembling-literal-placeholders ()
  (should
   (equal (markdown-to-org--convert
           (concat "MDTORGLITERAL0END MDTORGLITERALX0END "
                   "`MDTORGLITERAL0END *literal*` *italic*"))
          (concat "MDTORGLITERAL0END MDTORGLITERALX0END "
                  "=MDTORGLITERAL0END *literal*= /italic/"))))

(ert-deftest markdown-to-org-preserves-link-destinations ()
  (let ((url "https://example.com/a*b*c/path_name_value?q=*x*&key=a_b_c"))
    (should (equal (markdown-to-org--convert (concat "[label](" url ")"))
                   (concat "[[" url "][label]]")))))

(ert-deftest markdown-to-org-converts-emphasis-in-link-labels ()
  (let ((url "https://example.com/a*b*c/path_name_value"))
    (should
     (equal (markdown-to-org--convert
             (concat "[*one* _two_ **three**](" url ") *after*"))
            (concat "[[" url "][/one/ /two/ *three*]] /after/")))))

(ert-deftest markdown-to-org-preserves-escaped-emphasis-markers ()
  ;; An odd number of backslashes escapes the following delimiter.  Keep
  ;; their spelling rather than treating the escaped asterisks as emphasis.
  (dolist (marker '("*" "_"))
    (dolist (count '(1 3))
      (let* ((slashes (make-string count ?\\))
             (text (concat slashes marker "literal" slashes marker)))
        (should (equal (markdown-to-org--convert text) text))))))

(ert-deftest markdown-to-org-converts-emphasis-after-even-backslashes ()
  ;; Escaped backslashes do not escape the delimiter that follows them.
  (dolist (marker '("*" "_"))
    (dolist (count '(2 4))
      (let ((slashes (make-string count ?\\)))
        (should (equal (markdown-to-org--convert
                        (concat slashes marker "italic" slashes marker))
                       (concat slashes "/italic" slashes "/")))))))

(ert-deftest markdown-to-org-keeps-latex-and-code-with-surrounding-italic ()
  (let ((math "\\(x_i * y_j\\)")
        (code "\\(a_b * c_d\\)"))
    (should (equal (markdown-to-org--convert
                    (concat "*before* " math " `" code "` *after*"))
                   (concat "/before/ " math " =" code "= /after/")))))

(ert-deftest markdown-to-org-restores-latex-inside-protected-literals ()
  ;; Math is hidden first.  Restoring code/URLs must expose those tokens
  ;; before the math restoration pass, without leaving either token behind.
  (let ((math "\\(a_b * c_d\\)")
        (url "https://example.com/\\[a_b*c_d\\]/path_name_value"))
    (should
     (equal (markdown-to-org--convert
             (concat "```\n" math "\n```\n"
                     "`" math "`\n"
                     "[*label*](" url ")\n"
                     math " *outside*"))
            (concat "#+BEGIN_SRC\n" math "\n#+END_SRC\n"
                    "=" math "=\n"
                    "[[" url "][/label/]]\n"
                    math " /outside/")))))

(defconst markdown-to-org-test--emphasis-yank-input
  (concat "## **Heading**\n"
          "*one* *two*\n"
          "`*pointer* _name_`\n"
          "[*label*](https://example.com/a*b*c/path_name_value)\n"
          "\\(x_i * y_j\\)\n"
          "*last*")
  "Markdown exercising emphasis and protected literals through real yank.")

(defconst markdown-to-org-test--emphasis-yank-output
  (concat "** *Heading*\n"
          "/one/ /two/\n"
          "=*pointer* _name_=\n"
          "[[https://example.com/a*b*c/path_name_value][/label/]]\n"
          "\\(x_i * y_j\\)\n"
          "/last/")
  "Expected Org for `markdown-to-org-test--emphasis-yank-input'.")

(ert-deftest markdown-to-org-kill-ring-command-preserves-literals-and-italic ()
  (with-temp-buffer
    (org-mode)
    (insert "prefix\n")
    (let ((kill-ring (list markdown-to-org-test--emphasis-yank-input))
          (kill-ring-yank-pointer nil)
          (interprogram-paste-function nil))
      (call-interactively #'org-yank-from-kill-ring-from-md-to-org)
      (should (equal (buffer-string)
                     (concat "prefix\n"
                             markdown-to-org-test--emphasis-yank-output)))
      (should (equal (car kill-ring)
                     markdown-to-org-test--emphasis-yank-input)))))

(ert-deftest markdown-to-org-smart-yank-preserves-literals-and-italic ()
  (with-temp-buffer
    (org-mode)
    (insert "prefix\n")
    (let ((kill-ring (list markdown-to-org-test--emphasis-yank-input))
          (kill-ring-yank-pointer nil)
          (interprogram-paste-function nil)
          (already-advised
           (advice-member-p #'markdown-to-org--smart-yank-advice 'yank)))
      ;; Run actual yank under the installed advice, without reading the OS
      ;; clipboard or depending on another test having enabled the advice.
      (unwind-protect
          (progn
            (unless already-advised
              (advice-add 'yank :around #'markdown-to-org--smart-yank-advice))
            (call-interactively #'yank)
            (should (equal (buffer-string)
                           (concat "prefix\n"
                                   markdown-to-org-test--emphasis-yank-output)))
            (should (equal (car kill-ring)
                           markdown-to-org-test--emphasis-yank-input)))
        (unless already-advised
          (advice-remove 'yank #'markdown-to-org--smart-yank-advice))))))

(ert-deftest markdown-to-org-smart-yank-leaves-other-modes-unchanged ()
  (with-temp-buffer
    (fundamental-mode)
    (let ((kill-ring (list markdown-to-org-test--emphasis-yank-input))
          (kill-ring-yank-pointer nil)
          (interprogram-paste-function nil)
          (already-advised
           (advice-member-p #'markdown-to-org--smart-yank-advice 'yank)))
      (unwind-protect
          (progn
            (unless already-advised
              (advice-add 'yank :around #'markdown-to-org--smart-yank-advice))
            (call-interactively #'yank)
            (should (equal (buffer-string)
                           markdown-to-org-test--emphasis-yank-input)))
        (unless already-advised
          (advice-remove 'yank #'markdown-to-org--smart-yank-advice))))))

;;; markdown-to-org-test.el ends here
