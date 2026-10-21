# markdown-to-org

Convert Markdown text to Org-mode format automatically when yanking in Emacs. Perfect for users who work with both Markdown-based note-taking apps (like Obsidian) and Org-mode.

## Features

- Automatic conversion of Markdown to Org format when yanking
- Support for headers, lists, checkboxes, code blocks, and inline formatting
- Preserves nested structures and indentation
- Preserves code contents, link destinations, and escaped emphasis markers
- Preserves LaTeX math enclosed in `\[...\]` or `\(...\)` verbatim
- Works with both clipboard and kill-ring

## Installation

<!--
### MELPA (Coming Soon)

```elisp
(package-install 'markdown-to-org)
```
-->

### Manual Installation

Clone this repository and add to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/markdown-to-org")
(require 'markdown-to-org)
```

## Usage

### Basic Setup

```elisp
;; Enable automatic conversion globally
(markdown-to-org-advice-mode 1)

;; Or enable per-buffer
(markdown-to-org-mode 1)
```

### Available Commands

- `org-yank-from-clipboard-from-md-to-org`: Convert and paste Markdown from clipboard
- `org-yank-from-kill-ring-from-md-to-org`: Convert and paste Markdown from kill ring
- `markdown-to-org-mode`: Toggle automatic conversion for current buffer

### Default Keybindings

When `markdown-to-org-mode` is enabled:
- `C-c C-y c`: Convert and paste from clipboard
- `C-c C-y k`: Convert and paste from kill ring

## Examples

Input (Markdown):
```markdown
# Header
- [ ] Task 1
  - [x] Subtask
```

Output (Org):
```org
* Header
- [ ] Task 1
  - [X] Subtask
```

## Customization

```elisp
;; Toggle automatic conversion
(setq markdown-to-org-convert-on-yank t)
```

## Contributing

Contributions are welcome! Please feel free to submit a Pull Request.

Run the regression tests from the repository root:

```sh
emacs -Q --batch -L . -l test/markdown-to-org-test.el -f ert-run-tests-batch-and-exit
```

## License

This project is licensed under the MIT License.

## Acknowledgments

- Inspired by the need for seamless integration between modern note-taking apps and Org-mode
- Thanks to the Emacs community for feedback and suggestions
