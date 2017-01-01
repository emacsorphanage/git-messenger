# git-messenger.el [![melpa badge][melpa-badge]][melpa-link] [![melpa stable badge][melpa-stable-badge]][melpa-stable-link]

`git-messenger.el` is Emacs port of [git-messenger.vim](https://github.com/rhysd/git-messenger.vim).

`git-messenger.el` provides function that popup commit message at current line.
This is useful when you want to know why this line was changed.


## Screenshot

![Screenshot of git-messenger.el](image/git-messenger.png)


## Installation

`git-messenger` is available on [MELPA](https://melpa.org/) and [MELPA stable](https://stable.melpa.org/)

You can install `git-messenger` with the following command.

<kbd>M-x package-install [RET] git-messenger [RET]</kbd>


## Dependency

* [popup](https://github.com/auto-complete/popup-el)
* [cl-lib](http://elpa.gnu.org/packages/cl-lib.html)


## Supported VCS

- Git
- Subversion
- Mercurial


## Commands

### `git-messenger:popup-message`

Pop up last commit message at current line. Show detail message, Commit ID, Author,
Date and commit message with `C-u` prefix

![Screenshot of git-messenger with prefix argument](image/git-messenger-detail.png)


## Key Bindings

You can modify key bindings by customizing `git-messenger-map`.

| Key                  | Command                                                 |
|:--------------------:|:--------------------------------------------------------|
| `M-w`                | Copy commit message and quit                            |
| `c`                  | Copy commit ID and quit                                 |
| `d`                  | Pop up `git diff` of last change of this line           |
| `s`                  | Pop up `git show --stat` of last change of this line    |
| `S`                  | Pop up `git show --stat -p` of last change of this line |
| `q`                  | Quit                                                    |


## Customize

### `git-messenger:show-detail`(Default `nil`)

Always show detail message if this value is `t`.

### `git-messenger:handled-backends`(Default `'(git svn)`)

Handled VCS which `git-messenger` uses.
Entries in this list will be tried in order to determine whether a
file is under that sort of version control.

### `git-messenger:use-magit-popup`(Default `nil`)

Use `magit-show-commit` instead of `pop-to-buffer`.

## Hooks

### `git-messenger:before-popup-hook`

Run before popup commit message. Hook function take one argument, commit message.

### `git-messenger:after-popup-hook`

Run after popup commit message. Hook function take one argument, commit message.

### `git-messenger:popup-buffer-hook`

Run after popup buffer.


## Global Variables

You may be able to use these variables useful in commands of `git-messenger-map`.

#### `git-messenger:last-message`

Last popup-ed commit message

#### `git-messenger:last-commit-id`

Last popup-ed commit ID


## Sample Configuration

```lisp
(require 'git-messenger) ;; You need not to load if you install with package.el
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)

(define-key git-messenger-map (kbd "m") 'git-messenger:copy-message)

;; Enable magit-show-commit instead of pop-to-buffer
(custom-set-variables
 '(git-messenger:use-magit-popup t))
```

[melpa-link]: https://melpa.org/#/git-messenger
[melpa-stable-link]: https://stable.melpa.org/#/git-messenger
[melpa-badge]: https://melpa.org/packages/git-messenger-badge.svg
[melpa-stable-badge]: https://stable.melpa.org/packages/git-messenger-badge.svg
