# git-messenger.el
*git-messenger.el* is Emacs port of [git-messenger.vim](https://github.com/rhysd/git-messenger.vim).


## screenshot

![git-messenger.el](image/git-messenger.png)


## Dependency

* [popup](https://github.com/auto-complete/popup-el)


## Basic Usage

### `git-messenger:popup-message`

Pop up last commit message at current line.


## Sample Configuration

```elisp
(require 'git-messenger)
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
```
