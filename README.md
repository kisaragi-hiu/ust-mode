# ust-mode

An Emacs major mode for editing [UTAU Sequence Text](https://w.atwiki.jp/utaou/?cmd=word&word=ust&type=&pageid=21#id_a90784c7), the project file format for [UTAU](https://en.wikipedia.org/wiki/Utau).

This is intended to work alongside UTAU itself, as there are some tasks that can be done faster with a text editor, such as copy-and-pasting large sections of the project.

Syntax highlighting is provided, as well as a command, `ust-mode-normalize-paths`, to “normalize” paths within a UST: assuming the UST we're editing is named `main.ust`, it sets the output file to `main.wav`, caching directory to `main.cache`, and if the project name contains a dot (meaning it’s probably named after the file itself), to `main.ust`. This command is run everytime the file is saved, though I might make it a user option.

## Install

### With [`straight.el`](https://github.com/raxod502/straight.el)

```elisp
(straight-use-package '(ust-mode :host github :repo "kisaragi-hiu/ust-mode"))
```

### Manually

Put `ust-mode.el` under your `load-path`, then `(require 'ust-mode)` in your init file.

## License

GPL.
