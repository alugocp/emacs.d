# Potion Emacs
![Verison](https://img.shields.io/badge/Version-1.0-blue)
![Emacs](https://img.shields.io/badge/Support-Emacs%2027+-blueviolet)

Potion Emacs is a starter kit for those who miss Atom and who will never forgive VSCode.
It knows what it did.

Anyways Potion Emacs bundles some git diff highlighting, a project viewer, a text finder, and many key bindings found in Atom and similar text editors.

<img src="./potion.png" height="400"/>

## Getting started
Make sure you delete your `~/.emacs` file or Emacs won't look for `~/.emacs.d/init.el`.
Run the following command to make this starter kit usable:

```sh
cp init.el.template init.el
```

You will also need to install the following command-line tools:
- [git](https://git-scm.com)
- [fzf](https://github.com/junegunn/fzf)

## Features

### Included packages

- [redo+](https://www.emacswiki.org/emacs/RedoPlus) makes it easier to undo/redo actions
- [typescript-mode](https://melpa.org/#/typescript-mode) adds TypeScript support
- [fuzzy-finder](https://melpa.org/#/fuzzy-finder) allows you to search the project for text and maybe replace it
- [diff-hl](https://melpa.org/#/diff-hl) highlights the gutter next to lines with uncommitted changes
- [neotree](https://melpa.org/#/neotree) powers the project viewer
- [magit](https://melpa.org/#/magit) provides some git support

### Keybindings
Potion Emacs allows you to replace the `CMD` key in these keybindings with another key.

|Keys|Function|
|---|---|
|SHIFT + DELETE|forward delete|
|CMD + LEFT|skip to beginning of line|
|CMD + RIGHT|skip to end of line|
|CMD + UP|skip to beginning of buffer|
|CMD + DOWN|skip to end of buffer|
|CMD + L|highlight current line|
|CMD + SHIFT + ENTER|open terminal|
|CMD + ENTER|open Emacs command line|
|CMD + ]|indent current line or region|
|CMD + [|outdent current line or region|
|CMD + F|search current buffer for text|
|CMD + SHIFT + F|search file(s) for text|
|CMD + R|replaces searched text|
|CMD + W|close the current tab or panel|
|CMD + Q|quit Emacs|
|CMD + S|save current buffer|
|CMD + SHIFT + S|save current buffer as|
|CMD + O|open a file|
|CMD + T|create new empty buffer|
|CMD + I|set indent width|
|CMD + SHIFT + I|reindents the currently selected region|
|CMD + A|highlight entire buffer|
|CMD + Z|undo|
|CMD + SHIFT + Z|redo|
|CMD + X|cut|
|CMD + C|copy|
|CMD + V|paste|
|CMD + \||toggle project tree|
|CMD + \<1 - 8\>|skip to *nth* tab in editor|
|CMD + 9|skip to the last tab in the editor|

### Customization
- `potion-emacs/initial-tab-width`: The initial tab width to set when opening this editor (default `2`)
- `potion-emacs/initial-screen-width`: The initial screen width to set when opening this editor (default `150`)
- `potion-emacs/initial-screen-height`: The initial screen height to set when opening this editor (default `50`)
- `potion-emacs/command-key`: The key to be used in place of CMD for keybindings (default `"C"`)
- `potion-emacs/terminal`: The terminal command to be run by Emacs (default `"/bin/zsh"`)
- `potion-emacs/indentation-variables`: A list of indentation variables to keep in sync (default `'()`)