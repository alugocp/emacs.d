# Alex's Emacs setup
Yep, I'm an Emacs user now.

## Getting started
Make sure you delete your `~/.emacs` file or Emacs won't look for `~/.emacs.d/init.el`.
Run the following command to make this starter kit usable:

```sh
cp init.el.template init.el
```

You will also need to install the following command-line tools:
- [fzf](https://github.com/junegunn/fzf)

## Todo
- [x] Figure out how to indent by only tab-width spaces (at least for `typescript-mode`)
- [x] Make git gutter colors more vibrant
- [x] Don't have scratch buffer when opening files
- [x] Jesus stop asking me about closing modified buffers
- [x] Write my own `(indent-region)` for CMD + ] (remap tab switching to avoid collision)
- [x] Write an inverse of `(indent-region)` and map it to CMD + [
- [x] CMD + left should stop at preceding whitespace
- [x] Backspace should take spaces 2 at a time (like real tabs)
- [x] Find a good plugin for search + replace (should have regex and case sensitivity support, as well as project-wide search)
- [x] Figure out how to reorient the project viewer to a new root directory
- [ ] CTRL + left or right should skip to the next word, but non-alphanumeric characters count as separate words from each other as long as they are different characters
- [ ] Rebrand as my own Emacs configuration