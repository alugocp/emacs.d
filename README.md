# Alex's Emacs setup
Yep, I'm an Emacs user now.

## Getting started
Make sure you delete your `~/.emacs` file or Emacs won't look for `~/.emacs.d/init.el`.
Run the following command to make this starter kit usable:

```sh
cp init.el.template init.el
```

## Todo
- [x] Figure out how to indent by only tab-width spaces (at least for `typescript-mode`)
- [x] Make git gutter colors more vibrant
- [x] Don't have scratch buffer when opening files
- [x] Jesus stop asking me about closing modified buffers
- [ ] Leverage `(indent-region)` for CMD + ] (remap tab switching to avoid collision)
- [ ] Find some inverse of `(indent-region)` and map it to CMD + [
- [ ] CMD + left should stop at preceeding whitespace
- [ ] Backspace should take spaces 2 at a time (like real tabs)
- [ ] Find a good plugin for search + replace (should have regex and case sensitivity support, as well as project-wide search)
- [ ] CTRL + left or right should skip to the next word, but non-alphanumeric characters count as separate words from each other as long as they are different characters
- [ ] Pressing an arrow from a region skips to that end of the region (not where the cursor happens to be)
- [ ] Figure out how to reorient the project viewer to a new root directory
