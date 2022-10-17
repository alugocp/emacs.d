# Alex's Emacs setup
Yep, I'm an Emacs user now.

## Getting started
Make sure you delete your `~/.emacs` file or Emacs won't look for `~/.emacs.d/init.el`.
Run the following command to make this starter kit usable:

```sh
cp init.el.template init.el
```

## Todo
- [x] Delete selected text if you hit another non-navigation key
- [x] Add multiple tabs
- [x] Figure out running a terminal shell in this thing
- [x] Copy command should not deselect text
- [x] Implement horizontal scroll and remove line wrap
- [x] Make line select get all highlighted lines (not just the one with the cursor)
- [x] Allow you to jump to an arbitrary tab via `CMD + number` with consistent tab order
- [x] CMD + T to open new empty tab (scratch buffer)
- [x] Add file tree view (projectile)