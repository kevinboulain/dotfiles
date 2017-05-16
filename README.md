## How to use

Simply clone this repository somewhere and symlink the interesting files or directories in your
home directory.

```shell
cd ~
git clone [--recursive] https://github.com/ether42/dotfiles.git
ln -s dotfiles/.some_file
```

These configuration files are mainly focused on terminal use.

## Programs

### tmux

#### Settings

 - light theming
 - the prefix key is set to `C-]` as `C-b` is usually reserved to move backward
 - on OS X, install `reattach-to-user-namespace` to correctly handle graphic programs

You may want to edit `.tmux/os/*.conf` for your specific use.

#### Installation

You have to symlink both `.tmux.conf` and `.tmux` as tmux seems to lack a way to resolve symlinks.

```shell
ln -s dotfiles/.tmux
ln -s dotfiles/.tmux.conf
```

### Bash

#### Settings

 - history is archived by date into `~/.bash_history.d`, you may use the `hist`
   command to recursively grep for strings in it
 - prompt is simple but show a `$` as user or `#` as root, the sigil is blue when last
   command exited 0 else red

You may want to edit `.bash/os/*.bash` for your specific use.

Note that if it exists, `.bash/personal.bash` is loaded last but is ignored by git.

#### Installation

Symlink the configuration files for the login and non-login shell.

```shell
ln -s dotfiles/.bash_profile
ln -s dotfiles/.bashrc
```

### Emacs

#### Settings

 - `*~` files are stored in `temporary-file-directory`
 - you may wish to tweak highlighting of white spaces, see `.emacs.d/ui.el`
 - `flycheck` may emit warnings or errors in your buffers if you have some linters available
 - for `htmlize` (useful for pretty code exports), use `htmlize-buffer` or set the mark two times
   by using `C-SPC` and move the point to delimit the region then use `htmlize-region`
 - for `flyspell`, install `hunspell` and some dictionaries (see `.emacs.d/spellcheck.el`),
   once the mode is enabled, use `M-$` to correct an highlighted word ([ispell documentation])

You may want to edit `.emacs.d/os/*.el` for your specific use.

Note that if it exists, `.emacs.d/personal.el` is loaded last but is ignored by git.

[ispell documentation]: https://www.gnu.org/software/emacs/manual/html_node/emacs/Spelling.html

#### Installation

Simply symlink `.emacs`, no need for `.emacs.d`.

```shell
ln -s dotfiles/.emacs
```

If you didn't recursively clone the repository, not much will be setup.

The Emacs modules are submodules as it's easier to fiddle with them.
As such, it's easy to add or remove features by simply initializing or deinitializing the
corresponding submodule.

For example, to choose a theme between the ones already available:
```shell
git submodule deinit .emacs.d/theme/monokai
git submodule update --init .emacs.d/theme/zenburn
```

### Git

#### Installation

Don't forget to correct the author configuration.

```shell
ln -s dotfiles/.gitconfig
```
