# Applications

- htop
- inkscape
- gimp
- meld
- neovim
- pympress
- redshift
- vlc
- xcas
- zotero
- mupdf
- rlwrap
- ripgrep
- wine
- pdfshuffler
- fzf
- exa
- fex
- fd

## Pympress + i3

The default fullscreen behavior of `pympress` is annoying when using the window manager `i3`.

Using Ubuntu 20.04, the two ways to install `pympress` are via `apt` or by cloning the repository.

### Via `apt`

The [recommend way](https://github.com/Cimbali/pympress#installing-) is to install `pympress 1.5.1+dfsg-3build1`).

However, with `i3`, the Presenter window of `pympress` stays blank and nothing is loaded.

It happens when trying to set the Content window in fullscreen:

https://github.com/Cimbali/pympress/blob/60da16b766208087b41ec6c322b9d12e68577f45/pympress/ui.py#L350

It can be fixed by configuring `~/.config/pympress` with:
```
[content]
start_fullscreen = off

[presenter]
start_fullscreen = off
```

### Via `git clone`

In order to use a more recent version of `pympress` with Ubuntu 20.04, I clone the repository in `/opt` and used `direnv` + `pyenv` in order to locally install `python3.9` + `python -m pip install PyGObject`.

Then, I create the following script `~/.local/bin/pympress`:
```bash
#!/usr/bin/env bash

params=()
filename="$(readlink -f $1)"
[ -f "$filename" ] && params+=("$filename")
cd "/opt/pympress" && "/opt/pympress/.direnv/python-3.9.16/bin/python3.9" -m pympress ${params[@]}
```

Then, once `pympress` is launched, you can hit `F11`, to uncheck `Pympress presenter -> Starting configuration -> Content fullscreen`.
