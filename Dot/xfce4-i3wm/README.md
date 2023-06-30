# Installing i3wm with XFCE4

1. Follow this tuto: [feeblenerd](https://feeblenerd.blogspot.com/2015/11/pretty-i3-with-xfce.html) but **skip** section V and IX.

2. Custom [i3wm config](.config/i3/config) based on: [addy-dclxvi/i3-starterpack](https://github.com/addy-dclxvi/i3-starterpack).

3. Custom xfce4-panel:

![xfce4-panel illustration](xfce4-panel-illustration.png)

![xfce4-panel items](xfce4-panel-items.png)

~~The only non default item is [xfce4-windowck-plugin](https://github.com/cedl38/xfce4-windowck-plugin).~~

Replaced by [xfce4-namebar](https://github.com/HugLifeTiZ/xfce4-namebar-plugin).

The clock is displayed with the `%R` format.

The style of the panel is defined in the [gtk.css](.config/gtk-3.0/gtk.css) file.

Install `fonts-noto`.

The panel configuration is defined in [xfce4-panel-config.txt](xfce4-panel-config.txt) and can be loaded via the [xfce4-panel-profiles](https://docs.xfce.org/apps/xfce4-panel-profiles/start#examples) command.

4. Compton [config file](.config/.compton.conf) to have opacity support for `xfce4-notifyd`. Based on [Howto: Using Compton for tear-free compositing on XFCE or LXDE](https://ubuntuforums.org/showthread.php?t=2144468&p=12644745#post12644745).

5. Rofi [config file](.config/rofi/config).

# Custom i3wm bindings

| Binding        | Command         |
|----------------|-----------------|
| Super+²      | Mutes sound  |
| Super+e        | `emacs`           |
| Super+w        | `firefox`         |
| Alt+Ctrl+Shift+Right | [`go_empty.sh`](.config/i3/go_empty.sh)     |
| Alt+left       | [`move.sh -1`](.config/i3/move.sh) |
| Alt+right      | [`move.sh 1`](.config/i3/move.sh)  |
| Alt+Space      | `rofi`            |
| Super+f        | `thunar`          |
| Super+Shift+q   | `xfce4-session-logout`  |
| Super+Return   | `xfce4-terminal`  |
