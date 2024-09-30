# Installing i3wm with XFCE4

1. Follow this tuto: [feeblenerd](https://feeblenerd.blogspot.com/2015/11/pretty-i3-with-xfce.html) but **skip** section V and IX.

2. Custom [i3wm config](.config/i3/config) based on: [addy-dclxvi/i3-starterpack](https://github.com/addy-dclxvi/i3-starterpack).

3. Custom xfce4-panel:

[xfce4-panel illustration](xfce4-panel-illustration.png)

![xfce4-panel items](xfce4-panel-items.png)

The only non default item is [xfce4-wintitle-plugin](https://github.com/AdamYuan/xfce4-wintitle-plugin)

The clock is displayed with the `%F %a %R` format + font `Fira Sans ExtraBold` size `10`.

The style of the panel is defined in the [gtk.css](.config/gtk-3.0/gtk.css) file.

4. Install [Picom](https://github.com/yshui/picom) and use the following [config file](.config/picom/picom.conf).

5. Uninstall `dunst` to use `xfce4-notifyd`.

# Hacks for xfce4-* apps

## Appfinder

Selection using ctrl+j and ctrl+k.

Replace lines:

https://gitlab.xfce.org/xfce/xfce4-appfinder/-/blob/e483d04633a3072b40598fdf315d9a587450ad7a/src/appfinder-window.c?page=3#L2364-2371

With:

``` c
  if ((event->state & GDK_CONTROL_MASK) != 0)
    {
      if (event->keyval == GDK_KEY_j)
        return XFCE_APPFINDER_KEY_NEXT;

      if (event->keyval == GDK_KEY_k)
        return XFCE_APPFINDER_KEY_PREVIOUS;
    }
```


## Session logout

Display the machine's uptime instead of the user's name.

Replace line:

https://gitlab.xfce.org/xfce/xfce4-session/-/blob/5e1ca94d08f66e970182cb7996cb4a2a433067d8/xfce4-session/xfsm-logout-dialog.c#L202

With:

``` c
  FILE *fp = fopen("/proc/uptime", "r");
  if (fp == NULL) {
    label_str = g_strdup_printf(_("No uptime :'-("));
  } else {
    gulong uptime;
    if (!fscanf(fp, "%lu", &uptime)) {
      label_str = g_strdup_printf(_("No uptime :'-("));
    } else {
      gulong days = uptime / 60 / 60 / 24;
      gulong hours = uptime / 60 / 60 % 24;
      gulong minutes = uptime / 60 % 60;
      gulong seconds = uptime % 60;
      if (days > 0) {
        label_str = g_strdup_printf(_("Uptime: %lu days, %lu hours, %lu minutes"), days, hours, minutes);
      } else if (hours > 0) {
        label_str = g_strdup_printf(_("Uptime: %lu hours, %lu minutes"), hours, minutes);
      } else if (minutes > 0) {
        label_str = g_strdup_printf(_("Uptime: %lu minutes, %lu seconds"), minutes, seconds);
      } else {
        label_str = g_strdup_printf(_("Uptime: %lu seconds"), seconds);
      }
      fclose(fp);
    }
  }
```
