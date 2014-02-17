CPU load and memory usage indicators
====================================

`nc-indicators` displays a CPU load and memory usage mini-graphs in
your `i3bar`, much like Gnome's `indicator-multiload` applet.

To use `nc-indicators` simply `exec` it on startup:

```
exec --no-startup-id nc-indicators
```

`nc-indicators` uses the standard system tray icon protocol, so it
can be used with any system tray-like application, not just
`i3bar`. But it was originally created to be used with `i3bar`.
