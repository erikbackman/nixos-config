pkgs:

''
[module/xmonad]
type = custom/script
exec = ${pkgs.xmonad-log}/bin/xmonad-log
tail = true
interval = 1
''
