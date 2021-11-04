{
  paletteToKitty = p: ''
    active_tab_background #eee
    active_tab_foreground #000
    background ${p.background}
    color0 ${p.black}
    color1 ${p.red}
    color10 ${p.green}
    color11 ${p.yellow}
    color12 ${p.blue}
    color13 ${p.purple}
    color14 ${p.cyan}
    color15 ${p.white}
    color2 ${p.green}
    color3 ${p.yellow}
    color4 ${p.blue}
    color5 ${p.purple}
    color6 ${p.cyan}
    color7 ${p.white}
    color8 ${p.black}
    color9 ${p.red}
    cursor ${p.white}
    foreground ${p.foreground}
  '';

  paletteToPolybar = p: ''
    [colors]
    background = ${p.background}
    purple = ${p.purple}
    white = ${p.white}
    foreground = ${p.foreground}
    foreground-alt = ${p.foreground-alt}
    black = ${p.black}
    yellow = ${p.yellow}
    alert = ${p.yellow}
    accent = ${p.accent}
    gray = ${p.gray}
    bg-dark = ${p.bg-dark}
  '';
}
