{ config, lib, ... }: {
  kittyLight = ''
    foreground  #000000
    background  #ffffff
    ## cursor
    cursor  #777777
    cursor_shape  underline
    cursor_text_color  background
    ## black
    color8  #555555
    color0  #222222
    ## red
    color9  #7f1010
    color1  #b60000
    ## green
    color10  #104410
    color2  #006800
    ## yellow
    color11  #5f4400
    color3  #904200
    ## blue
    color12  #002f88
    color4  #002f88
    #color4  #1111ee
    ## magenta
    color13  #752f50
    color5  #7000e0
    ## cyan
    color14  #12506f
    color6  #205b93
    ## white
    color15  #ffffff
    color7  #dddddd
    ## Tabs
    active_tab_foreground  #30169e
    active_tab_background  #f6f6f6
    inactive_tab_foreground  #282828
    inactive_tab_background  #bdbdbd
  '';

  kittyDark = ''
    active_tab_background #eee
    active_tab_foreground #000
    background #0C0F12
    color0 #22252b
    color1 #b53f36
    color10 #5ab977
    color11 #ddb566
    color12 #6a7b92
    color13 #9d81ba
    color14 #0D9C94
    color15 #ebedf2
    color2 #5ab977
    color3 #ddb566
    color4 #6a7b92
    color5 #9d81ba
    color6 #0D9C94
    color7 #e6e8ee
    color8 #22252b
    color9 #b53f36
    cursor #f6f6ec
    foreground #e6e8ee
  '';

  polybar = with config; lib.optionalString (lib.hasAttr "colors" config) ''
      [colors]
      background = ${colors.background}
      purple = ${colors.purple}
      white = ${colors.white}
      foreground = ${colors.foreground}
      foreground-alt = ${colors.foreground-alt}
      black = ${colors.black}    
      yellow = ${colors.yellow}
      alert = ${colors.yellow}
      accent = ${colors.accent}
      gray = ${colors.gray}
      bg-dark = ${colors.bg-dark}
  '';
}
