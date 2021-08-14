local aurora = {}

local colors = {
	black = '#22252b'
	white = '#e6e8ee',
	red = '#b53f36'
	green = '#5ccc96',
	blue = '#00a3cc',
	purple = '#9d81ba'
	yellow = '#f2ce00',
	gray = '#686f9a',
	darkgray = '#30365F',
	lightgray = '#c1c3cc'

-- background: '#0C0F12'
-- blue: '#6a7b92'
-- cursor: '#f6f6ec'
-- foreground: '#e6e8ee'
-- green: '#5ab977'
-- purple: '#9d81ba'
-- red: '#b53f36'
-- selection_background: '#2e353d'
-- selection_foreground: '#1b1d22'
-- cyan: '#0D9C94'
-- white: '#e6e8ee'
-- yellow: '#ddb566'
-- cursorLine:     '#16172d'
-- # Coloration
-- pureWhite:      '#ffffff'
-- pureBlack:      '#000000'
-- grey:           '#818596'
-- grey2:          '#c1c3cc'
-- visualSel:      '#1b1c36'
-- orange:         '#e39400'
-- magenta:        '#ce6f8f'
-- azure:          '#62D2DB'
-- springGreen:    '#31E183'
-- green3:         '#7cf083'

}

aurora.normal = {
  -- gui parameter is optional and behaves the same way as in vim's highlight command
  a = {bg = colors.purple, fg = colors.black, gui = 'bold'},
  b = {bg = colors.darkgray, fg = colors.lightgray},
  c = {bg = colors.black, fg = colors.lightgray}
}

aurora.insert = {
  a = {bg = colors.green, fg = colors.black, gui = 'bold'},
  b = {bg = colors.darkgray, fg = colors.lightgray},
  c = {bg = colors.black, fg = colors.lightgray}
}

aurora.visual = {
  a = {bg = colors.yellow, fg = colors.black, gui = 'bold'},
  b = {bg = colors.darkgray, fg = colors.lightgray},
  c = {bg = colors.black, fg = colors.lightgray}
}

aurora.replace = {
  a = {bg = colors.purple, fg = colors.black, gui = 'bold'},
  b = {bg = colors.darkgray, fg = colors.lightgray},
  c = {bg = colors.black, fg = colors.lightgray}
}

aurora.command = {
  a = {bg = colors.blue, fg = colors.black, gui = 'bold'},
  b = {bg = colors.darkgray, fg = colors.lightgray},
  c = {bg = colors.black, fg = colors.lightgray}
}

-- you can assign one colorscheme to another, if a colorscheme is
-- undefined it falls back to normal
aurora.terminal = aurora.normal

aurora.inactive = {
  a = {bg = colors.black, fg = colors.lightgray, gui = 'bold'},
  b = {bg = colors.black, fg = colors.lightgray},
  c = {bg = colors.black, fg = colors.lightgray}
}

-- lualine.theme = aurora
return aurora
