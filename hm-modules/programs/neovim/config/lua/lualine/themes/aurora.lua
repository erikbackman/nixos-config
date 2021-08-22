local aurora = {}

local colors = {
  pureblack = '#000000',
	black     = '#0C0F12',
	white     = '#e6e8ee',
	red       = '#b53f36',
	green     = '#5ccc96',
	blue      = '#62D2DB',
  blueAlt   = '#6a7b92',
	purple    = '#9d81ba',
	--yellow    = '#f2ce00',
	yellow    = '#e59d23', -- slightly more muted yellow
	gray      = '#686f9a',
	darkgray  = '#1E2029',
	lightgray = '#c1c3cc',
  cyan = '#0D9C94'
}

aurora.normal = {
  a = { bg = colors.darkgray, fg = colors.purple },
  b = { bg = colors.darkgray, fg = colors.white },
  c = { bg = colors.darkgray, fg = colors.white },
  x = { bg = colors.darkgray, fg = colors.white },
  y = { bg = colors.darkgray, fg = colors.yellow },
  z = { bg = colors.darkgray, fg = colors.cyan },
}

aurora.insert = {
  a = { bg = colors.darkgray, fg = colors.green },
  b = { bg = colors.darkgray, fg = colors.white },
  c = { bg = colors.darkgray, fg = colors.white },
  x = { bg = colors.darkgray, fg = colors.white },
  y = { bg = colors.darkgray, fg = colors.yellow },
  z = { bg = colors.darkgray, fg = colors.cyan },
}

aurora.visual = {
  a = { bg = colors.darkgray, fg = colors.yellow },
  b = { bg = colors.darkgray, fg = colors.white },
  c = { bg = colors.darkgray, fg = colors.white },
  x = { bg = colors.darkgray, fg = colors.white },
  y = { bg = colors.darkgray, fg = colors.yellow },
  z = { bg = colors.darkgray, fg = colors.cyan },
}

aurora.replace = {
  a = { bg = colors.darkgray, fg = colors.red },
  b = { bg = colors.darkgray, fg = colors.white },
  c = { bg = colors.darkgray, fg = colors.white },
  x = { bg = colors.darkgray, fg = colors.white },
  y = { bg = colors.darkgray, fg = colors.yellow },
  z = { bg = colors.darkgray, fg = colors.cyan },
}

aurora.command = {
  a = { bg = colors.darkgray, fg = colors.purple },
  b = { bg = colors.darkgray, fg = colors.white },
  c = { bg = colors.darkgray, fg = colors.white },
  x = { bg = colors.darkgray, fg = colors.white },
  y = { bg = colors.darkgray, fg = colors.yellow },
  z = { bg = colors.darkgray, fg = colors.cyan },
}

-- you can assign one colorscheme to another, if a colorscheme is
-- undefined it falls back to normal
aurora.terminal = aurora.normal

aurora.inactive = {
  a = { bg = colors.darkgray, fg = colors.purple },
  b = { bg = colors.darkgray, fg = colors.white },
  c = { bg = colors.darkgray, fg = colors.white },
  x = { bg = colors.darkgray, fg = colors.white },
  y = { bg = colors.darkgray, fg = colors.yellow },
  z = { bg = colors.darkgray, fg = colors.cyan },
}

return aurora
