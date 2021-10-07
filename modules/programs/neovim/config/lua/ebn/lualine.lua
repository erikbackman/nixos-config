local config = {
  options = {
    icons_enabled = true,
    theme = 'nord',
    --require 'lualine/themes/aurora',
    component_separators = {'', ''},
    section_separators = {'', ''},
    disabled_filetypes = {}
  },
  sections = {
    lualine_a = {'filename'},
    lualine_b = {'location', 'progress'},
    lualine_c = {},
    lualine_x = {},
    lualine_y = {'filetype'},
    lualine_z = {'branch'},
  },
  inactive_sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'fileformat'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  },
  tabline = {},
  extensions = {}
}

require 'lualine'.setup(config)
