function map(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- General
map('n', '<leader>qq', '<cmd>qa<CR>', nil) 
map('n', '<leader>ot', '<cmd>term<CR>', nil) 

-- Buffer
map('n', '<leader>fs', '<cmd>w<CR>', nil)
map('n', '<leader>bk', '<cmd>bd<CR>', nil)
map('n', '<leader>eb', '<cmd>luafile %<CR>', nil) 
map('n', '<leader>bb', '<cmd>Buffers<CR>', nil)
map('n', '<leader>sb', '<cmd>:BLines<CR>', nil)  

-- Files
map('n', '<leader>fp', '<cmd>FZF /home/ebn/.config/nvim/lua/<CR>', nil)
map('n', '<leader>pp', '<cmd>FZF /home/ebn/repos/github.com/erikbackman/<CR>', nil)
map('n', '<leader>pf', '<cmd>GitFiles<CR>', nil)
map('n', '<leader>ff', '<cmd>Files<CR>', nil)
map('n', '<leader>fd', '<cmd>NnnPicker %<CR>', nil)
map('n', '<leader>fr', '<cmd>:History<CR>', nil) -- Recent files

-- History 
map('n', '<leader>h', '<cmd>:History:<CR>', nil) -- Command history
map('n', '<leader>hh', '<cmd>:History/<CR>', nil) -- Search history

-- Docs
map('n', '<leader>H', '<cmd>:Helptags<CR>', nil)
