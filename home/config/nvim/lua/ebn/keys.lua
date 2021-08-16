local function keymap(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- General
keymap('n', '<leader>qq', '<cmd>qa<CR>', nil) 
keymap('n', '<leader>ot', '<cmd>term<CR>', nil) 

-- Buffer
keymap('n', '<leader>fs', '<cmd>w<CR>', nil)
keymap('n', '<leader>bk', '<cmd>bd<CR>', nil)
keymap('n', '<leader>eb', '<cmd>luafile %<CR>', { silent = false }) 
keymap('n', '<leader>bb', '<cmd>Buffers<CR>', nil)
keymap('n', '<leader>sb', '<cmd>:BLines<CR>', nil)  

-- Project
keymap('n', '<leader>sp', '<cmd>Rg<CR>', nil)

-- Files
keymap('n', '<leader>fp', '<cmd>FZF /home/ebn/.config/nvim/lua/<CR>', nil)
keymap('n', '<leader>pp', '<cmd>FZF /home/ebn/repos/github.com/erikbackman/<CR>', nil)
keymap('n', '<leader>pf', '<cmd>GitFiles<CR>', nil)
keymap('n', '<leader>ff', '<cmd>Files %:p:h<CR>', nil) -- Files in current file directory
keymap('n', '<leader>fd', '<cmd>NnnPicker %<CR>', nil)
keymap('n', '<leader>fr', '<cmd>:History<CR>', nil) -- Recent files

-- History 
keymap('n', '<leader>h', '<cmd>:History:<CR>', nil) -- Command history
keymap('n', '<leader>hh', '<cmd>:History/<CR>', nil) -- Search history

-- Docs
keymap('n', '<leader>H', '<cmd>:Helptags<CR>', nil)
