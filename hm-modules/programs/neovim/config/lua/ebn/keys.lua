local function keymap(mode, lhs, rhs, opts)
  local options = { noremap = true, silent = true }
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

-- Misc
keymap('n', '<leader>qq', '<cmd>qa<CR>')
keymap('n', '<leader>ot', '<cmd>term<CR>')
keymap('n', 'Y', 'v$y')
keymap('n', '<C-Space>', '80lbi<CR><Esc>')

-- Buffer
keymap('n', '<leader>fs', '<cmd>w<CR>')
keymap('n', '<leader>bk', '<cmd>bd<CR>')
keymap('n', '<leader>eb', '<cmd>luafile %<CR>', { silent = false })
keymap('n', '<leader>bb', '<cmd>Buffers<CR>')
keymap('n', '<leader>sb', '<cmd>:BLines<CR>')

-- Project
keymap('n', '<leader>sp', '<cmd>Rg<CR>')

-- Files
keymap('n', '<leader>fp', '<cmd>FZF /home/ebn/.config/nvim/lua/<CR>')
keymap('n', '<leader>pp', '<cmd>FZF /home/ebn/repos/github.com/erikbackman/<CR>')
keymap('n', '<leader>pf', '<cmd>GitFiles<CR>')
keymap('n', '<leader>ff', '<cmd>Files %:p:h<CR>') -- Files in current file directory
keymap('n', '<leader>fd', '<cmd>NnnPicker %<CR>')
keymap('n', '<leader>fr', '<cmd>:History<CR>') -- Recent files

-- History
keymap('n', '<leader>h', '<cmd>:History:<CR>') -- Command history
keymap('n', '<leader>hh', '<cmd>:History/<CR>') -- Search history

-- Docs
keymap('n', '<leader>H', '<cmd>:Helptags<CR>')
