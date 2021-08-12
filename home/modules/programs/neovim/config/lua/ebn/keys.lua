function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end

map('n', '<leader>fs', '<cmd>w<CR>', nil)
map('n', '<leader>ps', '<cmd>Telescope git_files<CR>', nil)
map('n', '<leader>ff', '<cmd>Telescope file_browser<CR>', nil)
map('n', '<leader>ca', '<cmd>lua vim.lsp.buf.code_action()<CR>', nil) 
