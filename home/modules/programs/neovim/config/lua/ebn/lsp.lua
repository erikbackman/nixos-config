local on_attach = function(client, bufnr)
    require('completion').on_attach()

    local function buf_set_keymap(...) vim.api.nvim_buf_set_keymap(bufnr, ...) end
    local function buf_set_option(...) vim.api.nvim_buf_set_option(bufnr, ...) end

    buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

    -- Mappings
    local opts = { noremap=true, silent=true }

    buf_set_keymap('n', 'gD', '<Cmd>lua vim.lsp.buf.declaration()<CR>', opts)
    buf_set_keymap('n', 'gd', '<Cmd>lua vim.lsp.buf.definition()<CR>', opts)
    buf_set_keymap('n', 'ca', '<Cmd>lua vim.lsp.buf.code_action<CR>', opts)
    buf_set_keymap('n', 'K', '<Cmd>lua vim.lsp.buf.hover()<CR>', opts)
    buf_set_keymap('n', 'gi', '<cmd>lua vim.lsp.buf.implementation()<CR>', opts)
    buf_set_keymap('n', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>', opts)
    buf_set_keymap('n', 'gr', '<cmd>lua vim.lsp.buf.references()<CR>', opts)
    buf_set_keymap('n', '[d', '<cmd>lua vim.lsp.diagnostic.goto_prev()<CR>', opts)
    buf_set_keymap('n', ']d', '<cmd>lua vim.lsp.diagnostic.goto_next()<CR>', opts)
end

-- require('nlua.lsp.nvim').setup(require('lspconfig'), { on_attach = on_attach, })
require('lspconfig').hls.setup({ on_attach = on_attach})
-- require('lspconfig').sumneko.setup({ on_attach = on_attach})

-- set the path to the sumneko installation; if you previously installed via the now deprecated :LspInstall, use
--local sumneko_root_path = vim.fn.stdpath('cache')..'/lspconfig/sumneko_lua/lua-language-server'
--local sumneko_binary = sumneko_root_path.."/bin/"..system_name.."/lua-language-server"

--local runtime_path = vim.split(package.path, ';')
--table.insert(runtime_path, "lua/?.lua")
--table.insert(runtime_path, "lua/?/init.lua")

-- require'lspconfig'.sumneko_lua.setup {
--  cmd = "lua-language-server",
--  settings = {
--    Lua = {
--      runtime = {
--        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
--        version = 'LuaJIT',
--        -- Setup your lua path
--        path = "/home/ebn/.cache/lspconfig/sumneko_lua/lua-language-server",
--      },
--      diagnostics = {
--        -- Get the language server to recognize the `vim` global
--        globals = {'vim'},
--      },
--      workspace = {
--        -- Make the server aware of Neovim runtime files
--        library = vim.api.nvim_get_runtime_file("", true),
--      },
--      -- Do not send telemetry data containing a randomized but unique identifier
--      telemetry = {
--        enable = false,
--      },
--    },
--  },
--}


-- WTF is this shitty language server!?
--require'lspconfig'.sumneko_lua.setup {
--	cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"};
--	settings = {
--		Lua = {
--			runtime = {
--				-- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
--				version = 'LuaJIT',
--				-- Setup your lua path
--				-- path = runtime_path,
--			},
--			diagnostics = {
--				-- Get the language server to recognize the `vim` global
--				globals = {'vim'},
--			},
--			workspace = {
--				-- Make the server aware of Neovim runtime files
--				library = vim.api.nvim_get_runtime_file("", true),
--			},
--			-- Do not send telemetry data containing a randomized but unique identifier
--			telemetry = {
--				enable = false,
--			},
--		},
--	};
--	on_attach=require'completion'.on_attach
--}
