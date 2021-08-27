" Some sensible defaults before we load the lua configuration
"
set path+=**
if exists('+termguicolors')
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
    set termguicolors
endif

colorscheme aurora

set termguicolors
set relativenumber
set tabstop=2 softtabstop=2
set shiftwidth=2
set expandtab
set smartindent
set nu
set termguicolors
set scrolloff=8
set signcolumn=yes
set cmdheight=1
set incsearch

let mapleader = " "
inoremap <expr> <Tab>   pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"

" Clear search on esc
nnoremap <esc> :noh<return><esc>

" Set completeopt to have a better completion experience
set completeopt=menuone,noinsert,noselect

" Avoid extra message when using completion
set shortmess+=c

" let g:completion_enable_auto_popup = 0
let g:completion_matching_strategy_list = ['exact', 'substring', 'fuzzy', 'all']

" only show recent files from current working directory on startup screen
let g:startify_lists = [ { 'type': 'dir', 'header': ['MRU'. getcwd()] } ]

autocmd TermOpen * startinsert

" load lua configuration
lua require("ebn")
#let g:aniseed#env = v:true
