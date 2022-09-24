OPTION = vim.opt
GLOBAL = vim.g

GLOBAL.mapleader = ' '

-- [[ Columns And Lines ]]
OPTION.relativenumber = true
OPTION.number = true
OPTION.scrolloff = 4
OPTION.signcolumn = "yes"

-- [[ Files ]]
OPTION.encoding = 'utf8'
OPTION.fileencoding = 'utf8'
OPTION.swapfile = false
OPTION.undofile = true

-- [[ Theme ]]
GLOBAL.t_co = 256
GLOBAL.background = "dark"
vim.api.nvim_set_var('modus_green_strings', 1)
vim.api.nvim_set_var('modus_yellow_comments', 1)
vim.api.nvim_set_var('modus_termtrans_enable', 1)
vim.api.nvim_set_var('modus_cursorline_intense', 1)
-- vim.cmd('colorscheme modus-vivendi')
OPTION.syntax = "ON"
OPTION.termguicolors = true
OPTION.cursorline = true

-- [[ Search ]]
OPTION.ignorecase = true
OPTION.smartcase = true
OPTION.incsearch = true
OPTION.hlsearch = true
OPTION.gdefault = true

-- [[ Whitespace ]]
OPTION.expandtab = true
OPTION.shiftwidth = 2
OPTION.softtabstop = 2
OPTION.tabstop = 2

-- [[ Splits ]]
OPTION.splitright = true
OPTION.splitbelow = true

-- [[ Miscellaneous ]]
OPTION.clipboard = "unnamedplus"
OPTION.completeopt = 'menuone,noselect'
