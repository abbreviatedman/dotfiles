-- [[ Theme ]]
map('n', '<Leader>tn', ':lua toggle_night()<CR>')

-- [[ Buffers ]]
map('n', '<Leader><Tab>', ':b#<CR>')
map('n', '<Leader>bd', ':bd<CR>')
map('n', '<Leader>,', ':Telescope buffers<CR>')

-- [[ Files ]]
map('n', '<Leader>fs', ':w<CR>')
map('n', '<Leader><Leader>', ':Telescope find_files<CR>')
map('n', '<Leader>.', ':Telescope file_browser<CR>')
map('n', '<Leader>sp', ':Telescope live_grep<CR>')
map('n', '<Leader>so', ':Telescope grep_string<CR>')
map('n', '<Leader>fD', ':call delete(@%) | bdelete!<CR>')
map('n', '<Leader>op', ':NvimTreeToggle<CR>')

-- [[ Windows ]]
map('n', '<Leader>w', '<C-w>')
map('n', '<Leader>wd', '<C-w>c')

-- [[Text Editing]]
map('', 'f', '<Plug>Sneak_f', {noremap = false, silent = true})
map('', 'F', '<Plug>Sneak_F', {noremap = false, silent = true})
map('', '<Leader><Esc>', ':nohlsearch<CR>')

-- [[ Misc ]]
-- Turn off unintuitive U binding.
map('', 'U', 'u')
