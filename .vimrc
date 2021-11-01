" set leader key
let mapleader=" "
:set guifont=Fira_Code:h18
set updatetime=1000


function! ToggleHiddenAll()
    if s:hidden_all  == 0
        let s:hidden_all = 1
        set noshowmode
        set noruler
        set laststatus=0
        set noshowcmd
    else
        let s:hidden_all = 0
        set showmode
        set ruler
        set laststatus=2
        set showcmd
    endif
endfunction


nnoremap <leader>m :call ToggleHiddenAll()<CR>
let s:hidden_all = 1
set noshowmode
set noruler
set laststatus=0
set noshowcmd
" Keep undo history across sessions by storing it in a file
" The following creates the directory if it doesn't exist.
" And only for versions of vim that support persistent undo.
let vimDir = '$HOME/.vim'
let &runtimepath.=','.vimDir

if has('persistent_undo')
    let myUndoDir = expand(vimDir . '/undodir')
    " Create dirs
    call system('mkdir ' . vimDir)
    call system('mkdir ' . myUndoDir)
    let &undodir = myUndoDir
    set undofile
endif

set undodir=~/.vim/undodir

"spell check underline fix
set t_Cs=

" save redrawing the screen while macros and the like do their thing
" otherwise, things look a bit choppy
set lazyredraw

" sets syntax coloration on and the ability to have filetype plugins
filetype plugin on
syntax enable

" sets line numbers on
set number
" relative line numbers: super helpful!
set relativenumber

" highlight search matches
set hlsearch
" but do it incrementally
set incsearch
" ignore case when searching
set ignorecase
" unless we explicitly type an uppercased character
set smartcase


" Sets vim not to highlight matching pair in a way that can be visually
" confusing, making the user lose track of the cursor. There's certainly an
" argument to be made that we should get used to that, but... there are other
" ways to find matching parens when you need to, even without a plugin.
let g:loaded_matchparen = 1

" better colors
set background=dark
set cursorline
set termguicolors

" make tabs into spaces
set expandtab
" and make those tabs specifically two spaces
set shiftwidth=2
set softtabstop=2

" changes cursor based on insert vs. normal modes
" very useful visualization
let &t_SI = "\e[6 q"
let &t_EI = "\e[2 q"

" no 'bells' (aka beeps) of any kind:
set noeb vb t_vb=

" system clipboard integration
if has('clipboard')
  " using clipboard-capable vim
  set clipboard=unnamed
  if has('unnamedplus')
    " on X11 (linux)
    set clipboard+=unnamedplus
  endif
endif

" set leader key
nnoremap <SPACE> <Nop>
let mapleader=" "

" set proper line wrapping
set wrap linebreak nolist

" syntax highlighting for some file types
augroup syntax_highlighting
  autocmd!
  autocmd BufEnter *.html :syntax sync fromstart
  autocmd BufEnter *.js :syntax sync fromstart
augroup END

" adds vim-plug if it's not there
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

" Plugins for vim-plug to install.
"The above block will have installed vim-plug, and the below block will tell vim-plug which plugins to install.

call plug#begin()
  " add exporting to firenvim plugin
  " Plug 'glacambre/firenvim', { 'do': { _ -> firenvim#install(0) } }
  " exchange text objects
  Plug 'tommcdo/vim-exchange'
  " Drawing!
  Plug 'vim-scripts/DrawIt'
  " org mode
  Plug 'jceb/vim-orgmode'
  " Undo tree visualization
  Plug 'simnalamburt/vim-mundo'
  " fzf
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  " add indent guides
  Plug 'nathanaelkane/vim-indent-guides'
  " open link in browser
  Plug 'tyru/open-browser.vim'
  " auto-close XML/HTML tags
  Plug 'alvan/vim-closetag'
  " auto-close brackets
  Plug 'jiangmiao/auto-pairs'
  " title-casing
  Plug  'christoomey/vim-titlecase'
  " better syntax coloration for javascript
  Plug 'jelera/vim-javascript-syntax'
  " some alternative (not as good) syntax coloration helpers
  " Plug 'sheerun/vim-polyglot'
  " Plug 'pangloss/vim-javascript'
  " rainbow parentheses/bracket matching
  Plug 'luochen1990/rainbow'
  " provides a live server for markdown previews
  Plug 'iamcco/markdown-preview.vim'
  " comment toggling
  Plug 'tomtom/tcomment_vim'
  " Markdown goodies.
  Plug 'gabrielelana/vim-markdown'
  " jsx syntax coloration
  Plug 'maxmellon/vim-jsx-pretty'
  " case coercion
  Plug 'tpope/vim-abolish'
  " Uses an emacs-like kill ring
  " Plug 'maxbrunsfeld/vim-yankstack'
  " Two-character seek
  Plug 'justinmk/vim-sneak'
  " better substitution
  " Plug 'svermeulen/vim-subversive'
  " Adds repeatability for other plugins.
  Plug 'tpope/vim-repeat'
  " Adds surround as an action.
  Plug 'tpope/vim-surround'
  " Color theme collections
  Plug 'rodnaph/vim-color-schemes'
  Plug 'rainglow/vim'
  " Molokai theme
  Plug 'lucasprag/simpleblack'
  " Solarized color theme
  Plug 'altercation/vim-colors-solarized'
  " Another alternate solarized color theme
  Plug 'romainl/flattened'
  " Alternate solarized color theme
  Plug 'lifepillar/vim-solarized8'
  " linting
  Plug 'dense-analysis/ale'
  " version control in the gutter
  Plug 'airblade/vim-gitgutter'
  " Git integration
  " Plug 'tpope/vim-fugitive'
  " Hub integration
  " mostly for GBrowse
  Plug 'tpope/vim-rhubarb'
  " better statusline
  " Plug 'vim-airline/vim-airline'
  " Plug 'vim-airline/vim-airline-themes'
  " Smooth scrolling
  Plug 'mg979/scroll.vim'
  " completion engine through language servers
  " Plug 'neoclide/coc.nvim', { 'branch': 'release' }


  "" " Text objects!
  " "
  " " This one's needed to define the rest:
  Plug 'kana/vim-textobj-user'

  " " the rest:
  "
  " comment
  " mapping: ic
  Plug 'glts/vim-textobj-comment'
  " adds better text objects and seeking
  Plug 'wellle/targets.vim'
  " entire buffer
  " mapping: ie
  Plug 'kana/vim-textobj-entire'
  " better sentence text object
  " mapping: is (still)
  Plug 'reedes/vim-textobj-sentence'
  " global area (class, function, etc.)
  " mapping: iT
  Plug 'adolenc/vim-textobj-toplevel'
  " word within variable name
  " mapping: iv
  Plug 'Julian/vim-textobj-variable-segment'
  " uri
  " mapping: iu
  Plug 'jceb/vim-textobj-uri'
  " XML/HTML attribute
  " mapping: ix
  Plug 'whatyouhide/vim-textobj-xmlattr'
  " and from gitgutter:
  " ih for hunks
call plug#end()


" Plugin Configs

" mundo's undo visualization toggle
nnoremap <leader>u :MundoToggle<CR>

" color scheme
colorscheme simpleblack

" turns off coloration for the Sign Column
highlight clear SignColumn

" better mapping for vim-titlecase
let g:titlecase_map_keys = 0
nmap <leader>gt <Plug>Titlecase
vmap <leader>gt <Plug>Titlecase
nmap <leader>gT <Plug>TitlecaseLine

" fzf command to open files
" one more <C-T> will open the file in a new tab
nnoremap <C-T> :Files<CR>

" allows targets.vim to go as far as it can if given too high a count
let g:targets_gracious = 1

" gitgutter hunk text object mapping
omap ih <Plug>(GitGutterTextObjectInnerPending)
omap ah <Plug>(GitGutterTextObjectOuterPending)
xmap ih <Plug>(GitGutterTextObjectInnerVisual)
xmap ah <Plug>(GitGutterTextObjectOuterVisual)

" navigating the hunk text objects
nmap ]h <Plug>(GitGutterNextHunk)
nmap [h <Plug>(GitGutterPrevHunk)

" substitute over range
" substitutes for the first motion over the range of the second
" for the text entered in the prompt
" nmap s <plug>(SubversiveSubstituteRange)
" xmap s <plug>(SubversiveSubstituteRange)
" nmap ss <plug>(SubversiveSubstituteWordRange)

" sets the rainbow plugin to on by default--set to 0 if you want to enable it later via :RainbowToggle:
let g:rainbow_active = 1

" sets the rainbow plugin NOT to color html tags by depth.
" i find this very visually confusing.
let g:rainbow_conf = {
\  'separately': {
\    'html': 0
\  }
\}

" vim airline config

" display all buffers when only one tab is open
" let g:airline#extensions#tabline#enabled = 1
" display tab numbers (makes {count}gt very easy for picking a particular tab)
" let g:airline#extensions#tabline#tab_nr_type = 1
" show only half the file path... I think? lost to the sands of time
" let g:airline_section_c = '%.50F'
" don't show amount current positon in file
" let g:airline_section_z = ''

" fugitive bindings
nnoremap <Leader>gs :G<CR>
nnoremap <Leader>gc :G commit<CR>
nnoremap <Leader>gb :GBrowse<CR>

" Go to the link in your default browser.
nmap gx <Plug>(openbrowser-smart-search)
vmap gx <Plug>(openbrowser-smart-search)


""""""""

" mappings!

" save
nnoremap <Leader>s :w<CR>


" just... never use U, it's bad and annoying and bad
nnoremap U u

" add spaces above and below
" can take counts!
nnoremap <Leader>k O<Esc>1j
nnoremap <Leader>j o<Esc>1k

" Remove highlighting until next search.
nnoremap <Leader>/ :nohlsearch<CR>

" code to normally make k and j scroll down by VISUAL line (NOT logical line),
" while using a count makes it use LOGICAL lines
" this works very well with relativenumber
nnoremap <expr> k (v:count == 0 ? 'gk' : 'k')
nnoremap <expr> j (v:count == 0 ? 'gj' : 'j')

" Open new terminal window set to current directory.
" nnoremap <Leader>t :silent !kitty --detach<CR><CR>

" Open new terminal with fuzzy finder going.
" nnoremap <Leader>f :silent !kitty --detach vim $(fzf)<CR><CR>
"
" Open directory in VS Code.
nnoremap <Leader>c :silent !code $PWD<CR><CR>

" strip whitespace
nnoremap <Leader>dtw :%s/\s\+$//e<CR>

" accept first spelling suggestion
nmap <Leader>z 1z=

" copy and paste

" yankstack mappings
" let g:yankstack_map_keys = 0
" nmap <leader>p <Plug>yankstack_substitute_older_paste
" nmap <leader>P <Plug>yankstack_substitute_newer_paste
" call yankstack#setup()

" make Y behave more like the other capital variants and act on text until the
" end of the line
nmap Y y$

" macros

" Change function declaration to function expression.
let @f='0wxiwdhIconst " = ýa'
" Change function expression to arrow function.
" TODO
" Console log a label.
" TODO
" Console log out the value under the cursor, with label.
" TODO
" Console log out the value under the cursor, with label and colors
" (requires npm package 'colors').
" TODO

" strip whitespace
nnoremap <Leader>dtw :%s/\s\+$//e<CR>

let &t_8f = "\e[38;2;%lu;%lu;%lum"
let &t_8b = "\e[48;2;%lu;%lu;%lum"

" accept first spelling suggestion
nmap <Leader>z 1z=

" append file to notes
nnoremap <Leader>q ggvG$:w!>>~/Sync/org/notes.md<CR>ggdG

" sneak mapping
omap z <Plug>Sneak_s
omap Z <Plug>Sneak_S
nmap s <Plug>Sneak_s
nmap S <Plug>Sneak_S
map f <Plug>Sneak_f
map F <Plug>Sneak_F
map t <Plug>Sneak_t
map T <Plug>Sneak_T

""""""""
" many many MANY COC settings

" path to node
" let g:coc_node_path = '/usr/sbin/node'
"
" " TextEdit might fail if hidden is not set.
" set hidden
"
" " Some servers have issues with backup files, see #649.
" set nobackup
" set nowritebackup
"
" " Give more space for displaying messages.
" set cmdheight=2
"
" " Don't pass messages to |ins-completion-menu|.
" set shortmess+=c
"
" " Always show the signcolumn, otherwise it would shift the text each time
" " diagnostics appear/become resolved.
" set signcolumn=yes

" Use tab for trigger completion with characters ahead and navigate.
" " NOTE: Use command ':verbose imap <tab>' to make sure tab is not mapped by
" " other plugin before putting this into your config.
" inoremap <silent><expr> <TAB>
"       \ pumvisible() ? "\<C-n>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ coc#refresh()
" inoremap <expr><S-TAB> pumvisible() ? "\<C-p>" : "\<C-h>"
"
" function! s:check_back_space() abort
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~# '\s'
" endfunction
"
" " Trigger completion on the hovered word.
" inoremap <silent><expr> <Leader>lr coc#refresh()
"
" Use <cr> to confirm completion, `<C-g>u` means break undo chain at current
" position. Coc only does snippet and additional edit on confirm.
" <cr> could be remapped by other vim plugin, try `:verbose imap <CR>`.
" if exists('*complete_info')
"   inoremap <expr> <cr> complete_info()["selected"] != "-1" ? "\<C-y>" : "\<C-g>u\<CR>"
" else
"   inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
" endif
"
" " Use `[g` and `]g` to navigate diagnostics
" nmap <silent> [g <Plug>(coc-diagnostic-prev)
" nmap <silent> ]g <Plug>(coc-diagnostic-next)
"
" " GoTo code navigation.
" nmap <silent> gd <Plug>(coc-definition)
" nmap <silent> gy <Plug>(coc-type-definition)
" nmap <silent> gi <Plug>(coc-implementation)
" nmap <silent> gr <Plug>(coc-references)

" Use K to show documentation in preview window.
" " nnoremap <silent> K :call <SID>show_documentation()<CR>
" "
" function! s:show_documentation()
"   if (index(['vim','help'], &filetype) >= 0)
"     execute 'h '.expand('<cword>')
"   else
"     call CocAction('doHover')
"   endif
" endfunction
"
" " CoC  autocmds
" augroup cocgroup
"   autocmd!
"   autocmd FileType markdown let b:coc_suggest_disable = 1
  " autocmd FileType html let b:coc_suggest_disable = 1
  " " Highlight the symbol and its references when holding the cursor.
  " autocmd CursorHold * silent call CocActionAsync('highlight')
  " Setup formatexpr specified filetype(s).
"   autocmd FileType typescript,json setl formatexpr=CocAction('formatSelected')
"   " Update signature help on jump placeholder.
"   autocmd User CocJumpPlaceholder call CocActionAsync('showSignatureHelp')
" augroup end
"
" " Symbol renaming.
" nmap <leader>rn <Plug>(coc-rename)
"
" " Formatting selected code.
" xmap <leader>=  <Plug>(coc-format-selected)
" nmap <leader>= <Plug>(coc-format-selected)
"
" " Applying codeAction to the selected region.
" " Example: `<leader>aap` for current paragraph
" xmap <leader>a  <Plug>(coc-codeaction-selected)
" nmap <leader>a  <Plug>(coc-codeaction-selected)
"
" " Remap keys for applying codeAction to the current line.
" nmap <leader>ac  <Plug>(coc-codeaction)
" " Apply AutoFix to problem on the current line.
" nmap <leader>qf  <Plug>(coc-fix-current)

" Map function and class text objects
" NOTE: Requires 'textDocument.documentSymbol' support from the language server.
" xmap if <Plug>(coc-funcobj-i)
" omap if <Plug>(coc-funcobj-i)
" xmap af <Plug>(coc-funcobj-a)
" omap af <Plug>(coc-funcobj-a)
"
" " Use CTRL-S for selections ranges.
" " Requires 'textDocument/selectionRange' support of LS, ex: coc-tsserver
" nmap <silent> <C-s> <Plug>(coc-range-select)
" xmap <silent> <C-s> <Plug>(coc-range-select)
"
" " Add `:Format` command to format current buffer.
" command! -nargs=0 Format :call CocAction('format')

" Add `:Fold` command to fold current buffer.
" command! -nargs=? Fold :call     CocAction('fold', <f-args>)
"
" " Add `:OR` command for organize imports of the current buffer.
" command! -nargs=0 OR   :call     CocAction('runCommand', 'editor.action.organizeImport')

" Add (Neo)Vim's native statusline support.
" NOTE: Please see `:h coc-status` for integrations with external plugins that
" provide custom statusline: lightline.vim, vim-airline.
" set statusline^=%{coc#status()}%{get(b:,'coc_current_function','')}

" Mappings using CoCList:
" Show all diagnostics.
" nnoremap <silent> <Leader>la  :<C-u>CocList diagnostics<cr>
" " Manage extensions.
" nnoremap <silent> <Leader>le  :<C-u>CocList extensions<cr>
" " Show commands.
" nnoremap <silent> <Leader>lc  :<C-u>CocList commands<cr>
" " Find symbol of current document.
" nnoremap <silent> <Leader>lo  :<C-u>CocList outline<cr>
" " Search workspace symbols.
" nnoremap <silent> <Leader>ls  :<C-u>CocList -I symbols<cr>
" " Do default action for next item.
" nnoremap <silent> <Leader>lj  :<C-u>CocNext<CR>
" " Do default action for previous item.
" nnoremap <silent> <Leader>lk  :<C-u>CocPrev<CR>
" Resume latest coc list.
" nnoremap <silent> <Leader>lp  :<C-u>CocListResume<CR>
"
" nnoremap <silent> <Leader>y  :<C-u>CocList -A --normal yank<cr>
"
" let g:UltiSnipsExpandTrigger = "<nop>"
"
" inoremap <silent><expr> <TAB>
"       \ pumvisible() ? coc#_select_confirm() :
"       \ coc#expandableOrJumpable() ? "\<C-r>=coc#rpc#request('doKeymap', ['snippets-expand-jump',''])\<CR>" :
"       \ <SID>check_back_space() ? "\<TAB>" :
"       \ coc#refresh()
"
" function! s:check_back_space() abort
"   let col = col('.') - 1
"   return !col || getline('.')[col - 1]  =~# '\s'
" endfunction
"
" let g:coc_snippet_next = '<tab>'
