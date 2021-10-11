set nomodeline
set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vimrc
colorscheme solarized8_high

" firenvim
nnoremap <Esc><Esc> :call firenvim#focus_page()<CR>
let g:firenvim_config = { 
    \ 'localSettings': {
        \ '.*': {
            \ 'priority': 0,
            \ 'takeover': 'never',
        \ },
    \ }
\ }

