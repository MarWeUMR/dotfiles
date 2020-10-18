if exists('g:vscode')
  " Simulate same TAB behavior in VSCode
  nmap <Tab> :Tabnext<CR>
  nmap <S-Tab> :Tabprev<CR>
  inoremap kj <Esc>
else

  " remap escape
  inoremap jk <Esc>
  inoremap kj <Esc>

  " Use alt + hjkl to resize windows
  nnoremap <M-j>    :resize +2<CR>
  nnoremap <M-k>    :resize -2<CR>
  nnoremap <M-h>    :vertical resize -2<CR>
  nnoremap <M-l>    :vertical resize +2<CR>
 
  " TAB in general mode will move to text buffer
  nnoremap <TAB> :bnext<CR>
  " SHIFT-TAB will go back
  nnoremap <S-TAB> :bprevious<CR>

  " Better tabbing
  vnoremap < <gv
  vnoremap > >gv

  " Better window navigation
  nnoremap <C-h> <C-w>h
  nnoremap <C-j> <C-w>j
  nnoremap <C-k> <C-w>k
  nnoremap <C-l> <C-w>l




endif
