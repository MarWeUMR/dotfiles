" Source ----------------






" Plugins ---------------
call plug#begin('~/.config/nvim/plugged')

" Surround
  Plug 'tpope/vim-surround'
  Plug 'justinmk/vim-sneak'
  Plug 'morhetz/gruvbox'
  Plug 'unblevable/quick-scope'
  Plug 'neoclide/coc.nvim', {'branch': 'release'}
  Plug 'francoiscabrol/ranger.vim'
  Plug 'rbgrouleff/bclose.vim'
  "Plug 'joshdick/onedark.vim'
  Plug 'liuchengxu/vim-which-key'
  Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
  Plug 'junegunn/fzf.vim'
  Plug 'airblade/vim-rooter'
  Plug 'mhinz/vim-startify'
  Plug 'lervag/vimtex'
  "Plug 'sheerun/vim-polyglot'
  Plug 'jalvesaq/Nvim-R'
  Plug 'SirVer/ultisnips'
  "Plug 'honza/vim-snippets'
  "Plug 'Townk/vim-autoclose'
  "Plug 'vim-airline/vim-airline'
  Plug 'dense-analysis/ale'
  Plug 'Yggdroot/indentLine'



call plug#end()




source $HOME/.config/nvim/cfgs/coc.vim
source $HOME/.config/nvim/cfgs/rnvimr.vim
source $HOME/.config/nvim/general/settings.vim
source $HOME/.config/nvim/keys/mappings.vim
source $HOME/.config/nvim/cfgs/vim-rooter.vim
source $HOME/.config/nvim/cfgs/fzf.vim
"source $HOME/.config/nvim/themes/onedark.vim
source $HOME/.config/nvim/keys/which-key.vim
source $HOME/.config/nvim/cfgs/starty.vim
source $HOME/.config/nvim/cfgs/vimtex-cfg.vim

set background=dark
colorscheme  gruvbox

source $HOME/.config/nvim/cfgs/quickscope.vim
source $HOME/.config/nvim/cfgs/sneak.vim


let g:clipboard = {
  \   'name': 'xclip-xfce4-clipman',
  \   'copy': {
  \      '+': 'xclip -selection clipboard',
  \      '*': 'xclip -selection clipboard',
  \    },
  \   'paste': {
  \      '+': 'xclip -selection clipboard -o',
  \      '*': 'xclip -selection clipboard -o',
  \   },
  \   'cache_enabled': 1,
  \ }

let maplocalleader = "\\"



" R config
let R_rconsole_width = 0
let R_term = "tmux"
let R_notmuxconf = 1
let R_term_cmd = "tmux"

" latex

let g:tex_no_error=1
let g:vimtex_compiler_latexmk_engines = {
        \ '_'                : '-xelatex',
        \ 'pdflatex'         : '-pdf',
        \ 'dvipdfex'         : '-pdfdvi',
        \ 'lualatex'         : '-lualatex',
        \ 'xelatex'          : '-xelatex',
        \ 'context (pdftex)' : '-pdf -pdflatex=texexec',
        \ 'context (luatex)' : '-pdf -pdflatex=context',
        \ 'context (xetex)'  : '-pdf -pdflatex=''texexec --xtx''',
        \}


let g:ale_tex_latexindent_executable = 'latexindent'
let g:ale_fixers = {
\   '*': ['remove_trailing_lines', 'trim_whitespace'],
\   'tex': ['latexindent'],
\}
let g:ale_fix_on_save = 1

if (has("termguicolors"))
  set termguicolors
endif

if &term =~# '^screen'
    let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
    let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
endif


" indentation
let g:indentLine_char_list = ['|', '¦', '┆', '┊']


hi Normal guibg=NONE ctermbg=NONE
