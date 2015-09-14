" TODO: and get yankring working properly
"       write a haskell indenter

autocmd!

set nocompatible

set shell=/usr/bin/zsh\ -l
let $PATH = $PATH . ':' . expand('~/.local/bin') . ':' . expand('~/.cabal/bin') . ':' . expand('~/.stack/programs/x86_64-linux/ghc-7.8.4/bin')


call plug#begin('~/.vim/plugged')

" Development
Plug 'isovector/ghci.vim'

" Libraries
Plug 'vim-scripts/L9'
Plug 'xolox/vim-misc'
Plug 'tomtom/tlib_vim'
Plug 'MarcWeber/vim-addon-mw-utils'
Plug 'kana/vim-submode'

" Silent
Plug 'rking/ag.vim'
Plug 'tpope/vim-repeat'
Plug 'majutsushi/tagbar'
Plug 'tpope/vim-characterize'
Plug 'vim-scripts/vim-lamdify'
Plug 'junegunn/rainbow_parentheses.vim'

if has("gui_running")
    Plug 'bling/vim-airline'
endif

" Navigation
Plug 'kien/ctrlp.vim'
" Plug 'nathanaelkane/vim-indent-guides'


" Misc
" Plug 'epeli/slimux'
Plug 'mattn/gist-vim'
Plug 'junegunn/goyo.vim'
Plug 'Shougo/neosnippet'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/vim-peekaboo'
Plug 'junegunn/limelight.vim'
Plug 'vim-scripts/tregisters'
" Plug 'airblade/vim-gitgutter'
Plug 'laurentgoudet/vim-howdoi'




" Formatting
Plug 'vim-scripts/vis'
Plug 'godlygeek/tabular'
Plug 'skwp/greplace.vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
Plug 'michaeljsmith/vim-indent-object'



" Languages
" Plug 'bitc/vim-hdevtools'
Plug 'raichoo/haskell-vim'
"Plug 'tpope/vim-markdown'
Plug 'tristen/vim-sparkup'
Plug 'vim-scripts/lua.vim'
Plug 'derekwyatt/vim-scala'
Plug 'vim-scripts/lua_indent'
Plug 'plasticboy/vim-markdown'
"Plug 'lukerandall/haskellmode-vim'
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'vim-scripts/latex-support.vim'
Plug 'jtratner/vim-flavored-markdown'


" Colors
Plug 'ptrr/phd-vim'
Plug 'ciaranm/inkpot'
Plug 'ajh17/Spacegray.vim'
Plug 'nanotech/jellybeans.vim'

" Textobjs
Plug 'tmhedberg/matchit'
Plug 'tpope/vim-surround'
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-syntax'
Plug 'Lokaltog/vim-easymotion'
Plug 'junegunn/vim-after-object'
Plug 'terryma/vim-expand-region'
Plug 'rbonvall/vim-textobj-latex'
Plug 'Julian/vim-textobj-variable-segment'

call plug#end()



" Leaders
let mapleader = " "
let maplocalleader = "_"

nnoremap <CR> :
nnoremap <S-CR> :<Up><CR>
nnoremap <C-CR> :!  <BS>
nnoremap <leader>g :Ag  <BS>
nnoremap <leader>m :make<CR>
nnoremap <leader>gp :Gpush<CR>
nnoremap <leader>gy :Goyo<CR>
nnoremap <leader>f :CtrlP<CR>
nnoremap <leader>gs :Gstatus<CR>
nnoremap \<space> :noh<cr>
nnoremap <leader>sv :vert sb  <BS>
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>el :e ~/.vimrc.local<cr>
nnoremap <leader>ex :e ~/.xmonad/src/xmonad.hs<cr>
nnoremap <leader>ez :e ~/.zshrc.local<cr>
nnoremap <leader>cp :cd ~/Projects/
nnoremap <leader>l :TagbarToggle<CR>
noremap  <leader>mv :call RenameFile()<cr>
nnoremap <leader>pf V:! pointfree "`cat`"<CR>==
nnoremap <leader>pg o<ESC>"+pkddA'<ESC>0iPlug '<ESC>0
vnoremap <leader><leader>c :!octave --silent \| cut -c8-<cr>
nnoremap <leader>ee <C-w><C-v><C-l>:e ~/.notebook.db<cr>:vertical resize 84<cr>
nnoremap <leader>sca V:s/\(\)/\1\r/<Left><Left><Left><Left><Left><Left><Left><Left>
nnoremap <leader>scb V:s/\(\)/\r\1/<Left><Left><Left><Left><Left><Left><Left><Left>
vnoremap <leader>sl :sort<Cr>gv:! awk '{ print length(), $0 \| "sort -n \| cut -d\\  -f2-" }'<CR>
vmap     <leader>b :<C-U>!git blame <C-R>=expand("%:p") <CR> \| sed -n <C-R>=line("'<") <CR>,<C-R>=line("'>") <CR>p <CR>

" Useful inserts
inoremap \fn <c-r>=expand('%:t:r')<cr>
inoremap \dt <esc>:r! date "+\%Y-\%m-\%d \%H:\%m"<cr>kJA  <BS>


" Better normal behavior
noremap  za <nop>
nnoremap vv vip
vnoremap : :B  <BS>
nnoremap ;; :w<CR>
nnoremap :ws :w !sudo tee %<cr>
nnoremap / /\v
nnoremap : <nop>
inoremap <F1> <nop>
nnoremap <F1> <nop>
vnoremap <F1> <nop>
inoremap <t_%9> <nop>
nnoremap <C-q> <C-W><C-j><C-W><C-o>
nnoremap gb :ls<CR>:b<Space>
nnoremap gH :e %<.h<CR>
nnoremap gC :e %<.cc<CR>

" Unmapped
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
nnoremap Q <nop>

nnoremap <silent> <F11> :YRShow<CR>


"inoremap <up> <nop>
"inoremap <down> <nop>
"inoremap <left> <nop>
"inoremap <right> <nop>

nnoremap <up> <nop>
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
nnoremap j gj
nnoremap k gk

nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

vnoremap <C-c> "+yi
inoremap <C-v> <C-r><C-o>+
" imap <Del> <nop>
" imap <up> <nop>
" imap <down> <nop>
" imap <left> <nop>
" imap <right> <nop>

noremap <silent> <C-F9>  :vertical resize -10<CR>
noremap <silent> <C-F10> :resize +10<CR>
noremap <silent> <C-F11> :resize -10<CR>
noremap <silent> <C-F12> :vertical resize +10<CR>


" Wildmenu completion
set wildmenu
set wildmode=list:longest
set wildignore+=.hg,.git,.svn
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.spl
set wildignore+=*.sw?
set wildignore+=*.luac
set wildignore+=*.pyc
set wildignore+=*.orig
set wildignore+=*.hi



" Jump back to same line when reopening
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ execute 'normal! g`"zvzz' |
        \ endif
augroup END



" Removing scrollbars
if has("gui_running")
    set guitablabel=%-0.12t%M
    au BufAdd * :RainbowParentheses
else
    set t_Co=256
endif
set guioptions=ac





" Common autocmds
au VimResized * :wincmd =
au BufWritePre * :%s/\s\+$//e
au BufWritePre *.scala :SortScalaImports
au bufwritepost .vimrc source ~/.vimrc

autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR>
autocmd CmdwinEnter * nnoremap <buffer> <CR> <CR>



" Filetype setting
au BufRead,BufNewFile *.db setfiletype db
au BufRead,BufNewFile *.md setfiletype ghmarkdown
au BufRead,BufNewFile *.markdown setfiletype ghmarkdown
au BufRead,BufNewFile *.htex setfiletype htex
filetype on
filetype plugin indent on



" Start interactive EasyAlign in visual mode (e.g. vip<Enter>)
vmap <Enter> <Plug>(EasyAlign)

set cc=81,121

" Markdown settings
let g:vim_markdown_initial_foldlevel=1
let g:vim_markdown_math=1
let g:vim_markdown_frontmatter=1

" Rainbow colors
let g:rainbow#max_level = 16
let g:rainbow#pairs = [['(', ')'], ['[', ']']]
let g:rainbow#colors = {
\   'dark': [
\     ['yellow',  'orange1'     ],
\     ['green',   'yellow1'     ],
\     ['cyan',    'greenyellow' ],
\     ['magenta', 'green1'      ],
\     ['red',     'springgreen1'],
\     ['yellow',  'cyan1'       ],
\     ['green',   'slateblue1'  ],
\     ['cyan',    'magenta1'    ],
\     ['magenta', 'purple1'     ]
\   ] }

let g:ctrlp_custom_ignore = 'target/\|dist/\|\_site/'

set wrap
set ruler
set title
set hidden
set number
set mouse=a
set showcmd
set ttyfast
set gdefault
set relativenumber
set hlsearch
set modeline
set showmode
set wildmenu
set expandtab
set incsearch
set showmatch
set smartcase
set tabstop=4
set autoindent
set grepprg=ag
set ignorecase
set lazyredraw
set shiftround
set visualbell
set matchtime=3
set modelines=2
set scrolloff=8
set laststatus=2
set nofoldenable
set shiftwidth=4
set softtabstop=4
set encoding=utf-8
set timeoutlen=500
set formatoptions=qrn1
set wildmode=list:longest
set tags=./tags,tags,../tags
set backspace=indent,eol,start
set list listchars=tab:»·,trail:·
set dictionary=/usr/share/dict/words
set backupskip=/tmp/*,/private/tmp/*"

" set cpoptions=ce$


set background=dark
" colo inkpot
" colo spacegray
" colo phd
colo jellybeans


let g:grep_cmd_opts = '--line-numbers --noheading'

function! RenameFile()
    let old_name = expand('%')
    let new_name = input('New file name: ', expand('%'), 'file')
    if new_name != '' && new_name != old_name
        exec ':saveas ' . new_name
        exec ':silent !rm ' . old_name
        redraw!
    endif
endfunction

if executable('ag')
  set grepprg=ag\ --nogroup\ --nocolor
  let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
  let g:ctrlp_use_caching = 0
endif

syntax on

let g:haskell_indent_do = 3
let g:haskell_indent_if = 3
let g:haskell_indent_in = 1
let g:haskell_indent_let = 4
let g:haskell_indent_case = 2
let g:haskell_indent_where = 6
let g:haddock_browser="/usr/bin/sensible-browser"
au FileType haskell set cc=81

function! s:goyo_enter()
    Limelight
    augroup relnum
        au!
    augroup END
endfunction

function! s:goyo_leave()
    augroup relnum
        au!
        au FocusLost * :set norelativenumber
        au FocusGained * :set relativenumber
        au InsertEnter * :set norelativenumber
        au InsertLeave * :set relativenumber
    augroup END

    if exists(":Limelight")
        Limelight!
    endif
endfunction

autocmd User GoyoEnter nested call <SID>goyo_enter()
autocmd User GoyoLeave nested call <SID>goyo_leave()

call <SID>goyo_leave()

autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ')

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0
let g:syntastic_quiet_messages = { "type": "warning" }

let g:easy_align_delimiters = {
\ '[': { 'pattern': '[[\]]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
\ '(': { 'pattern': '[()]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
\ ']': { 'pattern': '[[\]]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ ')': { 'pattern': '[()]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ '<': { 'pattern': '[<]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ '>': { 'pattern': '[->]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ }

let g:gist_detect_filetype = 1

if has("gui_running")
    let g:airline#extensions#tabline#enabled = 1
    let g:airline#extensions#tabline#buffer_nr_show = 1
    set showtabline=2

    let g:airline_left_sep = ''
    let g:airline_left_alt_sep = ''
    let g:airline_right_sep = ''
    let g:airline_right_alt_sep = ''
endif

" let g:ctrlp_map = '<leader>f'


" easy buffer switching

function! s:bufSwitch(count)
    if count ># 1
        return ":" . count . "b"
    endif
    return 't'
endfunction
nnoremap <expr> t <SID>bufSwitch(v:count1)

" wipeout buffer 1
au VimEnter * if v:progname ==# "gvim" && expand('%') ==# "" |
                \ execute "normal! ihello\<ESC>:bw!\<CR>" |
                \ endif

source ~/.vimrc.local

