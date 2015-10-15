autocmd!
set nocompatible

if !has('nvim')
  set shell=/usr/bin/zsh\ -l
endif

" ------------------------------------------------------------------------------
                               " Path Extensions
" ------------------------------------------------------------------------------
let $PATH = $PATH . ':' . expand('~/.local/bin')
let $PATH = $PATH . ':' . expand('~/.cabal/bin')
let $PATH = $PATH . ':' . expand('~/.stack/programs/x86_64-linux/ghc-7.8.4/bin')



" ------------------------------------------------------------------------------
                              " Plugin Management
" ------------------------------------------------------------------------------
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
Plug 'ap/vim-buftabline'

" Navigation
Plug 'kien/ctrlp.vim'

" Misc
Plug 'mattn/gist-vim'
Plug 'junegunn/goyo.vim'
Plug 'Shougo/neosnippet'
Plug 'tpope/vim-fugitive'
Plug 'junegunn/vim-peekaboo'
Plug 'junegunn/limelight.vim'
Plug 'vim-scripts/tregisters'
Plug 'isovector/vim-howdoi'

" Formatting
Plug 'vim-scripts/vis'
Plug 'godlygeek/tabular'
Plug 'skwp/greplace.vim'
Plug 'tpope/vim-abolish'
Plug 'tpope/vim-commentary'
Plug 'junegunn/vim-easy-align'
Plug 'michaeljsmith/vim-indent-object'

" Languages
Plug 'raichoo/haskell-vim'
Plug 'tristen/vim-sparkup'
Plug 'vim-scripts/lua.vim'
Plug 'derekwyatt/vim-scala'
Plug 'vim-scripts/lua_indent'
Plug 'plasticboy/vim-markdown'
Plug 'PotatoesMaster/i3-vim-syntax'
Plug 'vim-scripts/latex-support.vim'
Plug 'jtratner/vim-flavored-markdown'

" Colors
Plug 'ptrr/phd-vim'
Plug 'ciaranm/inkpot'
Plug 'ajh17/Spacegray.vim'
Plug 'nanotech/jellybeans.vim'
Plug 'ronny/birds-of-paradise.vim'
Plug 'ratazzi/blackboard.vim'
Plug 'justincampbell/vim-railscasts'
Plug 'vim-scripts/leo256'
Plug 'w0ng/vim-hybrid'

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
Plug 'deris/vim-shot-f'
Plug 'wellle/targets.vim'
Plug 'michaeljsmith/vim-indent-object'

call plug#end()



" ------------------------------------------------------------------------------
                                   " Leaders
" ------------------------------------------------------------------------------
let mapleader = " "
let maplocalleader = ","

" Automatic pane split layouts
nnoremap <leader>3 :vnew<CR>:bn<CR>:vnew<CR>:bn<CR>
nnoremap <leader>4 :vnew<CR>:bn<CR>:vnew<CR>:bn<CR><C-W><C-L><C-W><C-L>:split<CR>:bn<CR>
nnoremap <leader>sv :vert sb  <BS>

" Easier access to commands
nnoremap <leader>m :make<CR>
nnoremap <leader>g :grep!  <BS>
nnoremap <leader>f :CtrlP<CR>
nnoremap <leader>l :TagbarToggle<CR>

" Todo management
nmap     <leader>td OTODO(sandy): <ESC>gccA
nnoremap <silent> <leader>wtd :lgrep! TODO.sandy<CR>:lw<CR>

" New functionality
noremap  <leader>mv :call RenameFile()<cr>
vnoremap <leader><leader>c :!octave --silent \| cut -c8-<cr>
nnoremap <leader>sla V:s/\(\)/\1\r/<Left><Left><Left><Left><Left><Left><Left><Left>
nnoremap <leader>slb V:s/\(\)/\r\1/<Left><Left><Left><Left><Left><Left><Left><Left>
vnoremap <leader>sl :sort<Cr>gv:! awk '{ print length(), $0 \| "sort -n \| cut -d\\  -f2-" }'<CR>



" ------------------------------------------------------------------------------
                                " Local Leaders
" ------------------------------------------------------------------------------
" TODO(sandy): Add infrastructure to only enable these in certain filetypes

" No distractions for writing
nnoremap <leader>gy :Goyo<CR>

" Insert new vim plugin line from system clipboard
nnoremap <leader>pg o<ESC>"+pkddA'<ESC>0iPlug '<ESC>0

" Create banners in vimrc
nmap <localleader>wb O<ESC>78i-<ESC>gccyyjpk<CR>center<CR>gcc

" Transform haskell line into a pointfree version
nnoremap <leader>pf V:! pointfree "`cat`"<CR>==



" ------------------------------------------------------------------------------
                              " Quick Directories
" ------------------------------------------------------------------------------
nnoremap <leader>cd :cd %:p:h<CR>
nnoremap <leader>cp :cd ~/Projects/
nnoremap <leader>ct :cd ~/.tino/
nnoremap <leader>cz :cd ~/.tino/zsh/



" ------------------------------------------------------------------------------
                                 " Quick Edits
" ------------------------------------------------------------------------------
nnoremap <leader>ev :e $MYVIMRC<cr>
nnoremap <leader>et :e ~/.tino/bin/tino<cr>
nnoremap <leader>el :e ~/.vimrc.local<cr>
nnoremap <leader>ea :e ~/.tino/zsh/aliases.zsh<cr>
nnoremap <leader>ex :e ~/.xmonad/src/xmonad.hs<cr>
nnoremap <leader>eo :e ~/one-liners<cr>
nnoremap <leader>ez :e ~/.zshrc.local<cr>
nnoremap <leader>ee <C-w><C-v><C-l>:e ~/.notebook.db<cr>:vertical resize 84<cr>
nnoremap <leader>ep :call EditPcfbFile()<cr>

" Switch to header or c file
nnoremap gH :e %<.h<CR>
nnoremap gC :e %<.cc<CR>

function! EditPcfbFile()
  let file = system("date +'%Y-%m-%d'")
  let file = strpart(file, 0, len(file) - 1)
  execute ":e ~/.tino/var/" . file . ".txt"
endfunction



" ------------------------------------------------------------------------------
                                " Quick Inserts
" ------------------------------------------------------------------------------
inoremap \fn <c-r>=expand('%:t:r')<cr>
inoremap \dt <esc>:r! date "+\%Y-\%m-\%d \%H:\%m"<cr>kJA  <BS>



" ------------------------------------------------------------------------------
                           " Better Default Bindings
" ------------------------------------------------------------------------------
" Remove dumb bindings
noremap  za <nop>
inoremap <F1> <nop>
nnoremap <F1> <nop>
vnoremap <F1> <nop>
map <t_%9> <nop>
nnoremap Q <nop>
nnoremap * *N
nnoremap Y y$

" Better bindings
nnoremap : <nop>
nnoremap <CR> :
vnoremap <CR> :B  <BS>
nnoremap / /\v
nnoremap <C-q> <C-W><C-q>
nnoremap K :grep! "\b<C-R><C-W>\b"<CR>:cw<CR>
vnoremap < <gv
vnoremap > >gv
nnoremap go o<esc>
nnoremap gO O<esc>

" Insert mode bindings
" inoremap <C-I> <C-O>^
" inoremap <C-A> <C-O>$

" Swap marker commands
nnoremap `` ``
nnoremap ' `
nnoremap ` '

" Switch to alt file
nnoremap # :e #<CR>

" Rename
" TODO(sandy): make a bufdo version of this
nnoremap S *N:noh<CR>:%s//

" Free keys:
" K, !?, &, \, Zx, Q


" ------------------------------------------------------------------------------
                             " Convenience Bindings
" ------------------------------------------------------------------------------
nnoremap vv vip

" Rerun last ex command
nnoremap <S-CR> :<Up><CR>

" Run shell command
nnoremap <C-CR> :!  <BS>

" Close buffer without changing layout
nnoremap :: :bp\|bd #<CR>

" Delete with no yank stack
nnoremap <silent> <leader>d "_d
vnoremap <silent> <leader>d "_d

" Save file
nnoremap ;; :w<CR>
inoremap <c-s> <esc>:w<CR>a
nnoremap <c-s> :w<CR>
vnoremap <c-s> :<c-u>w<CR>

" Pop tag stack
" nnoremap <C-[> <ESC>:po<CR>

" Remove search highlighting
nnoremap <silent> <s-space> :noh<cr>

" Help align visual blocks by delimiter
vmap a <Plug>(EasyAlign)

" Things we can align on
let g:easy_align_delimiters = {
\ '[': { 'pattern': '[[\]]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
\ '(': { 'pattern': '[()]', 'left_margin': 0, 'right_margin': 0, 'stick_to_left': 0 },
\ ']': { 'pattern': '[[\]]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ ')': { 'pattern': '[()]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ '<': { 'pattern': '[<]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ '>': { 'pattern': '[->]', 'left_margin': 1, 'right_margin': 0, 'stick_to_left': 0 },
\ }



" ------------------------------------------------------------------------------
                                 " Gnarly Shit
" ------------------------------------------------------------------------------
" Sudo save file.
cmap w!! %!sudo tee > /dev/null %

" Change property to dict lookup (a.x -> a['x'])
nmap cod mmysiw]hxlysw'`ml

" Change dict lookup to propert (a['x'] -> a.x)
nmap cop mmds'ds]i.<ESC>`mh



" ------------------------------------------------------------------------------
                              " Movement Bindings
" ------------------------------------------------------------------------------
" Cursor Movement
nnoremap <up> <nop>
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
nnoremap j gj
nnoremap k gk
vnoremap j gj
vnoremap k gk

" Window movement
nnoremap <C-J> <C-W><C-J>
nnoremap <C-K> <C-W><C-K>
nnoremap <C-L> <C-W><C-L>
nnoremap <C-H> <C-W><C-H>

" Window resizing
noremap <silent> <C-F9>  :vertical resize -10<CR>
noremap <silent> <C-F10> :resize +10<CR>
noremap <silent> <C-F11> :resize -10<CR>
noremap <silent> <C-F12> :vertical resize +10<CR>

" Buffer list
nnoremap gb :ls<CR>:b<Space>



" ------------------------------------------------------------------------------
                              " System Integration
" ------------------------------------------------------------------------------
" Regular copy and paste
" TODO(sandy): Maybe get rid of this and use the system clipboard?
vnoremap <C-c> "+yi
inoremap <C-v> <C-r><C-o>+

" Jump back to same line when reopening
augroup line_return
    au!
    au BufReadPost *
        \ if line("'\"") > 0 && line("'\"") <= line("$") |
        \ execute 'normal! g`"zvzz' |
        \ endif
augroup END

autocmd VimEnter * call after_object#enable('=', ':', '-', '#', ' ')

" Get rid of stupid system files from ctrl-p search
let g:ctrlp_custom_ignore = 'target/\|dist/\|\_site/'



" ------------------------------------------------------------------------------
                                " GUI and Colors
" ------------------------------------------------------------------------------
if has("gui_running")
    set guitablabel=%-0.12t%M
    set showtabline=2
    au BufAdd * :RainbowParentheses
else
    set t_Co=256
endif

set guioptions=ac
colo railscasts
set background=dark

if g:colors_name ==# "railscasts"
  hi WildMenu guifg=Black guibg=#777700 gui=bold cterm=bold
  hi Tabline guibg=#000000 guifg=#999999
  hi TablineFill guibg=#777700 guifg=#000000
  hi StatusLine guibg=#000000 guifg=#FFFFFF
endif

" Rainbow colored parentheses
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

" Right-margin length indicator
set cc=81



" ------------------------------------------------------------------------------
                                 " Autocommands
" ------------------------------------------------------------------------------
" Redraw when the window is resized
au VimResized * :wincmd =

" On save, remove all trailing spaces
au BufWritePre * :%s/\s\+$//e

" Re-source ~/.vimrc whenever it is saved
augroup automaticallySourceVimrc
  au!
  au bufwritepost .vimrc source ~/.vimrc
augroup END

" <CR> maps to :, but this is shitty for quickfix windows
augroup unmapCRInQuickfix
  au!
  autocmd BufReadPost quickfix nnoremap <buffer> <CR> <CR><C-W><C-J><C-W><C-Q>
  autocmd BufReadPost quickfix nnoremap <buffer> : :
  autocmd BufReadPost quickfix nnoremap <buffer> q <C-W><C-Q>
  autocmd CmdwinEnter * nnoremap <buffer> <CR> <CR><C-W><C-J><C-W><C-Q>
  autocmd CmdwinEnter * nnoremap <buffer> : :
  autocmd CmdwinEnter * nnoremap <buffer> q <C-W><C-Q>
augroup END


" ------------------------------------------------------------------------------
                              " Filetype Settings
" ------------------------------------------------------------------------------
au BufWritePre *.scala :SortScalaImports

" Set filetypes
au BufRead,BufNewFile *.db setfiletype db
au BufRead,BufNewFile *.md setfiletype ghmarkdown
au BufRead,BufNewFile *.markdown setfiletype ghmarkdown
au BufRead,BufNewFile *.htex setfiletype htex

syntax on
filetype on
filetype plugin indent on

" Markdown settings
let g:vim_markdown_initial_foldlevel=1
let g:vim_markdown_math=1
let g:vim_markdown_frontmatter=1

" Haskell settings
let g:haskell_indent_do = 3
let g:haskell_indent_if = 3
let g:haskell_indent_in = 1
let g:haskell_indent_let = 4
let g:haskell_indent_case = 2
let g:haskell_indent_where = 6
let g:haddock_browser="/usr/bin/sensible-browser"
au FileType haskell set cc=81

let g:gist_detect_filetype = 1



" ------------------------------------------------------------------------------
                                   " Settings
" ------------------------------------------------------------------------------
set autoindent
set backspace=indent,eol,start
set backupskip=/tmp/*,/private/tmp/*"
set dictionary=/usr/share/dict/words
set encoding=utf-8
set expandtab
set formatoptions=qrn1
set gdefault
set grepprg=ag
set hidden
set hlsearch
set ignorecase
set incsearch
set laststatus=2
set lazyredraw
set list listchars=tab:»·,trail:·
set matchtime=3
set modeline
set modelines=2
set mouse=a
set nofoldenable
set number
set relativenumber
set ruler
set scrolloff=8
set shiftround
set shiftwidth=4
set showcmd
set showmatch
set showmode
set smartcase
set softtabstop=4
set tabstop=4
set tags=./tags,tags,../tags
set timeoutlen=500
set title
set ttyfast
set visualbell
set wildignore+=*.aux,*.out,*.toc
set wildignore+=*.hi
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.luac
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.orig
set wildignore+=*.pyc
set wildignore+=*.spl
set wildignore+=*.sw?
set wildignore+=.hg,.git,.svn
set wildmenu
set wildmode=list:longest
set wrap


" ------------------------------------------------------------------------------
                        " Searching and File Management
" ------------------------------------------------------------------------------
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



" ------------------------------------------------------------------------------
                                     " Goyo
" ------------------------------------------------------------------------------
" Goyo is zen-mode writing. Most of my usual settings conflict with it pretty
" hard, so this fixes it
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



" ------------------------------------------------------------------------------
                            " Easy Buffer Switching
" ------------------------------------------------------------------------------
" Turn on buffer numbers in the tabline
let g:buftabline_numbers = 1

" Allow switching to buffer #<n> by typing <n>e
function! s:bufSwitch(count)
    if count ># 1
        return ":" . count . "b"
    endif
    return 'e'
endfunction
nnoremap <expr> e <SID>bufSwitch(v:count1)

" But we need to get rid of buffer #1 so it doesn't conflict with regular e
au VimEnter * if v:progname ==# "gvim" && expand('%') ==# "" |
                \ execute "normal! ihello\<ESC>:bw!\<CR>" |
                \ endif



" ------------------------------------------------------------------------------
                           " Source Local Definitions
" ------------------------------------------------------------------------------
" At work, I have company-specific bindings that would get me in trouble to post
" on github. So they live in this unmanaged file.
try
    source ~/.vimrc.local
catch
endtry

