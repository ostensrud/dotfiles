"Enable syntax highlight
:syntax enable

"Highlight search term
:set hlsearch

"Incremental search
:set incsearch

"Use two spaces instead of tab
:set sw=2
:set smarttab
:set tabstop=2 shiftwidth=2 expandtab

"Format the statusline
:set statusline=%F%m%r%h%w\ [FORMAT=%{&ff}]\ [TYPE=%Y]\ [POS=%04l,%04v][%p%%]\ [LEN=%L]
:set laststatus=2

"Indicate column 80
:set colorcolumn=80
highlight ColorColumn ctermbg=7

set backspace=2 " make backspace work like most other apps

"Perldoc
:set keywordprg=perldoc\ -f

map <F3> :set hlsearch!<CR>
map <F6> :set list!<CR>

"set background=light
set background=dark
colorscheme solarized
