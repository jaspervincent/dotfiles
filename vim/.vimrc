" General setting
" ----------------

set encoding=utf-8                " 默认字符编码
syntax on                         " 语法高亮
set ttimeout
set ttimeoutlen=10                " 设置键码超时时间为 10ms，改善响应速度
set clipboard=unnamedplus         " 方案1: 使用系统剪贴板
"set clipboard=unnamed             " 如果方案1不工作，尝试这个. 使用 * 寄存器（X11 主要选择）
set history=1000                  " 记录最近指定次数的操作，在退出vim后再进来仍可以 u 撤消之前的操作
set mouse=                        " nomarl模式中，可以用鼠标操作，如复制文本
set cursorline                    " 突出显示当前行

" 复制粘贴
"set paste                         " 复制保留格式，粘贴内容，不会有任何变形。 这样会一直处于 paste 模式
set pastetoggle=<F2>

" 搜索优化
set hlsearch                      " 高亮搜索，高亮显示所有的匹配短语
set incsearch                     " 查找短语时显示部分匹配
set hlsearch                      " 突出显示上次搜索的所有匹配项
set incsearch                     " 启用增量搜索(输入时获得反馈)
set ignorecase                    " 搜索时忽略大小写
set smartcase                     " 如果有大写字母则区分大小写
set wrapscan                      " 搜索到文件末尾时回到开头


" 缩进和编辑
set noautoindent
set nosmartindent
"set autoindent                   " 自动缩进
"set smartindent                  " 智能缩进
set ruler                         " 右下角显示光标状态行
set showcmd                       " 在屏幕最后一行显示命令
set nostartofline                 " 在行间行动时保持光标在同一列
set errorbells                    " 错误时响铃
set visualbell                    " 然后使用闪光灯代替蜂鸣声
set wildmenu                      " 执行命令时启用Tab键补全和建议
set wildmode=list:longest,full    " 如何完成匹配字符串的设置
set nomodeline                    " vim 读取模式行来执行当前文件的命令
set modelines=0                   " 检查文件顶部/底部的行数。0=关闭
set formatoptions-=t formatoptions-=o formatoptions+=crqjnl1
set confirm                       " 退出有更改的文件时要求确认
set joinspaces                    " 将多行合并为一行时，在标点符号后插入两个空格

" 全局默认缩进设置
set tabstop=4                     " 制表符显示为 4 个空格宽度; 简写：set ts=4
set shiftwidth=4                  " 自动缩进使用 4 个空格;  简写：set sw=4
set softtabstop=4
set expandtab                     " 用空格代替tab键。简写：set et

" 文件类型特定的缩进设置
augroup filetype_indent
  autocmd!
  " 2 空格缩进的文件类型
  autocmd FileType yaml,html,htmldjango,javascript,typescript,css,scss,json setlocal ts=2 sw=2 sts=2 expandtab
  " 4 空格缩进的文件类型  
  autocmd FileType python,java,c,cpp setlocal ts=4 sw=4 sts=4 expandtab
  " 保持制表符的文件类型
  autocmd FileType make setlocal ts=4 sw=4 sts=4 noexpandtab
  " setlocal 只对当前缓冲区生效，不影响其他文件
  " augroup 避免自动命令重复加载
augroup END

" 备份和撤销文件设置
"set undofile                     " 所有的操作会记录在unofile中，方便使用 u 和 ctrl + r 。会在当前目录创建一个 .同名文件.un~ 文件
"set undodir=~/.vim/undodir       " 存储undofile的目录，不会污染项目目录下文件。提前创建好目录
" set backupdir=~/.vim/backupdir
" set directory=~/.vim/swapdir
" 创建目录（需要手动执行或在vimrc外创建）
" mkdir -p ~/.vim/undodir ~/.vim/backupdir ~/.vim/swapdir


" 状态行                                                                                                                                                              
" 默认情况下，laststatus=1（只在有多个窗口时显示状态行）或 laststatus=2（总是显示状态行）
set laststatus=2
set statusline=%f                           " 文件名
set statusline+=\ %h%m%r%w                  " 标志
set statusline+=\ [%{&ff}/%Y]               " 文件格式/类型
set statusline+=\ [%{strlen(&fenc)?&fenc:&enc}] " 编码
set statusline+=\ [L=%l/%L,C=%v]            " 行/列信息
set statusline+=\ [%p%%]                    " 百分比
" 状态行各组件说明
"set statusline=%f%h%m%r%w[%{&ff}/%Y][L=%l/%L,C=%v][%p%%]
" %F      当前文件的完整路径
" %h      帮助文件标志 ([help])
" %m      修改标志 ([+])
" %r      只读标志 ([RO])
" %w      预览窗口标志 ([Preview])
" [%{&ff}] 文件格式 ([unix/dos/mac])
" %y      文件类型 (VIM)
" [%04l]  当前行号（4位数字，前导零）
" [%L]    文件总行数
" [%04v]  当前列号（4位数字，前导零）
" [%p%%]  文件位置百分比

"下面两行是超过80个字符底色变红
" 方法1：匹配第81个字符开始为红色
highlight OverLength ctermbg=red ctermfg=white guibg=#592929 
match OverLength /\%81v.\+/
" 方法2：直接设置颜色列
" set colorcolumn=81
" highlight ColorColumn ctermbg=red guibg=#592929


" Leader 键                         
" ----------------

let mapleader=","
" 使用kj快速进入Normal模式
inoremap <silent> kj <Esc>
inoremap <C-[> <Esc>


" 插件
" ----------------

"call plug#begin('~/.vim/plugged')

" 安装插件只需要把github地址放到这里重启后执行 :PlugInstall 就好了
"Plug 'Yggdroot/indentLine'

"call plug#end()

" 文件类型检测（放在最后）
filetype plugin indent on
