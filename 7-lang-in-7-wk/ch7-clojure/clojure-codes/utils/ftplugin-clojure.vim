" usage:
"     append the content of this file to `~/.vim/ftdetect/clojure.vim`
"     use <F7> to run the script, `-main` in the current file will be called
"
" dependency:
"     vim-foreplay 	https://github.com/tpope/vim-foreplay
"

" <F7> always stands for running the program

if exists("g:loaded_foreplay")
	nmap <F7> :w<CR>cpR:Eval (-main)<CR>
else
	nmap <F7> :w<CR>:! lein-ns-run %<CR>
endif
