" Psysvol vim utilities

" Note to users of this script:
" By default, &directory has current directory in the list of locations to
" place swap files. This should be removed if using this utility to edit
" p-System files. A suitable entry in your vimrc will take care of this, e.g.:
" set directory=/tmp


let s:psys_separator = '@'

if exists('b:psys_path')
        let s:psys_path = b:psys_path
else
        let s:psys_path = ''
endif


function! SetPsysPath (psys_path)
        let s:psys_path = a:psys_path
endfunction


" the windows version of vim transforms forward slashes in the p-system
" part of the filename into backslashes
function! BSlashToFSlash (str)
	
	return substitute(a:str, '\\', '/', 'g')

endfunction


function! PReadFile (pfilename, override)

        if a:override == '!' || ! &modified
                let pos = match(a:pfilename, s:psys_separator)
                if pos >= 0
                        " we're opening from a psys vol
                        execute 'enew!'
                        let cmd = 'read !' . s:psys_path . 'pread '
                        let cmd = cmd . strpart(a:pfilename, 0, pos) . ' '
                        let cmd = cmd . strpart(a:pfilename, pos + 1)
                        execute cmd
                        if ! v:shell_error
                                " enew gives us a new buffer with a single
                                " line in it, remove it here
                                execute '1d'
                                " set the file name
                                execute 'file ' . a:pfilename
                                echo 'Successful read from p-System volume'
                        else
                                " It went wrong, whatever is in the buffer is
                                " an error message - copy it to message area
                                while line('$') > 1 || getline(1) > ''
                                        echo getline(1)
                                        execute '1d'
                                endwhile
                                
                        endif
			set nomodified
                else
			" normal vim edit
			try
                        	execute 'edit! ' . a:pfilename
			catch /^Vim(edit):/
				echohl ErrorMsg
				echo substitute(v:exception, "^Vim(edit):", "", "")
				echohl None
			endtry
                endif
        else
                echohl ErrorMsg
                echo "No write since last change (add ! to override)"
                echohl None
        endif

endfunction


function! PWriteFile (pfilename, override)

        if a:pfilename == ''
                let pfilename = @%
        else
                let pfilename = a:pfilename
        endif

        let pos = match(pfilename, s:psys_separator)

        if pos >= 0 
		" we're writing to a p-system vol
		" fixme: should we create the '~' style backup file?
		" it's somewaht moot at the moment as pwrite will take a
		" backup of the entire volume
		" fixme: how to stop dos window appearing in windows when
		" using 'write !' from gvim?
                let cmd = 'write ! ' . s:psys_path . 'pwrite '
                let cmd = cmd . strpart(pfilename, 0, pos) . ' '
                let cmd = cmd . BSlashToFSlash(strpart(pfilename, pos + 1))
                execute cmd
                if ! v:shell_error
                        set nomodified
                        echo 'Successful write to p-System volume'
                endif
        else
		" normal vim write
		try
			execute 'write' . a:override . ' ' . a:pfilename
		catch /^Vim(write):/
                        echohl ErrorMsg
                        echo substitute(v:exception, "^Vim(write):", "", "")
                        echohl None
		endtry
        endif

endfunction


function! PWriteQuit (pfilename, override)

        call PWriteFile(a:pfilename, a:override)
        " only quit if we managed to write ok
        if ! &modified
                execute 'quit'
        endif
        
endfunction


command -nargs=1 -bang E call PReadFile('<args>', '<bang>')
command -nargs=? -bang W call PWriteFile('<args>', '<bang>')
command -nargs=? -bang WQ call PWriteQuit('<args>', '<bang>')

:cnoreabbrev e <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'E' : 'e')<CR>
:cnoreabbrev w <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'W' : 'w')<CR>
:cnoreabbrev wq <c-r>=(getcmdtype()==':' && getcmdpos()==1 ? 'WQ' : 'wq')<CR>

