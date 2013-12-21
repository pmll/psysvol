" Psysvol vim utilities

let s:psys_separator = '@'

if exists('b:psys_path')
        let s:psys_path = b:psys_path
else
        let s:psys_path = ''
endif


function! SetPsysPath (psys_path)
        let s:psys_path = a:psys_path
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
                                set nomodified
                                echo 'Successful read from p-System volume'
                        else
                                " It went wrong, whatever is in the buffer is
                                " an error message - copy it to message area
                                while line('$') > 1 || getline(1) > ''
                                        echo getline(1)
                                        execute '1d'
                                endwhile
                                
                        endif
                else
                        execute 'edit! ' . a:pfilename
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
                let cmd = 'write ! ' . s:psys_path . 'pwrite '
                let cmd = cmd . strpart(pfilename, 0, pos) . ' '
                let cmd = cmd . strpart(pfilename, pos + 1)
                execute cmd
                if ! v:shell_error
                        set nomodified
                        echo 'Successful write to p-System volume'
                endif
        else
                if a:override == '!' || ! &readonly 
                        execute 'write' . a:override . ' ' . a:pfilename
                else
                        " there are more ways a write can fail than this...
                        echohl ErrorMsg
                        echo "'readonly' option is set (add ! to override)"
                        echohl None
                endif
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

