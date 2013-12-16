" read

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
                        let cmd = '0read !' . s:psys_path . 'pread '
                        let cmd = cmd . strpart(a:pfilename, 0, pos) . ' '
                        let cmd = cmd . strpart(a:pfilename, pos + 1)
                        execute cmd
                        if ! v:shell_error
                                execute 'file ' . a:pfilename
                                set modified!
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
                let cmd = 'w ! ' . s:psys_path . 'pwrite '
                let cmd = cmd . strpart(pfilename, 0, pos) . ' '
                let cmd = cmd . strpart(pfilename, pos + 1)
                execute cmd
                if ! v:shell_error
                        set modified!
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


command -nargs=1 -bang Pread call PReadFile('<args>', '<bang>')
command -nargs=? -bang Pwrite call PWriteFile('<args>', '<bang>')
