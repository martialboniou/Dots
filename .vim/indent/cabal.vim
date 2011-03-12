if exists("b:did_indent")
    finish
endif
let b:did_indent = 1

fun! s:cur_line_no()
    return line('.')
endf
fun! s:prev_line_no()
    return s:cur_line_no() - 1
endf
fun! s:cur_line()
    return getline(s:cur_line_no())
endf
fun! s:prev_line()
    return getline(s:prev_line_no())
endf
fun! s:is_lef(l)
    return (a:l =~ '^\v\c\s*(library|executable|flag|source-repository)>')
endf

fun! s:is_freeform()
    " freeform fields tend to be longer, so after a new line we assume it's
    " continuing (until a ':' is typed, at least)
    let pos = s:prev_line_no()
    let ind = indent(pos)
    while indent(pos) >= ind && pos >= 0
        let pos -= 1
    endw
    return (getline(pos) =~
                \ '\v<(copyright|author|stability|synopsis|description)\:')
endf

fun! CabalIndent()
    let cur = s:cur_line()
    let prev = s:prev_line()
    let prev_no = s:prev_line_no()

    if prev_no < 1 " hack :(
        return 0
    elseif prev =~ '^\s*--' " previous line is comment
        return indent(prev)
    elseif prev =~ ':$' || s:is_lef(prev) || prev =~ '^\v\s*(if|else)>'
        " previous line is a field key, a flag/library/... block, or an if
        return indent(prev_no) + &sw
    elseif cur =~ '\<else\>' ||
                \ (cur =~ '^\v\s*(\w|-)+:' && prev !~ '^\v\s*(\w|-)+:'
                \ && indent(cur) <= indent(prev))
        " current line is an else or a field key, after an indented thing
        " (i.e. we're re-indenting after typing ':' or 'else')
        return max([0, indent(prev_no) - &sw])
    elseif prev =~ ':.*,$'
        " previous line is 'foo: bar,' and we need to line up with 'bar'
        return match(prev, ':\s*\zs.')
    elseif s:is_lef(cur)
        " current line starts a new block
        return 0
    elseif prev =~ ',$' || s:is_freeform()
        " current line is continuation of previous
        return indent(prev_no)
    else
        " current line starts a new property (presumably)
        return indent(search(':', 'bn'))
    endif
endf

set autoindent nocindent nocopyindent
set indentexpr=CabalIndent()
set indentkeys=!^F,o,O,e,<:>,=flag,=executable,=library,=source-repository
set comments=:--
