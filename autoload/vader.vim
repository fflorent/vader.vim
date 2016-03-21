" Copyright (c) 2015 Junegunn Choi
"
" MIT License
"
" Permission is hereby granted, free of charge, to any person obtaining
" a copy of this software and associated documentation files (the
" "Software"), to deal in the Software without restriction, including
" without limitation the rights to use, copy, modify, merge, publish,
" distribute, sublicense, and/or sell copies of the Software, and to
" permit persons to whom the Software is furnished to do so, subject to
" the following conditions:
"
" The above copyright notice and this permission notice shall be
" included in all copies or substantial portions of the Software.
"
" THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
" EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
" MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
" NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
" LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
" OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
" WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

if exists("g:loaded_vader")
  finish
endif
let g:loaded_vader = 1
let s:register = {}
let s:indent = 2

function! vader#run(bang, ...) range
  let s:error_line = 0

  if a:lastline - a:firstline > 0
    if a:0 > 1
      echoerr "You can't apply range on multiple files"
      return
    endif
    let [line1, line2] = [a:firstline, a:lastline]
  else
    let [line1, line2] = [1, 0]
  endif

  if a:0 == 0
    let patterns = [expand('%')]
  else
    let patterns = a:000
  endif

  call vader#assert#reset()
  call s:prepare()
  try
    let context = {
          \ 'all_cases': [],
          \ 'qfl': [],
          \ 'st': reltime(),
          \ 'success': 0,
          \ 'pending': 0,
          \ 'total': 0,
          \ 'bang': a:bang
          \ }

    for gl in patterns
      for fn in split(glob(gl), "\n")
        if fnamemodify(fn, ':e') == 'vader'
          let afn = fnamemodify(fn, ':p')
          let cases = vader#parser#parse(afn, line1, line2)
          call add(context.all_cases, [afn, cases])
          let context.total += len(cases)
        endif
      endfor
    endfor
    if empty(context.all_cases) | return | endif

    call vader#window#open()
    call vader#window#append(
    \ printf("Starting Vader: %d suite(s), %d case(s)", len(context.all_cases), context.total), 0)

    " Looping over all_cases asynchronously
    function! context.run_each_case()
      try
        let pair = remove(self.all_cases, 0)
        let [fn, case] = pair
        call s:run(fn, case, self)
      catch
        call s:on_exception(v:exception, self.bang)
      endtry
    endfunction

    function! context.on_case_success(res)
      try
        let [cs, cp, ct, lqfl] = a:res
        let self.success += cs
        let self.pending += cp
        call extend(self.qfl, lqfl)
        call vader#window#append(
              \ printf('Success/Total: %s/%s%s',
              \     cs, ct, cp > 0 ? (' ('.cp.' pending)') : ''),
              \ 1)

        if len(self.all_cases) ==# 0
          call s:on_run_finished(self)
        else
          call self.run_each_case()
        endif

      catch
        call s:on_exception(v:exception, self.bang)
      endtry
    endfunction

    call context.run_each_case()

  catch
    call s:on_exception(v:exception, a:bang)
  endtry
endfunction

function s:on_run_finished(run_context)
  try
    let stats = vader#assert#stat()
    call vader#window#append(printf('Success/Total: %s/%s (%sassertions: %d/%d)',
          \ a:run_context.success, a:run_context.total, (a:run_context.pending > 0 ? a:run_context.pending . ' pending, ' : ''),
          \ stats[0], stats[1]), 0)
    call vader#window#append('Elapsed time: '.
          \ substitute(reltimestr(reltime(a:run_context.st)), '^\s*', '', '') .' sec.', 0)
    call vader#window#cleanup()

    let g:vader_report = join(getline(1, '$'), "\n")
    let g:vader_errors = a:run_context.qfl
    call setqflist(a:run_context.qfl)

    if a:run_context.bang
      redir => ver
      silent version
      redir END

      call s:print_stderr(ver . "\n\n" . g:vader_report)
      if a:run_context.success + a:run_context.pending == a:run_context.total
        qall!
      else
        cq
      endif
    elseif !empty(a:run_context.qfl)
      call vader#window#copen()
    endif
  catch
    call s:on_exception(v:exception, a:run_context.bang)
  finally
    call s:cleanup()
  endtry
endfunction

function s:on_exception(exception, bang)
  if a:bang
    call s:print_stderr(a:exception)
    cq
  else
    echoerr a:exception
  endif
  call s:cleanup()
endfunction

function! s:print_stderr(output)
  let tmp = tempname()
  call writefile(split(a:output, '\n'), tmp)
  execute 'silent !cat '.tmp.' 1>&2'
  call delete(tmp)
endfunction

function! s:split_args(arg)
  let varnames = split(a:arg, ',')
  let names = []
  for varname in varnames
    let name = substitute(varname, '^\s*\(.*\)\s*$', '\1', '')
    let name = substitute(name, '^''\(.*\)''$', '\1', '')
    let name = substitute(name, '^"\(.*\)"$',  '\1', '')
    call add(names, name)
  endfor
  return names
endfunction

function! vader#log(msg)
  let msg = type(a:msg) == 1 ? a:msg : string(a:msg)
  call vader#window#append('> ' . msg, s:indent)
endfunction

function! vader#save(args)
  for varname in s:split_args(a:args)
    if exists(varname)
      let s:register[varname] = eval(varname)
    endif
  endfor
endfunction

function! vader#restore(args)
  let varnames = s:split_args(a:args)
  for varname in empty(varnames) ? keys(s:register) : varnames
    if has_key(s:register, varname)
      execute printf("let %s = s:register['%s']", varname, varname)
    endif
  endfor
endfunction

function! s:prepare()
  command! -nargs=+ Log            :call vader#log(<args>)
  command! -nargs=+ Save           :call vader#save(<q-args>)
  command! -nargs=* Restore        :call vader#restore(<q-args>)
  command! -nargs=+ Assert         :call vader#assert#true(<args>)
  command! -nargs=+ AssertEqual    :call vader#assert#equal(<args>)
  command! -nargs=+ AssertNotEqual :call vader#assert#not_equal(<args>)
  command! -nargs=+ AssertThrows   :call vader#assert#throws(<q-args>)
  let g:SyntaxAt = function('vader#helper#syntax_at')
  let g:SyntaxOf = function('vader#helper#syntax_of')
endfunction

function! s:cleanup()
  let s:register = {}
  delcommand Log
  delcommand Save
  delcommand Restore
  delcommand Assert
  delcommand AssertEqual
  delcommand AssertNotEqual
  delcommand AssertThrows
  unlet g:SyntaxAt
  unlet g:SyntaxOf
endfunction

function! s:comment(case, label)
  return get(a:case.comment, a:label, '')
endfunction

function! s:execute(prefix, type, block, lang_if)
  try
    call vader#window#execute(a:block, a:lang_if)
    return 1
  catch
    call s:append(a:prefix, a:type, v:exception, 1)
    call s:print_throwpoint()
    return 0
  endtry
endfunction

function! s:print_throwpoint()
  if v:throwpoint !~ 'vader#assert'
    Log v:throwpoint
  endif
endfunction

function! s:run(filename, cases, run_context)
  let total = len(a:cases)
  let context = {
        \ 'given': [],
        \ 'before': [],
        \ 'after': [],
        \ 'then': [],
        \ 'comment': { 'given': '', 'before': '', 'after': '' },
        \ 'cases': copy(a:cases),
        \ 'total': total,
        \ 'just' : len(string(total)),
        \ 'cnt': 0,
        \ 'pending': 0,
        \ 'success': 0,
        \ 'qfl': [],
        \ 'run_context': a:run_context,
        \ 'filename': a:filename,
        \ }
  let g:vader_file = a:filename

  call vader#window#append("Starting Vader: ". a:filename, 1)

  function context.run_cases()
    let l:cnt = 0
    for case in self.cases
      let self.cnt += 1
      let l:cnt += 1
      let ok = 1
      let prefix = printf('(%'.self.just.'d/%'.self.just.'d)', self.cnt, self.total)

      for label in ['given', 'before', 'after', 'then']
        if has_key(case, label)
          let self[label] = case[label]
          let self.comment[label] = get(case.comment, label, '')
        endif
      endfor

      if !empty(self.given)
        call s:append(prefix, 'given', self.comment.given)
      endif
      call vader#window#prepare(self.given, get(case, 'type', ''))

      if !empty(self.before)
        let s:indent = 2
        let ok = ok && s:execute(prefix, 'before', self.before, '')
      endif

      let s:indent = 3
      if has_key(case, 'execute')
        call s:append(prefix, 'execute', s:comment(case, 'execute'))
        let ok = ok && s:execute(prefix, 'execute', case.execute, get(case, 'lang_if', ''))
      elseif has_key(case, 'do')
        call s:append(prefix, 'do', s:comment(case, 'do'))
        try
          call vader#window#replay(case.do)
        catch
          call s:append(prefix, 'do', v:exception, 1)
          call s:print_throwpoint()
          let ok = 0
        endtry
      endif

      if has_key(case, 'waituntil')
        call remove(self.cases, 0, l:cnt - 1)
        call vader#window#waitUntil(case.events, case.waituntil, self)
        return
      endif

      if !empty(self.after)
        let s:indent = 2
        let ok = ok && s:execute(prefix, 'after', self.after, '')
      endif

      if has_key(case, 'then')
        call s:append(prefix, 'then', s:comment(case, 'then'))
        let ok = ok && s:execute(prefix, 'then', self.then, '')
      endif

      if has_key(case, 'expect')
        let result = vader#window#result()
        let match = case.expect ==# result
        if match
          call s:append(prefix, 'expect', s:comment(case, 'expect'))
        else
          let begin = s:append(prefix, 'expect', s:comment(case, 'expect'), 1)
          let ok = 0
          let data = { 'type': get(case, 'type', ''), 'got': result, 'expect': case.expect }
          call vader#window#append('- Expected:', 3)
          for line in case.expect
            call vader#window#append(line, 5, 0)
          endfor
          let end = vader#window#append('- Got:', 3)
          for line in result
            let end = vader#window#append(line, 5, 0)
          endfor
          call vader#window#set_data(begin, end, data)
        endif
      endif

      if ok
        let self.success += 1
      else
        let self.pending += case.pending
        let description = join(filter([
              \ self.comment.given,
              \ get(case.comment, 'do', get(case.comment, 'execute', '')),
              \ get(case.comment, 'then', ''),
              \ get(case.comment, 'expect', '')], '!empty(v:val)'), ' / ') .
              \ ' (#'.s:error_line.')'
        call add(self.qfl, { 'type': 'E', 'filename': self.filename, 'lnum': case.lnum, 'text': description })
      endif
    endfor
    unlet g:vader_file
    call self.run_context.on_case_success([self.success, self.pending, self.total, self.qfl])
  endfunction
  call context.run_cases()
endfunction

function! s:append(prefix, type, message, ...)
  let error = get(a:, 1, 0)
  let message = (error ? '(X) ' : '') . a:message
  let line = vader#window#append(printf("%s [%7s] %s", a:prefix, toupper(a:type), message), 2)
  if error
    let s:error_line = line
  endif
  return line
endfunction

