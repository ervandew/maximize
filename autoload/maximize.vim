" Author:  Eric Van Dewoestine
"
" License: {{{
"   Copyright (c) 2005 - 2024, Eric Van Dewoestine
"   All rights reserved.
"
"   Redistribution and use of this software in source and binary forms, with
"   or without modification, are permitted provided that the following
"   conditions are met:
"
"   * Redistributions of source code must retain the above
"     copyright notice, this list of conditions and the
"     following disclaimer.
"
"   * Redistributions in binary form must reproduce the above
"     copyright notice, this list of conditions and the
"     following disclaimer in the documentation and/or other
"     materials provided with the distribution.
"
"   * Neither the name of Eric Van Dewoestine nor the names of its
"     contributors may be used to endorse or promote products derived from
"     this software without specific prior written permission of
"     Eric Van Dewoestine.
"
"   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
"   IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
"   THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
"   PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR
"   CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
"   EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
"   PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
"   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
"   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
"   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
"   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
" }}}

" Global Variables {{{
  if !exists('g:MaximizeMinWinHeight')
    let g:MaximizeMinWinHeight = 0
  endif
  if !exists('g:MaximizeMinWinWidth')
    let g:MaximizeMinWinWidth = 0
  endif
" }}}

function! maximize#MaximizeWindow(full) " {{{
  if s:IsFloating()
    return
  endif

  " disable any minimize settings
  call maximize#ResetMinimized()

  " get the window that is maximized
  if s:IsTabMaximized()
    " only disable if there are no longer any maximized windows on all tabs.
    if !s:IsAnotherTabMaximized()
      call s:DisableMaximizeAutoCommands()
    else
    endif
    call maximize#RestoreWindows(s:GetMaximizedWindow())
  endif

  let maximized_mode = a:full ? 'full' : 'fixed'
  if !exists('t:maximized_mode') || t:maximized_mode != maximized_mode
    let t:maximized_mode = maximized_mode
    let last = winnr('$')
    let index = 1
    while index <= last
      call s:InitWindowDimensions(index)
      let index += 1
    endwhile
    exec 'set winminwidth=' . g:MaximizeMinWinWidth
    exec 'set winminheight=' . g:MaximizeMinWinHeight
    call maximize#MaximizeUpdate(a:full, 1)
  else
    let t:maximized_mode = ''
  endif
endfunction " }}}

function! maximize#MinimizeWindow(...) " {{{
  if s:IsFloating()
    return
  endif

  let curwinnum = winnr()

  exec 'set winminheight=' . g:MaximizeMinWinHeight
  exec 'set winminwidth=' . g:MaximizeMinWinWidth

  call s:DisableMaximizeAutoCommands()
  call s:DisableMinimizeAutoCommands()

  " first turn off maximized if enabled
  if s:IsTabMaximized()
    call maximize#RestoreWindows(s:GetMaximizedWindow())
  endif

  let args = []
  if len(a:000) == 0
    let args = [winnr()]
  else
    let args = a:000[:]
  endif

  " initialize window dimensions
  let last = winnr('$')
  let index = 1
  while index <= last
    call s:InitWindowDimensions(index)
    let index += 1
  endwhile

  " loop through and mark the buffers
  let num_minimized = 0
  for winnum in args
    if winnum < 0 || winnum > last
      continue
    endif

    let val = getwinvar(winnum, 'minimized')
    let minimized = type(val) == 0 ? !val : 1
    if minimized
      let num_minimized += 1
    else
      call s:RestoreWinVar(winnum, '&winfixheight')
      call s:RestoreWinVar(winnum, '&winfixwidth')
    endif
    call setwinvar(winnum, 'minimized', minimized)
  endfor

  " check for existing minimized windows
  if num_minimized == 0
    let index = 1
    while index <= last
      if getwinvar(index, 'minimized') == 1
        let num_minimized += 1
      endif
      let index = index + 1
    endwhile
  endif

  noautocmd call s:Reminimize(1)
  let t:minimized = num_minimized > 0 ? 1 : ''
  if num_minimized > 0 || s:IsAnotherTabMinimized()
    call s:EnableMinimizeAutoCommands()
  endif
endfunction " }}}

function! maximize#MaximizeUpdate(full, force) " {{{
  if !a:force && !s:IsTabMaximized()
    return
  endif

  call s:InitWindowDimensions(winnr())
  call s:DisableMaximizeAutoCommands()

  if !s:IsFloating()
    let w:maximized = 1
    winc |
    winc _
  endif

  if !a:full
    call s:RestoreFixedWindows()
  else
    let winnr = winnr()
    if getwinvar(winnr, '&winfixheight')
      exec winnr . 'resize ' . getwinvar(winnr, 'winheight')
    endif
    if getwinvar(winnr, '&winfixwidth')
      exec 'vertical ' . winnr . 'resize ' . getwinvar(winnr, 'winwidth')
    endif
  endif
  call s:EnableMaximizeAutoCommands(a:full)
endfunction " }}}

function! maximize#ResetMinimized() " {{{
  if !s:IsTabMinimized()
    return
  endif
  call s:DisableMinimizeAutoCommands()
  let t:minimized = ''
  let winend = winnr('$')
  let winnum = 1
  let num_minimized = 0
  while winnum <= winend
    if getwinvar(winnum, 'minimized') == 1
      let num_minimized += 1
      call setwinvar(winnum, 'minimized', 0)
      call s:RestoreWinVar(winnum, '&winfixheight')
      call s:RestoreWinVar(winnum, '&winfixwidth')
    endif
    let winnum = winnum + 1
  endwhile

  call s:RestoreFixedWindows()
  winc =

  if s:IsAnotherTabMinimized()
    call s:EnableMinimizeAutoCommands()
  endif
endfunction " }}}

function! maximize#RestoreWindows(maximized) " {{{
  " reset the maximized var.
  if a:maximized
    call setwinvar(a:maximized, 'maximized', 0)
  endif

  winc _
  winc =

  call s:RestoreFixedWindows()
endfunction " }}}

function! maximize#NavigateWindows(wincmd) " {{{
  " Used navigate windows by skipping minimized windows.

  " edge case for the command line window
  if &ft == 'vim' && bufname('%') == '[Command Line]'
    quit
    return
  endif

  let start = winnr()
  let lastwindow = start

  exec a:wincmd
  while exists('w:minimized') && w:minimized && winnr() != lastwindow
    let lastwindow = winnr()
    let lastfile = expand('%')
    exec a:wincmd
  endwhile

  if exists('w:minimized') && w:minimized && winnr() != start
    exec start . 'wincmd w'
  endif
endfunction " }}}

function! s:InitWindowDimensions(winnr) " {{{
  if getwinvar(a:winnr, 'winheight') == ''
    "echom 'win: ' . a:winnr . ' height: ' . winheight(a:winnr)
    call setwinvar(a:winnr, 'winheight', winheight(a:winnr))
  endif
  if getwinvar(a:winnr, 'winwidth') == ''
    "echom 'win: ' . a:winnr . ' width:  ' . winwidth(a:winnr)
    call setwinvar(a:winnr, 'winwidth', winwidth(a:winnr))
  endif
endfunction " }}}

function! s:DisableMaximizeAutoCommands() " {{{
  augroup maximize
    autocmd!
  augroup END
endfunction " }}}

function! s:EnableMaximizeAutoCommands(full) " {{{
  call s:DisableMaximizeAutoCommands()
  call s:DisableMinimizeAutoCommands()
  augroup maximize
    autocmd!
    exec 'autocmd BufWinEnter,WinEnter * nested ' .
      \ 'call maximize#MaximizeUpdate(' . a:full . ', 0)'
    exec 'autocmd VimResized,BufDelete * nested ' .
      \ 'call s:MaximizeRefresh(' . a:full . ')'
    exec 'autocmd BufReadPost quickfix ' .
      \ 'call s:MaximizeRefresh(' . a:full . ')'
    exec 'autocmd BufUnload * call s:CloseFixedWindow(' . a:full . ')'
    " for :pclose
    exec 'autocmd BufWinLeave * nested call s:DelayedCommand(' .
        \ '"call maximize#MaximizeUpdate(' . a:full . ', 0)")'
  augroup END

  augroup supertab_preview_closed
    exec 'autocmd! User <supertab> ' .
      \ 'call maximize#MaximizeUpdate(' . a:full . ', 0)'
  augroup END
endfunction " }}}

function! s:DisableMinimizeAutoCommands() " {{{
  augroup minimize
    autocmd!
  augroup END
endfunction " }}}

function! s:EnableMinimizeAutoCommands() " {{{
  call s:DisableMaximizeAutoCommands()
  augroup minimize
    autocmd!
    autocmd BufWinEnter,WinEnter * nested noautocmd call s:Reminimize(0)
  augroup END
endfunction " }}}

function! s:GetMaximizedWindow(...) " {{{
  if a:0
    let winend = tabpagewinnr(a:1, '$')
  else
    let winend = winnr('$')
  endif
  let winnum = 1
  while winnum <= winend
    if a:0
      let max = gettabwinvar(a:1, winnum, 'maximized')
    else
      let max = getwinvar(winnum, 'maximized')
    endif
    if max
      return winnum
    endif
    let winnum = winnum + 1
  endwhile

  return 0
endfunction " }}}

function! s:IsFloating() " {{{
  let floating = v:false
  if has('nvim')
    let floating = luaeval('vim.api.nvim_win_get_config(0).zindex ~= nil')
  endif
  return floating
endfunction " }}}

function! s:IsTabMaximized(...) " {{{
  if a:0
    return gettabvar(a:1, 'maximized_mode') != ''
  endif
  return exists('t:maximized_mode') && t:maximized_mode != ''
endfunction " }}}

function! s:IsAnotherTabMaximized() " {{{
  let tabend = tabpagenr('$')
  let index = 0
  while index < tabend
    let index += 1
    if index == tabpagenr()
      continue
    endif
    if s:IsTabMaximized(index)
      return 1
    endif
  endwhile
  return 0
endfunction " }}}

function! s:IsTabMinimized(...) " {{{
  if a:0
    return gettabvar(a:1, 'minimized') != ''
  endif
  return exists('t:minimized') && t:minimized != ''
endfunction " }}}

function! s:IsAnotherTabMinimized() " {{{
  let tabend = tabpagenr('$')
  let index = 0
  while index < tabend
    let index += 1
    if index == tabpagenr()
      continue
    endif
    if s:IsTabMinimized(index)
      return 1
    endif
  endwhile
  return 0
endfunction " }}}

function! s:MaximizeRefresh(full) " {{{
  if !s:IsTabMaximized()
    return
  endif

  call s:InitWindowDimensions(winnr())
  let maximized = s:GetMaximizedWindow()
  if maximized
    let curwin = winnr()
    try
      noautocmd exec maximized . 'winc w'
      call maximize#MaximizeUpdate(a:full, 0)
    catch /E788/
      " ignore, happens when opening a quickfix window
    finally
      if winnr() != curwin
        exec curwin . 'winc w'
      endif
    endtry
  endif
endfunction " }}}

function! s:CloseFixedWindow(full) " {{{
  if expand('<afile>') == '' || &buftype != ''
    let maximized = s:GetMaximizedWindow()
    if maximized
      call s:DelayedCommand(
        \ 'call maximize#MaximizeUpdate(' . a:full . ', 0)')
    endif
  endif
endfunction " }}}

function! s:RestoreFixedWindows() " {{{
  let last = winnr('$')
  let index = last
  while index >= 1
    let minimized = getwinvar(index, 'minimized')
    if getwinvar(index, '&winfixheight') && minimized != 1
      "echom index . 'resize ' . getwinvar(index, 'winheight')
      exec index . 'resize ' . getwinvar(index, 'winheight')
    endif
    if getwinvar(index, '&winfixwidth') && minimized != 1
      "echom 'vertical ' . index . 'resize ' . getwinvar(index, 'winwidth')
      exec 'vertical ' . index . 'resize ' . getwinvar(index, 'winwidth')
    endif
    let index -= 1
  endwhile
endfunction " }}}

function! s:Reminimize(force) " {{{
  " Invoked when changing windows to ensure that any minimized windows are
  " returned to their minimized state.

  if !a:force && !s:IsTabMinimized()
    return
  endif

  call s:InitWindowDimensions(winnr())
  let curwinnum = winnr()
  let winend = winnr('$')
  let winnum = 1
  let commands = []
  while winnum <= winend
    let minimized = getwinvar(winnum, 'minimized')
    if minimized
      let row_minimized = s:RowMinimized(winnum)
      let column_minimized = s:ColumnMinimized(winnum)

      "echom 'winnr = ' . winnum
      "echom '  row minimized    = ' . row_minimized
      "echom '  column minimized = ' . column_minimized
      "echom '  in row           = ' . s:IsInRow(winnum)
      "echom '  in column        = ' . s:IsInColumn(winnum)

      if row_minimized
        call add(commands, winnum . "resize 0")
        call s:SaveWinVar(winnum, '&winfixheight')
        call setwinvar(winnum, "&winfixheight", 1)

      elseif column_minimized
        call add(commands, "vertical " . winnum . "resize 0")
        call s:SaveWinVar(winnum, '&winfixwidth')
        call setwinvar(winnum, "&winfixwidth", 1)

      elseif s:IsInRow(winnum)
        call add(commands, "vertical " . winnum . "resize 0")
        call s:SaveWinVar(winnum, '&winfixwidth')
        call setwinvar(winnum, "&winfixwidth", 1)

      elseif s:IsInColumn(winnum)
        call add(commands, winnum . "resize 0")
        call s:SaveWinVar(winnum, '&winfixheight')
        call setwinvar(winnum, "&winfixheight", 1)

      else
        call add(commands, winnum . "resize 0")
        call add(commands, "vertical " . winnum . "resize 0")
        call s:SaveWinVar(winnum, '&winfixheight')
        call s:SaveWinVar(winnum, '&winfixwidth')
        call setwinvar(winnum, "&winfixheight", 1)
        call setwinvar(winnum, "&winfixwidth", 1)
      endif
    endif
    let winnum = winnum + 1
  endwhile

  " ensure we end up in the window we started in
  exec curwinnum . 'winc w'

  " run all the resizing commands
  for cmd in commands
    exec cmd
  endfor

  winc =
  call s:RestoreFixedWindows()
endfunction " }}}

function! s:IsInRow(window) " {{{
  " Determines if the supplied window is in a row of equally sized windows.

  let origwinnr = winnr()
  exec a:window . 'winc w'

  " check windows to the right
  let curwinnr = winnr()
  winc l
  while winnr() != curwinnr
    let curwinnr = winnr()
    if winheight(curwinnr) == winheight(a:window)
      exec origwinnr . 'winc w'
      return 1
    endif
    winc l
  endwhile

  exec a:window . 'winc w'

  " check windows to the left
  let curwinnr = winnr()
  winc h
  while winnr() != curwinnr
    let curwinnr = winnr()
    if winheight(curwinnr) == winheight(a:window)
      exec origwinnr . 'winc w'
      return 1
    endif
    winc h
  endwhile

  exec origwinnr . 'winc w'
  return 0
endfunction " }}}

function! s:IsInColumn(window) " {{{
  " Determines is the supplied window is in a column of equally sized windows.

  let origwinnr = winnr()
  exec a:window . 'winc w'

  " check windows above
  let curwinnr = winnr()
  winc k
  while winnr() != curwinnr
    let curwinnr = winnr()
    if winwidth(curwinnr) == winwidth(a:window)
      exec origwinnr . 'winc w'
      return 1
    endif
    winc k
  endwhile

  exec a:window . 'winc w'

  " check windows below
  let curwinnr = winnr()
  winc j
  while winnr() != curwinnr
    let curwinnr = winnr()
    if winwidth(curwinnr) == winwidth(a:window)
      exec origwinnr . 'winc w'
      return 1
    endif
    winc j
  endwhile

  exec origwinnr . 'winc w'
  return 0
endfunction " }}}

function! s:RowMinimized(window) " {{{
  " Determines if all windows on a row are minimized.

  let origwinnr = winnr()
  exec a:window . 'winc w'

  let windows = []

  " check windows to the right
  let right = 0
  let curwinnr = winnr()
  winc l
  while winnr() != curwinnr
    let right += 1
    let curwinnr = winnr()
    if winheight(curwinnr) == winheight(a:window)
      if getwinvar(curwinnr, 'minimized') == ''
        exec origwinnr . 'winc w'
        return 0
      else
        call add(windows, curwinnr)
      endif
    elseif winheight(curwinnr) > winheight(a:window)
      let right -= 1
    endif
    winc l
  endwhile

  exec a:window . 'winc w'

  " check windows to the left
  let left = 0
  let curwinnr = winnr()
  winc h
  while winnr() != curwinnr
    let left += 1
    let curwinnr = winnr()
    if winheight(curwinnr) == winheight(a:window)
      if getwinvar(curwinnr, 'minimized') == ''
        exec origwinnr . 'winc w'
        return 0
      else
        call add(windows, curwinnr)
      endif
    elseif winheight(curwinnr) >= winheight(a:window)
      let left -= 1
    endif
    winc h
  endwhile

  " if the window had none to the left or right, then it is in a row all by
  " itself.
  if !right && !left
    call add(windows, a:window)
  endif

  exec origwinnr . 'winc w'
  return len(windows) > 0
endfunction " }}}

function! s:ColumnMinimized(window) " {{{
  " Determines all windows in column are minimized.

  let origwinnr = winnr()
  exec a:window . 'winc w'

  let windows = []

  " check windows above
  let above = 0
  let curwinnr = winnr()
  winc k
  while winnr() != curwinnr
    let above += 1
    let curwinnr = winnr()
    if winwidth(curwinnr) == winwidth(a:window)
      if getwinvar(curwinnr, 'minimized') == ''
        exec origwinnr . 'winc w'
        return 0
      else
        call add(windows, curwinnr)
      endif
    elseif winwidth(curwinnr) >= winwidth(a:window)
      let above -= 1
    endif
    winc k
  endwhile

  exec a:window . 'winc w'

  " check windows below
  let below = 0
  let curwinnr = winnr()
  winc j
  while winnr() != curwinnr
    let below += 1
    let curwinnr = winnr()
    if winwidth(curwinnr) == winwidth(a:window)
      if getwinvar(curwinnr, 'minimized') == ''
        exec origwinnr . 'winc w'
        return 0
      else
        call add(windows, curwinnr)
      endif
    elseif winwidth(curwinnr) >= winwidth(a:window)
      let below -= 1
    endif
    winc j
  endwhile

  " if the window had none above or below, then it is in a column all by
  " itself.
  if !above && !below
    call add(windows, a:window)
  endif

  exec origwinnr . 'winc w'
  return len(windows) > 0
endfunction " }}}

function! s:SaveWinVar(winnr, var) " {{{
  let save = substitute(a:var, '^&', '', '') . '_save'
  if getwinvar(a:winnr, save) == ''
    call setwinvar(a:winnr, save, getwinvar(a:winnr, a:var))
  endif
endfunction " }}}

function! s:RestoreWinVar(winnr, var) " {{{
  let save = substitute(a:var, '^&', '', '') . '_save'
  if getwinvar(a:winnr, save) != ''
    call setwinvar(a:winnr, a:var, getwinvar(a:winnr, save))
  endif
endfunction " }}}

function! s:DelayedCommand(command, ...) " {{{
  " Executes a delayed command.  Useful in cases where one would expect an
  " autocommand event (WinEnter, etc) to fire, but doesn't, or you need a
  " command to execute after other autocommands have finished.
  " Note: Nesting is not supported.  A delayed command cannot be invoke off
  " another delayed command.

  let uid = fnamemodify(tempname(), ':t:r')
  if &updatetime > 1
    exec 'let g:delayed_updatetime_save' . uid . ' = &updatetime'
  endif
  exec 'let g:delayed_command' . uid . ' = a:command'
  let &updatetime = len(a:000) ? a:000[0] : 1
  exec 'augroup delayed_command' . uid
    exec 'autocmd CursorHold * ' .
      \ '  if exists("g:delayed_updatetime_save' . uid . '") | ' .
      \ '    let &updatetime = g:delayed_updatetime_save' . uid . ' | ' .
      \ '    unlet g:delayed_updatetime_save' . uid . ' | ' .
      \ '  endif | ' .
      \ '  exec g:delayed_command' . uid . ' | ' .
      \ '  unlet g:delayed_command' . uid . ' | ' .
      \ '  autocmd! delayed_command' . uid
  exec 'augroup END'
endfunction " }}}

" vim:ft=vim:fdm=marker
