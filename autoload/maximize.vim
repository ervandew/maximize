" Author:  Eric Van Dewoestine
" Version: $Revision$
"
" Description: {{{
"   see http://eclim.sourceforge.net/vim/common/maximize.html
"
" License:
"
" Copyright (c) 2005 - 2006
"
" Licensed under the Apache License, Version 2.0 (the "License");
" you may not use this file except in compliance with the License.
" You may obtain a copy of the License at
"
"      http://www.apache.org/licenses/LICENSE-2.0
"
" Unless required by applicable law or agreed to in writing, software
" distributed under the License is distributed on an "AS IS" BASIS,
" WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
" See the License for the specific language governing permissions and
" limitations under the License.
"
" }}}

" Global Variables {{{
  if !exists('g:MaximizeExcludes')
    let g:MaximizeExcludes =
      \ '\(ProjectTree_*\|__Tag_List__\|-MiniBufExplorer-\|^\[.*\]$\)'
  endif
  if !exists('g:MaximizeMinWinHeight')
    let g:MaximizeMinWinHeight = 0
  endif
  if !exists('g:MaximizeMinWinWidth')
    let g:MaximizeMinWinWidth = 0
  endif
  if !exists('g:MaximizeQuickfixHeight')
    let g:MaximizeQuickfixHeight = 10
  endif
" }}}

" MaximizeWindow() {{{
function! eclim#display#maximize#MaximizeWindow ()
  " disable any minimize settings
  call eclim#display#maximize#ResetMinimized()

  " get the window that is maximized
  let maximized = s:GetMaximizedWindow()
  if maximized
    call s:DisableMaximizeAutoCommands()
    call eclim#display#maximize#RestoreWindows(maximized)
  else
    exec "set winminwidth=" . g:MaximizeMinWinWidth
    exec "set winminheight=" . g:MaximizeMinWinHeight
    call s:MaximizeNewWindow()
  endif
endfunction " }}}

" MinimizeWindow() {{{
function! eclim#display#maximize#MinimizeWindow (...)
  let curwinnum = winnr()

  exec "set winminheight=" . g:MaximizeMinWinHeight
  exec "set winminwidth=" . g:MaximizeMinWinWidth
  call s:DisableMaximizeAutoCommands()

  " first turn off maximized if enabled
  let maximized = s:GetMaximizedWindow()
  if maximized
    call eclim#display#maximize#RestoreWindows(maximized)
  endif

  let args = []
  if len(a:000) == 0
    let args = [bufnr('%')]
  else
    let args = a:000[:]
    call map(args, 's:BufferNumber(v:val)')
  endif

  " first loop through and mark the buffers
  for buffer in args
    let winnum = bufwinnr(buffer)
    if winnum != -1
      call setwinvar(winnum, "minimized", 1)
    endif
  endfor

  call s:Reminimize()

  " second loop sweep through and resize
  "for buffer in args
  "  let winnum = bufwinnr(buffer)
  "  if winnum != -1
  "    let row_wins = s:RowMinimized(winnum)
  "    let column_wins = s:ColumnMinimized(winnum)
  "    let vertical_split = s:IsVerticalSplit(winnum)
  "    echom 'row_wins    = ' . string(row_wins)
  "    echom 'column_wins = ' . string(column_wins)
  "    if vertical_split && len(row_wins) && len(column_wins)
  "      let windows = column_wins
  "      let cmd = 'vertical <window>resize 0'
  "      let winvar = '&winfixwidth'

  "    elseif len(row_wins) && len(column_wins)
  "      let windows = row_wins
  "      let cmd = '<window>resize 0'
  "      let winvar = '&winfixheight'

  "    elseif len(column_wins)
  "      let windows = column_wins
  "      let cmd = 'vertical <window>resize 0'
  "      let winvar = '&winfixwidth'

  "    elseif s:IsInColumn(winnum)
  "      let windows = []
  "      let cmd = '<window>resize 0'
  "      let winvar = '&winfixheight'

  "    elseif s:IsInRow(winnum)
  "      let windows = []
  "      let cmd = 'vertical <window>resize 0'
  "      let winvar = '&winfixwidth'

  "    elseif vertical_split
  "      let windows = column_wins
  "      let cmd = 'vertical <window>resize 0'
  "      let winvar = '&winfixwidth'

  "    else
  "      let windows = row_wins
  "      let cmd = '<window>resize 0'
  "      let winvar = '&winfixheight'
  "    endif

  "    echom 'windows = ' . string(windows)
  "    echom 'cmd = ' . cmd
  "    echom 'winvar = ' . winvar
  "    call add(windows, winnum)
  "    for window in windows + [winnum]
  "      exec substitute(cmd, '<window>', window, 'g')
  "      call setwinvar(window, winvar, 1)
  "    endfor
  "  endif
  "endfor

  " ensure we end up in the window we started in
  "exec curwinnum . 'winc w'
  "echom 'min end ' . curwinnum

  "winc =

  "call s:RestoreFixedWindows()
  "call s:EnableMinimizeAutoCommands()
endfunction " }}}

" MaximizeNewWindow() {{{
function! s:MaximizeNewWindow ()
  if expand('%') !~ g:MaximizeExcludes
    call s:DisableMaximizeAutoCommands()
    call s:GetMaximizedWindow()
    let w:maximized = 1
    call s:ResizeWindows()
    call s:EnableMaximizeAutoCommands()
  endif
endfunction " }}}

" GetMaximizedWindow() {{{
function! s:GetMaximizedWindow ()
  let winend = winnr('$')
  let winnum = 1
  while winnum <= winend
    let max = getwinvar(winnum, "maximized")
    if max
      return winnum
    endif
    let winnum = winnum + 1
  endwhile

  return 0
endfunction " }}}

" ResetMinimized() {{{
function! eclim#display#maximize#ResetMinimized ()
  call s:DisableMinimizeAutoCommands()
  let winend = winnr('$')
  let winnum = 1
  while winnum <= winend
    call setwinvar(winnum, "minimized", 0)
    call setwinvar(winnum, "&winfixheight", 0)
    call setwinvar(winnum, "&winfixwidth", 0)
    let winnum = winnum + 1
  endwhile
endfunction " }}}

" ResizeWindows() {{{
function! s:ResizeWindows ()
  winc |
  winc _

  let curwindow = winnr()
  if &ft == 'qf'
    let quickfixwindow = curwindow
  endif

  winc w
  while curwindow != winnr()
    let window = winnr()
    let buffername = expand('%')
    let quickfix = (&ft == 'qf')
    if quickfix
      let quickfixwindow = window
    endif
    winc w
    if !quickfix && buffername !~ g:MaximizeExcludes
      exec "vertical " . window . "resize 0"
      exec window . "resize 0"
    endif
  endwhile

  resize +100
  vertical resize +100
  if exists("quickfixwindow")
    exec quickfixwindow . "resize " . g:MaximizeQuickfixHeight
    exec "vertical " . quickfixwindow . "resize"
    winc |
  endif
  unlet curwindow

  call s:RestoreFixedWindows()
endfunction " }}}

" DisableMaximizeAutoCommands() {{{
function! s:DisableMaximizeAutoCommands ()
  augroup maximize
    autocmd!
  augroup END
endfunction " }}}

" EnableMaximizeAutoCommands() {{{
function! s:EnableMaximizeAutoCommands ()
  call s:DisableMaximizeAutoCommands()
  call s:DisableMinimizeAutoCommands()
  augroup maximize
    autocmd!
    autocmd BufReadPost quickfix call s:FixQuickfix(1)
    autocmd BufUnload * call s:CloseQuickfix()
    autocmd BufWinEnter,WinEnter * call s:MaximizeNewWindow()
  augroup END
endfunction " }}}

" DisableMinimizeAutoCommands() {{{
function! s:DisableMinimizeAutoCommands ()
  augroup minimize
    autocmd!
  augroup END
endfunction " }}}

" EnableMinimizeAutoCommands() {{{
function! s:EnableMinimizeAutoCommands ()
  call s:DisableMaximizeAutoCommands()
  augroup minimize
    autocmd!
    autocmd BufReadPost quickfix call s:FixQuickfix(0)
    autocmd BufWinEnter,WinEnter * call s:Reminimize()
  augroup END
endfunction " }}}

" FixQuickfix(maximize) {{{
function s:FixQuickfix (maximize)
  exec "resize " . g:MaximizeQuickfixHeight
  set winfixheight

  let curwindow = winnr()

  if a:maximize
    "return to previous window to restore it's maximized
    winc p
    call s:MaximizeNewWindow()

    exec curwindow . "winc w"
  endif
endfunction " }}}

" CloseQuickfix() {{{
function s:CloseQuickfix ()
  if expand('<afile>') == ""
    "echom "Closing nofile (maybe quickfix)"
    " can't figure out how to re-maximize if cclose is called from a maximized
    " window, so just resetting.
    call s:GetMaximizedWindow()
  endif
endfunction " }}}

" Reminimize() {{{
" Invoked when changing windows to ensure that any minimized windows are
" returned to their minimized state.
function s:Reminimize ()
  call s:DisableMinimizeAutoCommands()
  let curwinnum = winnr()
  let winend = winnr('$')
  let winnum = 1
  let commands = []
  while winnum <= winend
    if bufname(winbufnr(winnum)) !~ g:MaximizeExcludes
      let minimized = getwinvar(winnum, "minimized")
      if minimized
        let row_wins = s:RowMinimized(winnum)
        let column_wins = s:ColumnMinimized(winnum)
        "let vertical_split = s:IsVerticalSplit(winnum)

        "echom 'buffer = ' . winbufnr(winnum)
        "echom '  row minimized    = ' . len(row_wins)
        "echom '  column minimized = ' . len(column_wins)
        "echom '  in row           = ' . s:IsInRow(winnum)
        "echom '  in column        = ' . s:IsInColumn(winnum)

        "if vertical_split && len(row_wins) && len(column_wins)
        "  exec "vertical " . winnum . "resize 0"
        "  call setwinvar(winnum, "&winfixwidth", 1)

        if len(row_wins) && len(column_wins)
          call add(commands, winnum . "resize 0")
          call setwinvar(winnum, "&winfixheight", 1)

        elseif len(column_wins)
          call add(commands, "vertical " . winnum . "resize 0")
          call setwinvar(winnum, "&winfixwidth", 1)

        elseif s:IsInRow(winnum)
          call add(commands, "vertical " . winnum . "resize 0")
          call setwinvar(winnum, "&winfixwidth", 1)

        elseif s:IsInColumn(winnum)
          call add(commands, winnum . "resize 0")
          call setwinvar(winnum, "&winfixheight", 1)

        "elseif vertical_split
        "  exec "vertical " . winnum . "resize 0"
        "  call setwinvar(winnum, "&winfixwidth", 1)

        else
          call add(commands, winnum . "resize 0")
          call add(commands, "vertical " . winnum . "resize 0")
          call setwinvar(winnum, "&winfixheight", 1)
          call setwinvar(winnum, "&winfixwidth", 1)
        endif
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

  call s:RestoreFixedWindows()

  winc =

  call s:RestoreFixedWindows()
  call s:EnableMinimizeAutoCommands()
endfunction " }}}

" RestoreWindows(maximized) {{{
function! eclim#display#maximize#RestoreWindows (maximized)
  " reset the maximized var.
  if a:maximized
    call setwinvar(a:maximized, "maximized", 0)
  endif

  winc _
  winc =

  call s:RestoreFixedWindows()

  let curwinnr = winnr()
  winc w
  while winnr() != curwinnr
    if &ft == 'qf'
      exec "resize " . g:MaximizeQuickfixHeight
    endif
    winc w
  endwhile
endfunction " }}}

" RestoreFixedWindows() {{{
function! s:RestoreFixedWindows ()
  "fixes TList pane that ends up getting resized
  if exists("g:TagList_title")
    if bufwinnr(g:TagList_title) != -1
      exec "vertical " . bufwinnr(g:TagList_title) .
        \ "resize " . g:Tlist_WinWidth
    endif
  endif
  if exists("g:EclimProjectTreeTitle")
    let winnr = bufwinnr(g:EclimProjectTreeTitle . '_*')
    if winnr != -1
      exec "vertical " . winnr . "resize " . g:EclimProjectTreeWidth
    endif
  endif
endfunction " }}}

" BufferNumber() {{{
" Convert a string buffer # to an int buffer #
function! s:BufferNumber (number)
  exec "return winbufnr(bufwinnr(" . a:number . "))"
endfunction " }}}

" IsVerticalSplit(window) {{{
" Determines if the current window is vertically split.
function! s:IsVerticalSplit (window)
  let origwinnr = winnr()

  exec a:window . 'winc w'
  let curwinnr = winnr()

  " check to the right
  winc l
  if winnr() != curwinnr && expand('%') !~ g:MaximizeExcludes
    return 1
  endif

  exec a:window . 'winc w'

  " check to the left
  winc h
  if winnr() != curwinnr && expand('%') !~ g:MaximizeExcludes
    return 1
  endif

  exec origwinnr . 'winc w'
  return 0
endfunction " }}}

" IsInRow(window) {{{
" Determines if the supplied window is in a row of equally sized windows.
function! s:IsInRow (window)
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
    let buffer = bufnr('%')
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

" IsInColumn(window) {{{
" Determines is the supplied window is in a column of equally sized windows.
function! s:IsInColumn (window)
  let origwinnr = winnr()
  exec a:window . 'winc w'

  " check windows above
  let curwinnr = winnr()
  winc k
  while winnr() != curwinnr
    let buffer = bufnr('%')
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
    let buffer = bufnr('%')
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

" RowMinimized(window) {{{
" Determines all windows on a row are minimized.
function! s:RowMinimized (window)
  let origwinnr = winnr()
  exec a:window . 'winc w'

  let windows = []

  " check windows to the right
  let curwinnr = winnr()
  winc l
  while winnr() != curwinnr
    let buffer = bufnr('%')
    let curwinnr = winnr()
    if winheight(curwinnr) == winheight(a:window) &&
        \ expand('%') !~ g:MaximizeExcludes
      if getwinvar(curwinnr, 'minimized') == ''
        exec origwinnr . 'winc w'
        return []
      else
        call add(windows, curwinnr)
      endif
    endif
    winc l
  endwhile

  exec a:window . 'winc w'

  " check windows to the left
  let curwinnr = winnr()
  winc h
  while winnr() != curwinnr
    let buffer = bufnr('%')
    let curwinnr = winnr()
    if winheight(curwinnr) == winheight(a:window) &&
        \ expand('%') !~ g:MaximizeExcludes
      if getwinvar(curwinnr, 'minimized') == ''
        exec origwinnr . 'winc w'
        return []
      else
        call add(windows, curwinnr)
      endif
    endif
    winc h
  endwhile

  exec origwinnr . 'winc w'
  return windows
endfunction " }}}

" ColumnMinimized(window) {{{
" Determines all windows in column are minimized.
function! s:ColumnMinimized (window)
  let origwinnr = winnr()
  exec a:window . 'winc w'

  let windows = []

  " check windows above
  let curwinnr = winnr()
  winc k
  while winnr() != curwinnr
    let buffer = bufnr('%')
    let curwinnr = winnr()
    if winwidth(curwinnr) == winwidth(a:window) &&
        \ expand('%') !~ g:MaximizeExcludes
      if getwinvar(curwinnr, 'minimized') == ''
        exec origwinnr . 'winc w'
        return []
      else
        call add(windows, curwinnr)
      endif
    endif
    winc k
  endwhile

  exec a:window . 'winc w'

  " check windows below
  let curwinnr = winnr()
  winc j
  while winnr() != curwinnr
    let buffer = bufnr('%')
    let curwinnr = winnr()
    if winwidth(curwinnr) == winwidth(a:window) &&
        \ expand('%') !~ g:MaximizeExcludes
      if getwinvar(curwinnr, 'minimized') == ''
        exec origwinnr . 'winc w'
        return []
      else
        call add(windows, curwinnr)
      endif
    endif
    winc j
  endwhile

  exec origwinnr . 'winc w'
  return windows
endfunction " }}}

" NavigateWindows(cmd) {{{
" Used navigate windows by skipping minimized windows.
function! eclim#display#maximize#NavigateWindows (wincmd)
  let start = winnr()
  let lastwindow = start

  exec a:wincmd
  while exists('w:minimized') && w:minimized && winnr() != lastwindow
    let lastwindow = winnr()
    exec a:wincmd
  endwhile

  if exists('w:minimized') && w:minimized
    exec start . 'wincmd w'
  endif
endfunction " }}}

" vim:ft=vim:fdm=marker
