" Vim Game Of Life 1.0
" Source and run with :Gol
" hit q and wait a while to stop
" note that the script opens blindly a new window, which is dirty
" Arnaud 'nohar' Cornet

let s:WIDTH=70 " make that > 4
let s:HEIGHT=20 " that too
let s:WAIT_TIME=1

function! Gol_init()
	syntax match Golbg "L"
	syntax match Golfg "D"
	highlight Golbg ctermfg=Black ctermbg=Black guibg=#101010 guifg=#101010
	highlight Golfg ctermfg=Gray ctermbg=White guibg=#f0f0f0 guifg=#f0f0f0
	execute "normal gg0i\<space>\<esc>".s:WIDTH."aD\<esc>yy".s:HEIGHT."p"
	execute "normal ".(s:HEIGHT/2 - 1)."G".(s:WIDTH/2 - 1)."|rLlrLh"
	execute "normal jrLhrLjlrL"
	call setline(s:HEIGHT+1," q key to stop")
endfunction

function! Gol_live_neighbors_nr(buffy_prec,buffy,buffy_next,x)
	let n = 0
	let e = a:buffy_prec[(a:x-1)%(s:WIDTH)+1]
	if e == "L" || e == "K"
		let n = n+1
	endif
	let e = a:buffy_prec[(a:x)%(s:WIDTH)+1]
	if e == "L" || e == "K"
		let n = n+1
	endif
	let e = a:buffy_prec[(a:x+1)%(s:WIDTH)+1]
	if e == "L" || e == "K"
		let n = n+1
	endif
	let e = a:buffy[(a:x-1)%(s:WIDTH)+1]
	if e == "L" || e == "K"
		let n = n+1
	endif
	let e = a:buffy[(a:x+1)%(s:WIDTH)+1]
	if e == "L" || e == "K"
		let n = n+1
	endif
	let e = a:buffy_next[(a:x-1)%(s:WIDTH)+1]
	if e == "L" || e == "K"
		let n = n+1
	endif
	let e = a:buffy_next[a:x%(s:WIDTH)+1]
	if e == "L" || e == "K"
		let n = n+1
	endif
	let e = a:buffy_next[(a:x+1)%(s:WIDTH)+1]
	if e == "L" || e == "K"
		let n = n+1
	endif
	return n
endfunction

function! Gol_update()
	let y = 1
	while y <= s:HEIGHT
		execute "normal ".y."G0"
		let buffy_prec = getline((y-1-1)%(s:HEIGHT)+1)
		let buffy=getline(y)
		let buffy_next = getline((y-1+1)%(s:HEIGHT)+1)
		let x = 1
		while x <= s:WIDTH
			if x == s:WIDTH
	"			call getchar()
			endif
			execute "normal l"
			if x == s:WIDTH
	"			call getchar()
			endif
			let ln = Gol_live_neighbors_nr(buffy_prec,buffy,buffy_next,x-1)
			let cell = buffy[x]
			if ((cell == "D" || cell == "B") && ln == 3)
				execute "normal rB"
			elseif cell == "L" && (ln != 2 && ln != 3)
				execute "normal rK"
			endif

			let x = x+1
		endwhile
		let y = y+1
	endwhile
	let y = 1
	while y <= s:HEIGHT
		call setline(y,substitute(getline(y),"B","L","g"))
		call setline(y,substitute(getline(y),"K","D","g"))
		let y=y+1
	endwhile
endfunction

function! Gol_loop()
	execute "new"
	"dirty
	execute "normal \<C-w>20+"
	call Gol_init()
	let c=0
	while c != char2nr("q")
		let c = getchar(0)
		execute "sleep ".s:WAIT_TIME."m"
		call Gol_update()
		redraw
	endwhile
endfunction

command! Gol call Gol_loop()
