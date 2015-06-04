; cmd prompt
#IfWinActive, ahk_class ConsoleWindowClass
	; Emulate *NIX Shell
	^p::SendInput {Up}
	^n::SendInput {Down}
	^f::SendInput {Right}
	^b::SendInput {Left}
	^a::SendInput {Home}
	^e::SendInput {End}
	^d::SendInput {Delete}
	^j::SendInput {Enter}
	^k::SendInput {F4}{Space}
	^u::SendInput {Esc}  ; TODO: incomplete
	^l::SendInput {Esc}cls{Enter}
	;
	^w::SendInput {NOP}  ; TODO: emulate C-w
	^v::SendInput !{Space}ep
#IfWinActive

; Firefox ( and Thunderbird )
#IfWinActive, ahk_class MozillaWindowClass
	; Support vimperator
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput {Enter}
	^h::SendInput {BS}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
	^l::SendINput {Esc}
#IfWinActive

; IP Messenger
#IfWinActive, ahk_class #32770
	; Support text edit
	^p::SendInput {Up}
	^n::SendInput {Down}
	^f::SendInput {Right}
	^b::SendInput {Left}
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput {Enter}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
	^h::SendInput {BS}
	^d::SendInput {Del}
	Ctrl & Enter::SendInput {Tab}{Enter}
	^,::SendInput +{Left}
	^.::SendInput +{Right}
	^[::SendInput +{Up}
	^]::SendInput +{Down}
#IfWinActive

; LINE
#IfWinActive, ahk_class Qt5QWindowIcon
	; Support text edit
	^p::SendInput {Up}
	^n::SendInput {Down}
	^f::SendInput {Right}
	^b::SendInput {Left}
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput {Enter}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
	^h::SendInput {BS}
	^d::SendInput {Del}
#IfWinActive
