; cmd prompt
#IfWinActive, ahk_class ConsoleWindowClass
	^p::SendInput {Up}
	^n::SendInput {Down}
	^f::SendInput {Right}
	^b::SendInput {Left}
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput {Enter}
	^k::SendInput {F4}{Space}
	^u::SendInput {Esc}
	^l::SendInput {Esc}cls{Enter}
	^V::SendInput !{Space}ep
#IfWinActive

; Firefox, Thunderbird
#IfWinActive, ahk_class MozillaWindowClass
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput {Enter}
	^h::SendInput {BS}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
#IfWinActive

; IP Messenger
#IfWinActive, ahk_class #32770
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
