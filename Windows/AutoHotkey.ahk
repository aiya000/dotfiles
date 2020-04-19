#h::SendInput #{Left}
#j::SendInput #{Down}
#k::SendInput #{Up}
#l::SendInput #{Right}
F11::SendInput {vk1Dsc07B}
^!c::SendInput !{F4}
^!i::SendInput !{Tab}

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
	^w::SendInput ^+{Left}{BS}
	;
	^v::SendInput !{Space}ep
#IfWinActive

; Skype
#IfWinActive, ahk_class ApplicationFrameWindow
	; Support text edit
	^p::SendInput {Up}
	^n::SendInput {Down}
	^f::SendInput {Right}
	^b::SendInput {Left}
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput ^{Enter}
	^m::SendInput {Enter}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
	^w::SendInput ^+{Left}{BS}
	^h::SendInput {BS}
	^d::SendInput {Del}
	^,::SendInput +{Left}
	^.::SendInput +{Right}
	^[::SendInput +{Up}
	^]::SendInput +{Down}
#IfWinActive

; Windows mailer
#IfWinActive, ahk_class Shell_HostingContainer
	; Support text edit
	^p::SendInput {Up}
	^n::SendInput {Down}
	^f::SendInput {Right}
	^b::SendInput {Left}
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput ^{Enter}
	^m::SendInput {Enter}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
	^w::SendInput ^+{Left}{BS}
	^h::SendInput {BS}
	^d::SendInput {Del}
	^,::SendInput +{Left}
	^.::SendInput +{Right}
	^[::SendInput +{Up}
	^]::SendInput +{Down}
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
	^j::SendInput ^{Enter}
	^m::SendInput {Tab}{Enter}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
	^w::SendInput ^+{Left}{BS}
	^h::SendInput {BS}
	^d::SendInput {Del}
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
	^w::SendInput ^+{Left}{BS}
	^h::SendInput {BS}
	^d::SendInput {Del}
#IfWinActive

; Adobe Reader
#IfWinActive, ahk_class AcrobatSDIWindow
	; Easily vimnize
	h::SendInput {Left}
	j::SendInput {Down}
	k::SendInput {Up}
	l::SendInput {Right}
	^f::SendInput {PgDn}
	^b::SendInput {PgUp}
#IfWinActive

; Franz
#IfWinActive, ahk_exe Franz.exe
	^p::SendInput {Up}
	^n::SendInput {Down}
	^f::SendInput {Right}
	^b::SendInput {Left}
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput ^{Enter}
	^m::SendInput {Enter}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
	^w::SendInput ^+{Left}{BS}
	^h::SendInput {BS}
	^d::SendInput {Del}
	^,::SendInput +{Left}
	^.::SendInput +{Right}
	^[::SendInput +{Up}
	^]::SendInput +{Down}
	!j::SendInput ^{Enter}
#IfWinActive

; Notepad
#IfWinActive, ahk_class Notepad
	^p::SendInput {Up}
	^n::SendInput {Down}
	^f::SendInput {Right}
	^b::SendInput {Left}
	^a::SendInput {Home}
	^e::SendInput {End}
	^j::SendInput {Enter}
	^u::SendInput +{Home}{BS}
	^k::SendInput +{End}{BS}
	^w::SendInput ^+{Left}{BS}
	^h::SendInput {BS}
	^d::SendInput {Del}
	^,::SendInput +{Left}
	^.::SendInput +{Right}
	^[::SendInput +{Up}
	^]::SendInput +{Down}
#IfWinActive
