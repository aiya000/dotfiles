; ^ = Ctrl
; ! = Alt
; # = Super
; + = Shift

#h::SendInput #{Left}
#j::SendInput #{Down}
#k::SendInput #{Up}
#l::SendInput #{Right}

^!d::SendInput #{Tab}
!^c::SendInput !{F4}

; With bug.n
;; To avoid broke window arranging
EnvGet, Home, HOME
#d::Run explorer.exe %Home%\Desktop

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

; Adobe Reader
#IfWinActive, ahk_class AcrobatSDIWindow
  h::SendInput {Left}
  j::SendInput {Down}
  k::SendInput {Up}
  l::SendInput {Right}
  ^b::SendInput {PgUp}
  ^f::SendInput {PgDn}
  /::SendInput ^f
#IfWinActive

#IfWinActive, ahk_class SUMATRA_PDF_FRAME
  h::SendInput {Left}
  j::SendInput {Down}
  k::SendInput {Up}
  l::SendInput {Right}
  ; ^b::SendInput {PgUp}
  ; ^f::SendInput {PgDn}
  ^b::SendInput {Left}
  ^f::SendInput {Right}
  /::SendInput ^f
#IfWinActive
