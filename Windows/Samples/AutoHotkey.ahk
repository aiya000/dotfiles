; ^ = Ctrl
; ! = Alt
; # = Super
; + = Shift

; http://ahkwiki.net/MsgBox
; MsgBox, 0x1000, Title, Text, 400

#h::SendInput #{Left}
#j::SendInput #{Down}
#k::SendInput #{Up}
#l::SendInput #{Right}

^!d::SendInput #{Tab}
!^c::SendInput !{F4}
!+4::SendInput +#s
; !+4::Run SnippingTool.exe

; http://ahkwiki.net/Reload
!^r::
  MsgBox, 0x1000, AutoHotKey, Will reload, 400
  Reload
  WinWait,ahk_class #32770,Error at line ,2
  If ErrorLevel = 0
  {
    ControlGetText,v,Static1
    StringGetPos,p,v,.
    p -= 14
    StringMid,line,v,15,%p%
    MsgBox, 0x1000, Error, %line%, 400
    WinWaitNotActive
    WinActivate
  }
  Return

; Vars
EnvGet, Home, HOME

; With bug.n
;; To avoid broke window arrangment
#d::Run explorer.exe %Home%\Desktop

; OpenVR-AdvanceSettings keyboardOne
^+m::Run powershell.exe -ExecutionPolicy RemoteSigned -File C:\Users\aiya0\Desktop\Repository\OneTouch-To-RecordReplay\Record-Replay.ps1

#IfWinActive, ahk_exe vivaldi.exe
  ; ^p::SendInput {Up}
  ; ^n::SendInput {Down}
  ; ^f::SendInput {Right}
  ; ^b::SendInput {Left}
  ^a::SendInput {Home}
  ^e::SendInput {End}
  ^j::SendInput {Enter}
  ^u::SendInput +{Home}{BS}
  ^k::SendInput +{End}{BS}
  ^h::SendInput {BS}
  ^d::SendInput {Del}
  ^,::SendInput +{Left}
  ^.::SendInput +{Right}
  ^[::SendInput +{Up}
  ^]::SendInput +{Down}
  !j::SendInput ^{Enter}
#IfWinActive

#IfWinActive, ahk_class Chrome_WidgetWin_1
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
  ^,::SendInput +{Left}
  ^.::SendInput +{Right}
  ^[::SendInput +{Up}
  ^]::SendInput +{Down}
  !j::SendInput ^{Enter}
#IfWinActive

#IfWinActive, ahk_class ConsoleWindowClass
  ; ^p::SendInput {Up}
  ; ^n::SendInput {Down}
  ; ^f::SendInput {Right}
  ; ^b::SendInput {Left}
  ^a::SendInput {Home}
  ^e::SendInput {End}
  ^j::SendInput {Enter}
  ^u::SendInput +{Home}{BS}
  ^k::SendInput +{End}{BS}
  ^h::SendInput {BS}
  ^d::SendInput {Del}
  ^,::SendInput +{Left}
  ^.::SendInput +{Right}
  ^[::SendInput +{Up}
  ^]::SendInput +{Down}
  !j::SendInput ^{Enter}
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

#IfWinActive, ahk_class UnityContainerWndClass
  ^e::SendInput {End}
  ^u::SendInput +{Home}{BS}
  ^k::SendInput +{End}{BS}
  ^h::SendInput {BS}
  ^f::SendInput +{Left}
  ^b::SendInput +{Right}
#IfWinActive

; Excel
#IfWinActive, ahk_class XLMAIN
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
