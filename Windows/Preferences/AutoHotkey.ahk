; ^ = Ctrl
; ! = Alt
; # = Super
; + = Shift

; http://ahkwiki.net/MsgBox
; MsgBox, 0x1000, Title, Text, 400

; Arrange windows
#h::SendInput #{Left}
#j::SendInput #{Down}
#k::SendInput #{Up}
#l::SendInput #{Right}

; Select all
; NOTE: For applications that heavily use text input, Ctrl + a is remapped to Home, so use this instead
#a::SendInput ^a

; Clip
#x::SendInput #+s

; Select captured texts
#t::SendInput #+t

; Kill Window
#c::SendInput !{F4}
!^c::SendInput !{F4}

; Show Windows
^!d::SendInput #{Tab}

; Snip
; !+4::SendInput +#s
!+4::Run SnippingTool.exe
!+s::Run SnippingTool.exe

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

; OpenVR-AdvanceSettings keyboardOne
^+m::Run powershell.exe -ExecutionPolicy RemoteSigned -File C:\Users\aiya0\Desktop\Repository\OneTouch-To-RecordReplay\Record-Replay.ps1

; Replace the Windows default clipboard manager
; because it cannot paste contents to Windows Terminal
#v::
  Run C:\Program Files\Ditto\Ditto.exe /Open
Return

; Bash-like key mapping
#IfWinActive, ahk_exe Discord.exe ; {{{
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
  ; Disable accidentally triggering page reload while typing messages
  ^r::Return
#IfWinActive ; }}}
#IfWinActive, ahk_exe slack.exe ; {{{
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
#IfWinActive ; }}}
#IfWinActive, ahk_exe Code.exe ; {{{
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
#IfWinActive ; }}}
#IfWinActive, ahk_exe Code - Insiders.exe ; {{{
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
#IfWinActive ; }}}
#IfWinActive, ahk_exe draw.io.exe ; {{{
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
  ^,::SendInput +{Left}
  ^.::SendInput +{Right}
  ^[::SendInput +{Up}
  ^]::SendInput +{Down}
  !j::SendInput ^{Enter}
#IfWinActive ; }}}
#IfWinActive, ahk_exe Chrome.exe ; {{{
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
#IfWinActive ; }}}

; Bash-like key mapping.
; However, for default browsers, let SurfingKeys handle as much as possible, and only map keys that cannot be overridden internally.
; Chrome is excluded as it is used for debugging at work.
;
#IfWinActive, ahk_exe floorp.exe ; {{{
  ; TODO: Temporary. Refer to the comment below.
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
  ; TODO: Until SurfingKeys can be configured, use the above keymap and temporarily comment out the following original mappings. Once SurfingKeys is set up, restore them.
  ; ^a::SendInput {Home}
  ; ^,::SendInput +{Left}
  ; ^.::SendInput +{Right}
  ; ^[::SendInput +{Up}
  ; ^]::SendInput +{Down}
#IfWinActive ; }}}
#IfWinActive, ahk_exe vivaldi.exe ; {{{
  ; Due to updates in Vivaldi, SurfingKeys stopped working properly, requiring several key mappings on this side
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
#IfWinActive ; }}}

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
