; ^ = Ctrl
; ! = Alt
; # = Super
; + = Shift

; http://ahkwiki.net/MsgBox
; MsgBox, 0x1000, Title, Text, 400

; ウィンドウを配置
#h::SendInput #{Left}
#j::SendInput #{Down}
#k::SendInput #{Up}
#l::SendInput #{Right}

; Select all
; NOTE: テキスト入力を多用するアプリケーションに対しては、`Ctrl + a`に`Home`へのキーマップを割り当てるため、代わりにこちらを使う
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

; TODO: 多分全く動いてない
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

#v::
  Run C:\Program Files\Ditto\Ditto.exe /Open
Return

; bashライクキーマッピング
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
  ; メッセージのタイプ中に間違えてページリロードすることが多いので、無効にする
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

; bashライクキーマッピング。
; ただし常用のWebブラウザにはできるだけSurfingKeysに制御をして欲しいので、アプリケーション内部レイヤーではどうしようもないところだけキーマッピングする。
; Chromeは業務でデバッグに使用しているので、除外。
#IfWinActive, ahk_class MozillaWindowClass ; {{{
  ; TODO: tmp. 下記コメント参照
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
  ; TODO: Surfingkeysが設定できるまで、上記キーマップを使用し、本来の以下を一時的にコメントアウトしている。Surfingkeysが設定できたら、元に戻す
  ; ^a::SendInput {Home}
  ; ^,::SendInput +{Left}
  ; ^.::SendInput +{Right}
  ; ^[::SendInput +{Up}
  ; ^]::SendInput +{Down}
#IfWinActive ; }}}
#IfWinActive, ahk_exe vivaldi.exe ; {{{
  ; Vivaldiの更新でSurfingKeysが色々動かなくなって、こちら側でなんとかしていたので、いくつかキーマッピングが多い
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
