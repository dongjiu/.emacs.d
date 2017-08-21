; Disable AutoHotKey
^!Esc::
MsgBox Exit AutHotKey
ExitApp

; Open file in visual studio with Emacs
^+m::
IfWinActive, ahk_exe devenv.exe
{
Send !{-}
Sleep 200
Send u
WinActivate, ahk_exe emacs.exe
WinWaitActive, ahk_exe emacs.exe
Send ^{x}{r}{w}{l} ; save window config to register 'l'
Send ^{x}^{f}^{y}{Enter} ; open file
Send ^{x}{1} ; use single window
}
return

^!b::
IfWinActive, ahk_exe firefox.exe
{
Send, ^{l}
Sleep, 100
Send, !{w}
AddBookmarkInEmacs()
}
If WinActive("ahk_exe iexplore.exe") or WinActive("ahk_exe chrome.exe")
{
Send, ^{l}
Sleep, 100
Send, ^{c}
AddBookmarkInEmacs()
}
return

AddBookmarkInEmacs()
{
WinActivate, ahk_exe emacs.exe
Send, ^{;}
Send, b
}