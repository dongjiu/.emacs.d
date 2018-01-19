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
    Send ^{x}^{f}^{a}^{y}^{k}{Enter} ; open file
    Send ^{x}{1} ; use single window
}
return

^!b::
If WinActive("ahk_exe iexplore.exe")
{
    Send, ^{l}
    Sleep, 500
    Send, +{F10}
    Sleep, 500
    Send, {c}
    OpenBookmarksInEmacs()
}
else If WinActive("ahk_exe firefox.exe")
{
    Send, ^{l}
    Sleep, 200
    Send, !{w}
    OpenBookmarksInEmacs()
}
else If WinActive("ahk_exe chrome.exe")
{
    Send, ^{l}
    Sleep, 200
    Send, ^{c}
    OpenBookmarksInEmacs()
}
else If WinActive("ahk_exe explorer.exe")
{
    Send, !{d}
    Sleep, 200
    Send, ^{c}
    OpenBookmarksInEmacs()
}
return

^!+b::
OpenBookmarksInEmacs()
return

OpenBookmarksInEmacs()
{
    WinActivate, ahk_exe emacs.exe
    Send, ^{;}
    Send, b
}


^!o::
Run C:\Users\donzhu\Desktop\bookmark_page_onenote.url