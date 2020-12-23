; =====================================================================================
; AHK Version ...: AHK 1.1.32.00 (Unicode 64-bit) - September 13, 2015
; Platform ......: Windows 10
; Language ......: English (en-US)
; Author ........: TyGreeny
; =====================================================================================

;~ #Warn                                    ;Enable every type of warning.
#Warn, UseUnsetLocal, StdOut                ;Warn when local var used before set.
SendMode Input                              ;Recommended for new scripts.
#SingleInstance                             ;Replaces old instance with new one.
SetWorkingDir %A_ScriptDir%\AutoHotkey\     ;Set script path as working directory.

; Run %A_ScriptDir%\AutoHotKey\Master.ahk

; ExitApp

;{ ============================== Notes ===============================================
; All directives(that are settings) and many commands appear here irvrespective of its need
; The directives/commands that are default are commented by ";;" ,
; those not needed by ";;;" and those not desired by ";~ "
;} ====================================================================================

Suspend On
#SingleInstance Force
#include %A_ScriptDir%

#include %A_ScriptDir%

;Set script path as working directory.
SetWorkingDir %A_ScriptDir%    
;Runs reloadAsAdmin task 
reloadAsAdmin_Task()            
;For tray menu options | SCR_Name = Name of file without ext
global SCR_Name                 
SplitPath, A_ScriptFullPath, , , , SCR_Name,
;~ SCR_UserDir = C:\Users\%USER%
global SCR_UserDir              
SplitPath, A_MyDocuments, , SCR_UserDir, , ,s
;Sets path for tray menu options
global SCR_Path = A_ScriptDir   

#NoEnv                          ;Donot use default environmental variables.
#Persistent                     ;Keeps a script permanently running
#SingleInstance Force           ;Replaces old instance with new one
#UseHook                        ;For machine level hotkeys. Removes need to use $ in hotkeys
#Hotstring * ? B C K0 Z         ;Hotstring global settings
#InstallKeybdHook               ;Installs Keyboard hook
#InstallMouseHook               ;Installs Mouse hook
#KeyHistory 250                 ;maximum number of keyboard and mouse events displayed
#MaxHotkeysPerInterval 200      ;Rate a warning dialog will be displayed
#MaxMem 256                     ;Allows variables with heavy memory usage.
#MaxThreads 255                 ;Allows # of pseudo-threads to run simultaneously.
#MaxThreadsPerHotkey 1          ;Hotkey can not be launched when it is already running
Thread, interrupt, 50           ;Minimum time for interupt
SendMode Input                  ;How the script sends simulated keys.
Process, Priority, , R          ;Runs Script at High process priority for best performance
SetBatchLines, -1               ;Never sleep for best performance
SetKeyDelay, 0, 0               ;Smallest ossible delay
SetMouseDelay, 0                ;Smallest possible delay
SetDefaultMouseSpeed, 0         ;Move the mouse instantly
SetWinDelay, 0                  ;Smallest possible delay
SetControlDelay, 0              ;Smallest possible delay
SetTitleMatchMode, 2            ;A window's title can contain WinTitle anywhere inside it
BlockInput, Mouse               ;Keyboard/mouse is blocked during Click or MouseClick
CoordMode, ToolTip, Screen      ;Tooltip co-ords are specified in global co-ords
CoordMode, Pixel, Screen        ;Co-ords for image/pixelSearch are specified in global co-ords
CoordMode, Mouse, Screen        ;Mouse co-ords are specified in global co-ords
CoordMode, Caret, Screen        ;A_CaretX/Y are specified in global co-ords
CoordMode, Menu, Screen         ;Co-ords for "Menu, Show" are specified in global co-ords
SetNumLockState, On

caseMenu.__new()

trayMenu()

delayedTimer.start()
delayedTimer.firstRun()

Toast.show({title:{text:"Script Loaded"},sound:False,life:1000})
suspend Off


; =====================================================================================
; AHK Version ...: AHK 1.1.32.00 (Unicode 64-bit) - December, 20th 2020
; Platform ......: Windows 10
; Language ......: English (en-US)
; Author ........: TyGreeny
; =====================================================================================

^Esc::
	if (escPresses > 0){
		 escPresses += 1
		 return
	}
	escPresses := 1
	KeyWait, Esc, U
	time1 := A_TimeSinceThisHotkey
	SetTimer, escKey, -300 ; Wait for more presses within a 300 millisecond window.
return

escKey:
	if (escPresses = 1){
		if (time1>1000)
		use_TrayIcon(A_ScriptFullPath, "Open")
	} else if (escPresses = 2) {
	use_TrayIcon(A_ScriptFullPath, "Reload")
	}
	escPresses := 0
return

;{=============================== Mouse ===============================================
;~ ; Hold right mouse botton, click left to switch to previous window

; #if (blm = 1) 
; LButton::
; return
; #if

; #if getKeyState("RButton","P")
; ; LButton::                                   ;- Switch to next window
; ; 	if (!GetKeyState("ScrollLock", "T"))
; ; 		CtrlClickSub()              ; ScrollLock On
; ; 	else if (GetKeyState("ScrollLock", "T"))
; ; 		SendToWindowBackSub()               ; ScrollLock Off
; ; return

; ; MButton Up::
; ; if (MButton_presses > 0){ ; SetTimer already started, so we log the keypress instead.
; ; 	MButton_presses += 1
; ; 	return
; ; }
; ; MButton_presses := 1
; ; SetTimer, MButtonOptions, -200 ; Wait for more presses within a 400 millisecond window.
; ; return
; #if

;~ #if getKeyState("LButton","P")
;~ Rbutton::
;~ return
;~ #if

; Create the Hotkeys with the function and select labels and wait times.
; RButton::
; MouseExtras("RHoldMsg", "500", "RDoubleMsg", "0.1")
; return

; Ctrl + Right Click to Open Tray Menu
^RButton::Menu, Tray, Show
return

; ; Mouse Side Button for Ditto Clipboard - Check End of Document for Extras
; XButton2::
; if (XButton2_presses > 0){ ; SetTimer already started, so we log the keypress instead.
; 	XButton2_presses += 1
; 	return
; }
; XButton2_presses := 1
; SetTimer, XButton2Options, -200 ; Wait for more presses within a 400 millisecond window.
; return

; XButton1::
; if (XButton1_presses > 0){ ; SetTimer already started, so we log the keypress instead.
; 	XButton1_presses += 1
; 	return
; }
; XButton1_presses := 1
; SetTimer, XButton1Options, -200 ; Wait for more presses within a 400 millisecond window.
; return

; Mouse WheelLeft for Copy
WheelLeft::
	if (wheelLeftPresses > 0) {
	wheelLeftPresses += 1
	return
	}
	wheelLeftPresses := 1
	SetTimer, WheelLeftKey, -300 ; Wait for more presses within a 200 millisecond window.
return

; Mouse Wheel Right for Paste
WheelRight::
	if (wheelRightPresses > 0){
		wheelRightPresses += 1
		return
	}
	wheelRightPresses := 1
	SetTimer, WheelRightKey, -300 ; Wait for more presses within a 300 millisecond window.
return

;~ ;}

;{=============================== Labels =========================================
WheelLeftKey:
	if (wheelLeftPresses = 1){
		if !getKeyState("RButton","P")
			Send, ^c
		else
			Send, ^x
	} else if (wheelLeftPresses = 2) {

	}
	wheelLeftPresses := 0
return

WheelRightKey:
	if (wheelRightPresses = 1){
		if !getKeyState("RButton","P")
			Send ^v ; Clip(Clipboard)
		else
			Send, {Space}
	} else if (wheelRightPresses = 2) {

	}
	wheelRightPresses := 0
return

MButtonOptions:
	if (MButton_presses = 1) ; The key was pressed once.
		caseMenu.show()
	else if (MButton_presses = 2) ; The key was pressed twice.
		Send, {Enter}
	MButton_presses := 0
return

RHoldMsg:
	#MaxThreadsPerHotkey, 2
	Toggle = 0
	KeyWait, Rbutton, U
	if  (A_PriorKey = "RButton")
		;~ caseMenu.show()
		return
return

RightDoubleLeft:
	#if getKeyState("RButton","P")
		Toggle = !Toggle
	if (Toggle) AND if (A_Priorkey = A_PriorHotkey)
		Click, Down
	else
		Click, Up
return
#if

RDoubleMsg:
	if (WinActive("ahk_exe EXPLORER.exe"))
		Send, {LButton}{Enter}
	else if (GetKeyState("ScrollLock", "T"))
		Send, {LButton}{LButton}{LButton}
return

XButton2Options:
	if (XButton2_presses = 1){          ; The key was pressed once.
		if WinActive("ahk_class CabinetWClass ahk_exe Explorer.EXE")
			Send, !{Right}
	} else if (XButton2_presses = 2) {  ; The key was pressed twice.
		if WinActive("ahk_class CabinetWClass ahk_exe Explorer.EXE")
			return
		else
			Send, {XButton2}
	}
	XButton2_presses := 0
return

XButton1Options:
	if (XButton1_presses = 1){          ; The key was pressed once.
		if WinActive("ahk_exe chrome.exe")
			Send, {XButton1}
		if WinActive("ahk_class CabinetWClass ahk_exe Explorer.EXE") {
			Send, !{left}		
	} else if (XButton1_presses = 2) {  ; The key was pressed twice.
		Send, {XButton1}

	}}
	XButton1_presses := 0
return
;~ ;}

;~ Numpad0 & Numpad1::MsgBox You pressed Numpad1 while holding down Numpad0.
;~ Numpad0 & Numpad2::Run Notepad
NumpadEnter::Send {NumpadEnter}

NumpadEnter & NumpadAdd::
	casemenu.Show()
return

*CapsLock::
keywait, Capslock, T0.15
if (ErrorLevel){
	^CapsLock::
	caseMenu.show()
	return
	}
Send, {CapsLock Up}
return

;~ NumLock::
return

~>+CapsLock::
NumLock::
ScrollLock::
~Insert::
	Toast.show( {title:{text:strRemove(A_ThisHotkey,["~","+",">"]) (GetKeyState(strRemove(A_ThisHotkey,["~","+",">"]),"T")? " On":" Off")}, sound:False})
return

;~ Toast 

; d:={ title:{color:"0x0000FF"} , margin:[100,100]}
; t:=new toast(d)
; p:={ title:{text:"hi",color:"0x0000FF"} , margin:[100,50] ,message:{ text:["hello","2","3"], def_size:18, size:[9,10], color:["0xFF00FF","0x00FF00"], offset:["",1] },life:0 }
; t.show(p)
; sleep, 1000
; t.show({ title:{text:"whatever"},message:{text:["hello"]} }) ;Replaces previous instance of same obj
; sleep, 200
; Toast.show("hi") ;Show with all default settings
; Toast.show(p) ;Show without creating a dedicated object. Will replace other instances without object, but not other object
;~ Listlines, Off
class Toast{
	__new(byRef p:=""){
		;~ ;~ Listlines, Off
		static toastCount:=0
		toastCount++
		this.id:=toastCount, this.closeObj:=ObjBindMethod(this,"close")
		,this.def:={ life:500, pos:{x:"center",y:"center"}, bgColor:"0x222222", trans:180, margin:{x:5,y:5}
					, closekeys:[["~Space","return","~LButton","Esc"]], sound:false, activate:False
					, title:{ text:"", color: "0xFFFFFF", size:16, opt:"bold", font:"Segoe UI" }
					, message:{ text:[], color: [], size:[], opt:[], name:[], offset:[20]
							  , def_color: "0xFFFFFF", def_size:12, def_opt:"", def_name:"Segoe UI", def_offset:20 } }

		if p
			for i,x in p {
				if IsObject(x)
					for j,y in x
						this.def[i][j]:= p[i][j]
				else this.def[i]:=p[i]
			}
	}
	setParam(byRef p,def:=false){
		if !IsObject(p) ;If not object, assume only title is given
			p:={title:{text:p}}
		for i,x in this.def {
			if IsObject(x) {
				this[i]:={}
				for j,y in x
					this[i][j]:= (p[i][j]="")? this.def[i][j] : p[i][j]
			} else this[i]:= (p[i]="")? this.def[i] : p[i]
		}
		this.x:=this.pos.x, this.y:=this.pos.y, this.pos:="", this.closekeys:=this.closekeys[1]
		return
	}
	show(byRef param){
		;~ ;~ Listlines, Off
		if A_IsPaused
			return
		if !this.def
			this.__new()
		this.setParam(param)

		GUI_handle:="Toast_GUI" this.id
		Gui, %GUI_handle%: New, -Caption +ToolWindow +AlwaysOnTop +hwndHWND
		this.hwnd:=hwnd
		Gui, %GUI_handle%: Color, % this.bgColor
		GUI, %GUI_handle%:+LastFoundExist
		WinSet, Trans, % this.trans
		Gui, %GUI_handle%:Margin, % this.marginX, % this.marginY

		t:=this.title.text, s:=this.title.size, c:=this.title.color, o:=this.title.opt, f:=this.title.Font
		Gui, %GUI_handle%: Font, norm s%s% c%c% %o%, %f%
		Gui, %GUI_handle%: Add, Text,, %t%

		for i,t in this.message.text {
			 s:= this.message.size  [i] = "" ? this.message.def_size    : this.message.size  [i]
			,c:= this.message.Color [i] = "" ? this.message.def_color   : this.message.color [i]
			,o:= this.message.opt   [i] = "" ? this.message.def_opt     : this.message.opt   [i]
			,f:= this.message.Font  [i] = "" ? this.message.def_font    : this.message.Font  [i]
			,m:= this.message.offset[i] = "" ? this.message.def_offset  : this.message.offset[i]
			Gui, %GUI_handle%: Font, norm s%s% c%c% %o%, %f%
			Gui, %GUI_handle%: Add, Text, xp y+%m%, %t%
		}
		OnMessage(0x202, closeObj:=this.closeObj)
		this.exist:=True
		if this.sound
			SoundPlay, *-1
		GUI, %GUI_handle%: Show, % (this.activate?"":"NoActivate ") "autosize x" this.x " y" this.y, % "Toast" this.id
		if this.life
			setTimer, % closeObj , % "-" this.life
		for _,k in this.closekeys
			Hotkey, % k , % closeObj, On B0 T1
		ListLines, On
		return
	}
	close(wparam:="",lParam:="",msg:="",hwnd:=""){
		;~ ;~ Listlines, Off
		if (hwnd and hwnd!=this.hwnd)
			return

		this.exist:=False, GUI_handle:="Toast_GUI" this.id
		for _,k in this.closekeys
			Hotkey % k, Off
		GUI, %GUI_handle%: Destroy
		return
	}
}

ToastTipInfos(){
	MouseGetPos, xpos, ypos
	halfscreenheight := Round((A_ScreenHeight/2))
	halfscreenwidth := Round((A_ScreenWidth/2))
	MouseGetPos, mx, my
	CoordMode, Mouse, Relative
	ToolTipInfosVar:= % "Width: " halfscreenwidth " | Height: " halfscreenheight "`n" "Mouse X=" xpos "+" A_ScreenWidth-xpos "=" A_ScreenWidth " | Y="  ypos "+" A_ScreenHeight-ypos "=" A_ScreenHeight "`n" "Caret: X="  A_CaretX " | Y=" A_CaretY "`n" "PriorKey: " A_Priorkey " | PriorHotKey: " A_PriorHotkey " | LastError: " A_LastError " | Time Idle: " A_TimeIdle " | " A_TimeSinceThisHotkey " | " A_TimeIdle " | " A_TimeSincePriorHotkey " | "
	WinGetPos, FoundX, FoundY, WinRight, WinBottom, A
		MouseX := (FoundX + WinRight - mx)
		MouseY := (FoundY + WinBottom - my)
		 p := "           "
	ToolTipInfosVar2 := % "`nIdle: " SubStr(p A_TimeIdle,-10) "`nError: " SubStr(p A_LastError,-9) "`n`nFoundX:" SubStr(p FoundX,-4) "`nFoundY: " SubStr(p FoundY,-3) "`n`nmx: " SubStr(p mx,-10) "`nmy: " SubStr(p my,-10) "`n`nRight: " SubStr(p MouseX,-7) "`nBottom: " SubStr(p MouseY,-3) "`n`nCaret X: " SubStr(p A_CaretX,-4) "`nCaret Y: " SubStr(p A_CaretY,-4)
	Toast.show({pos:{x:1800,y:800},trans:220,margin:{x:1,y:1},sound:false,title:{text:ToolTipInfosVar2,size:9,opt:"norm",font:"Segoe UI"}})
	;----------------------------------------
	; ToolTipInfosVar2 := % "FoundX: " FoundX ", FoundY: " FoundY " | mx: " mx ", my: " my " | From Right: " MouseX ", From Bottom: " MouseY " | Caret X:" A_CaretX ", Caret Y:" A_CaretY
	;----------------------------------------
	; ,this.def:={ life:500, pos:{x:"center",y:"center"}, bgColor:"0x222222", trans:180, margin:{x:5,y:5}
	;                   , closekeys:[["~Space","return","~LButton","Esc"]], sound:false, activate:False
	;                   , title:{ text:"", color: "0xFFFFFF", size:16, opt:"bold", font:"Segoe UI" }
	;                   , message:{ text:[], color: [], size:[], opt:[], name:[], offset:[20]
	;                             , def_color: "0xFFFFFF", def_size:12, def_opt:"", def_name:"Segoe UI", def_offset:20 } }
	; Toast.show({pos:{x:1300,y:1010},trans:200,margin:{x:1,y:1},sound:false,title:{text:ToolTipInfosVar2,color: "0xFFFFFF",size:9,opt:"norm",font:"Segoe UI"},message:{text:[],size:[8],offset:[0],def_color:"0xFFFFFF",def_offset:0}})
	;----------------------------------------
	; Toast.show({pos:{x:4,y:1011},trans:220,margin:{x:1,y:1},sound:false,title:{text:ToolTipInfosVar2,size:9,opt:"norm",font:"Segoe UI"}})
	;----------------------------------------
}

;~ == Delayed Timer 

class delayedTimer {
    set(f0,t0,runatStart:=False){
        if !isObject(this.obj)
            this.obj:=[]
        return this.obj.push({f:f0,t:t0,r:runatStart})
    }
    start(r:=False){
        
        for _,item in this.obj {
            f:=item.f
            setTimer, % f, % item.t
        }
        return r? this.firstRun() :0
    }
    firstRun(){
        for _,item in this.obj
            if item.r
                %f%()
        return this.reset()
    }
    reset(){
        return this.obj:=[]
    }
}

;~ Mouse Extras

isOver_mouse(WinTitle:="A"){ ;If ahk_id is passed, d:ont use ahk_id prefix or any other options
    ; ;~ ;~ Listlines, Off
    MouseGetPos, , , Win
    if WinTitle is number
        return (win==WinTitle)
    else return WinExist(WinTitle " ahk_id " Win)
    ; ListLines, On
}

isOver_coord(win,pos){
    CoordMode, Mouse, Screen        ;Mouse co-ords are specified in global co-ords
    WinGetPos, x, y, w, h, % win
    msgbox, %x%, %y%, %w%, %h%
    msgbox, % mpos[1] "|" mpos[2]
    if ((pos[1]>=x) and (pos[1]<=x+w) and (pos[2]>=y) and (pos[2]<=y+h))
        return True
    return false
}

isOver_mouse2(WinTitle:="A"){ ;If ahk_id is passed, dont use ahk_id prefix or any other options
    MouseGetPos, , , WiWin
    return Win
}

isOver_titlebar(A){
    MouseGetPos, , , A,
    if WinTitle is number
        (win==WinTitle)
    CoordMode, Mouse, Screen
    WinGetPos, x, y, w, h, % win
    MouseGetPos, posx, posy, Win
    tbx := (x+w), tby := (y+30)
    if ((posx>=x) and (posx<=x+w) and (posy>=y) and (posy<=y+30))
        return true
    return false
}

isOver_anytitlebar(){
    ;~ PastePos = %FoundT%, %FoundW%, %FoundH%, %FoundX%, %FoundY%
    ;~ MousePos := posx ">=" FoundX " | " posx "<=" (FoundW+FoundX) " | " posy ">=" FoundY " | " posy "<=" (FoundY+30) " | "  postitle
    ;~ Toast.show({trans:220,life:5000,margin:{x:1,y:1},sound:false,title:{text:PastePos "`n" FoundW ","  FoundH ", " FoundX  ", " FoundY ", " posx ", " posy "`n" MousePos ,size:9,opt:"norm",font:"Segoe UI"}})

    MouseGetPos, posx, posy, postitle
    WinActivate, ahk_id %postitle%,
    WinGetActiveStats, FoundT, FoundW, FoundH, FoundX, FoundY
    if ((posx>=FoundX) and (posx<=FoundX+FoundW) and (posy>=FoundY) and (posy<=FoundY+30))
        return true
    return false
}

;~ Listlines, Off
isOver_taskbar(){
    CoordMode, Mouse, Screen
    MouseGetPos, posx, posys
    if (posx>=1040)
        return true
    return false
}

Mouse_RelativeMove(x, y) {
    DllCall("mouse_event", "UInt", 0x01, "UInt", x, "UInt", y)
}

;~ ==== Reload as Admin

reloadAsAdmin(force:=True){
    if A_IsAdmin
        return 0
    Run % "*RunAs " ( A_IsCompiled ? "" : """"  A_AhkPath """" )  " """ A_ScriptFullpath """", %A_ScriptDir%, UseErrorLevel
    return _reloadAsAdmin_Error(e,force)
}

;http://ahkscript.org/boards/viewtopic.php?t=4334
reloadAsAdmin_Task(force:=True) { ;  By SKAN,  http://goo.gl/yG6A1F,  CD:19/Aug/2014 | MD:22/Aug/2014
; Asks for UAC only first time

  TASK_CREATE := 0x2,  TASK_LOGON_INTERACTIVE_TOKEN := 3
  e:=""
  Try TaskSchd := ComObjCreate( "Schedule.Service" ),    TaskSchd.Connect()
  Catch e
      return _reloadAsAdmin_Error(e,force)

  CmdLine       := ( A_IsCompiled ? "" : """"  A_AhkPath """" )  A_Space  ( """" A_ScriptFullpath """"  )
  TaskName      := A_ScriptName " @" SubStr( "000000000"  DllCall( "NTDLL\RtlComputeCrc32"
                   , "Int",0, "WStr",CmdLine, "UInt",StrLen( CmdLine ) * 2, "UInt" ), -9 )

  Try {
    Try TaskRoot := TaskSchd.GetFolder("\AHK-ReloadAsAdmin")
    catch
        TaskRoot := TaskSchd.GetFolder("\"), TaskName:="[AHK-ReloadAsAdmin]" TaskName
    RunAsTask := TaskRoot.GetTask( TaskName )
  }
  TaskExists    := !A_LastError

  if !A_IsAdmin {
    if TaskExists {
        RunAsTask.Run("")
        ExitApp
    } else reloadAsAdmin(force)
  }

  else if !TaskExists {
    XML := "
    (LTrim Join
      <?xml version=""1.0"" ?><Task xmlns=""http://schemas.microsoft.com/windows/2004/02/mit/task""><Regi
      strationInfo /><Triggers><LogonTrigger><Enabled>true</Enabled><Delay>PT30S</Delay></LogonTrigger>
      </Triggers><Principals><Principal id=""Author""><LogonType>InteractiveToken</LogonT
      ype><RunLevel>HighestAvailable</RunLevel></Principal></Principals><Settings><MultipleInstancesPolic
      y>Parallel</MultipleInstancesPolicy><DisallowStartIfOnBatteries>false</DisallowStartIfOnBatteries><
      StopIfGoingOnBatteries>false</StopIfGoingOnBatteries><AllowHardTerminate>false</AllowHardTerminate>
      <StartWhenAvailable>false</StartWhenAvailable><RunOnlyIfNetworkAvailable>false</RunOnlyIfNetworkAva
      ilable><IdleSettings><StopOnIdleEnd>true</StopOnIdleEnd><RestartOnIdle>false</RestartOnIdle></IdleS
      ettings><AllowStartOnDemand>true</AllowStartOnDemand><Enabled>true</Enabled><Hidden>false</Hidden><
      RunOnlyIfIdle>false</RunOnlyIfIdle><DisallowStartOnRemoteAppSession>false</DisallowStartOnRemoteApp
      Session><UseUnifiedSchedulingEngine>false</UseUnifiedSchedulingEngine><WakeToRun>false</WakeToRun><
      ExecutionTimeLimit>PT0S</ExecutionTimeLimit></Settings><Actions Context=""Author""><Exec>
      <Command>"   (  A_IsCompiled ? A_ScriptFullpath : A_AhkPath )       "</Command>
      <Arguments>" ( !A_IsCompiled ? """" A_ScriptFullpath  """" : "" )   "</Arguments>
      <WorkingDirectory>" A_ScriptDir "</WorkingDirectory></Exec></Actions></Task>
    )"

    TaskRoot.RegisterTask( TaskName, XML, TASK_CREATE, "", "", TASK_LOGON_INTERACTIVE_TOKEN )

  }

  return TaskName
}

_reloadAsAdmin_Error(e,force){
    if force {
        MsgBox, 4112, FATAL ERROR!!, Couldn't restart script!!`nError Code: %e%
        ExitApp
    }
    return 1
}

*/

;~=== URI Encode

URI_Encode(URI, RE="[0-9A-Za-z]"){
    VarSetCapacity(Var, StrPut(URI, "UTF-8"), 0), StrPut(URI, &Var, "UTF-8")
    While Code := NumGet(Var, A_Index - 1, "UChar")
        Res .= (Chr:=Chr(Code)) ~= RE ? Chr : Format("%{:02X}", Code)
    Return, Res
}

URI_Decode(URI){
    Pos := 1
    While Pos := RegExMatch(URI, "i)(%[\da-f]{2})+", Code, Pos)
    {
        VarSetCapacity(Var, StrLen(Code) // 3, 0), Code := SubStr(Code,2)
        Loop, Parse, Code, `%
            NumPut("0x" A_LoopField, Var, A_Index-1, "UChar")
        Decoded := StrGet(&Var, "UTF-8")
        URI := SubStr(URI, 1, Pos-1) . Decoded . SubStr(URI, Pos+StrLen(Code)+1)
        Pos += StrLen(Decoded)+1
    }
    return, URI
}

;----------------------------------

URI_URLEncode(URL){ ; keep ":/;?@,&=+$#."
    return URI_Encode(URL, "[0-9a-zA-Z:/;?@,&=+$#.]")
}

URI_URLDecode(URL){
    return URI_Decode(URL)
}

;Msgbox % URI_Encode("hi hello")
;Msgbox % URI_Decode("hi%20hello")

;~ ^j::
;~ ;Msgbox % URI_Encode("hi hello")
;~ Msgbox % URI_Encode("On British TV's ""Top of the Pops" "this Booker T. & the MGs hit might be titled ""Spring Onions")
;~ return

;~ JEE

;functions for Desktop and Explorer folder windows:
;JEE_ExpWinGetFoc ;get focused file
;JEE_ExpWinGetSel ;get selected files (list)
;JEE_ExpWinSetFoc ;set focused file
;JEE_ExpWinSetSel ;set selected files
;JEE_ExpWinGetFileCount ;get file count (selected, total)
;JEE_ExpOpenContainingFolder ;select file(s) in folder (open new window)
;JEE_ExpOpenContainingFolder2 ;select file in folder (open new window)
;JEE_ExpOpenContainingFolderEx ;select file in folder (use existing window, else, open new window)
;JEE_ExpListOpenDirs ;get folders for open Explorer windows (list)
;JEE_ExpWinInvSel ;set selected files: invert selection
;JEE_ExpWinSelAll ;set selected files: select all
;JEE_ExpWinSelNone2 ;set selected files: select none
;JEE_ExpWinSelNone ;set selected files: select none
;JEE_ExpWinGetDir ;get Explorer window folder
;JEE_ExpWinSetDir ;set Explorer window folder (navigate) (or open file/url in new window)
;JEE_ExpWinNewFile ;create new file/folder (create file, focus file, start edit mode)
;JEE_ExpWinSelFirst ;set selected files: jump to first file/folder
;JEE_ExpWinGetText ;get text from visible rows (requires Acc library)

;==================================================

JEE_ExpWinGetFoc(hWnd:=0)
{
    local oItem, oWin, oWindows, vPath, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return

    vPath := oWin.Document.FocusedItem.path
    oWindows := oWin := oItem := ""
    return vPath
}

;==================================================

JEE_ExpWinGetSel(hWnd:=0, vSep:="`n")
{
    local oItem, oWin, oWindows, vCount, vOutput, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return

    vCount := oWin.Document.SelectedItems.Count
    vOutput := ""
    VarSetCapacity(vOutput, (260+StrLen(vSep))*vCount*2)
    for oItem in oWin.Document.SelectedItems
        if !(SubStr(oItem.path, 1, 3) = "::{")
            vOutput .= oItem.path vSep
    oWindows := oWin := oItem := ""
    return SubStr(vOutput, 1, -StrLen(vSep))
}

;==================================================

;vByPos allows you to use vName to specify a file by position (1-based index)
JEE_ExpWinSetFoc(hWnd, vName, vByPos:=0, vFlags:=0x1D)
{
    local oItems, oWin, oWindows, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return

    oItems := oWin.Document.Folder.Items
    ;SVSI_FOCUSED = 0x10 ;SVSI_ENSUREVISIBLE := 0x8
    ;SVSI_DESELECTOTHERS := 0x4 ;SVSI_EDIT := 0x3
    ;SVSI_SELECT := 0x1 ;SVSI_DESELECT := 0x0
    if !vByPos
        oWin.Document.SelectItem(oItems.Item(vName), vFlags)
    else
        oWin.Document.SelectItem(oItems.Item(vName-1), vFlags)
    oWindows := oWin := oItems := ""
}

;==================================================

;the first file in the list can be given different flags to the other files
JEE_ExpWinSetSel(hWnd, vList, vSep:="`n", vFlagsF:=0x1D, vFlagsS:=0x1)
{
    local oItems, oWin, oWindows, vFlags, vName, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return

    oItems := oWin.Document.Folder.Items
    Loop, Parse, vList, % vSep
    {
        ;SVSI_FOCUSED = 0x10 ;SVSI_ENSUREVISIBLE := 0x8
        ;SVSI_DESELECTOTHERS := 0x4 ;SVSI_EDIT := 0x3
        ;SVSI_SELECT := 0x1 ;SVSI_DESELECT := 0x0
        vFlags := (A_Index = 1) ? vFlagsF : vFlagsS
        if !InStr(A_LoopField, "\")
            oWin.Document.SelectItem(oItems.Item(A_LoopField), vFlags)
        else
        {
            SplitPath, A_LoopField, vName
            oWin.Document.SelectItem(oItems.Item(vName), vFlags)
        }
    }
    oWindows := oWin := oItems := ""
}

;==================================================

JEE_ExpWinGetFileCount(hWnd, ByRef vCountS, ByRef vCountT)
{
    local oWin, oWindows, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return

    vCountS := oWin.Document.SelectedItems.count
    vCountT := oWin.Document.Folder.Items.count
    oWindows := oWin := ""
}

;==================================================

JEE_ExpOpenContainingFolder(vList, vSep:="`n")
{
    local ArrayITEMIDLIST, DirITEMIDLIST, PathITEMIDLIST, vComSpec, vCount, vDir, vList2, vPath
    vPath := SubStr(vList, 1, InStr(vList vSep, vSep)-1)
    SplitPath, vPath,, vDir

    ;this works but SHOpenFolderAndSelectItems is faster/smoother
    ;if !InStr(vList, vSep)
    ;{
    ;   if FileExist(vPath)
    ;   {
    ;       vComSpec := A_ComSpec ? A_ComSpec : ComSpec
    ;       Run, % vComSpec " /c explorer.exe /select, " Chr(34) vPath Chr(34),, Hide
    ;   }
    ;   else if InStr(FileExist(vDir), "D")
    ;       Run, % vDir
    ;   return
    ;}

    vList2 := "", vCount := 0
    Loop, Parse, vList, % vSep
        if FileExist(A_LoopField)
            vList2 .= A_LoopField "`n", vCount += 1
    if !vCount
    {
        Run, % vDir
        return
    }
    VarSetCapacity(ArrayITEMIDLIST, vCount*A_PtrSize)
    Loop, Parse, vList, `n
    {
        DllCall("shell32\SHParseDisplayName", Str,A_LoopField, Ptr,0, PtrP,PathITEMIDLIST, UInt,0, Ptr,0)
        NumPut(PathITEMIDLIST, &ArrayITEMIDLIST, (A_Index-1)*A_PtrSize, "Ptr")
    }
    DllCall("ole32\CoInitializeEx", Ptr,0, UInt,0)
    DllCall("shell32\SHParseDisplayName", Str,vDir, Ptr,0, PtrP,DirITEMIDLIST, UInt,0, Ptr,0)
    DllCall("shell32\SHOpenFolderAndSelectItems", Ptr,DirITEMIDLIST, UInt,vCount, Ptr,&ArrayITEMIDLIST, UInt,0)
    DllCall("ole32\CoTaskMemFree", Ptr,DirITEMIDLIST)
    Loop, % vCount
        DllCall("ole32\CoTaskMemFree", Ptr,NumGet(&ArrayITEMIDLIST, (A_Index-1)*A_PtrSize, "Ptr"))
    DllCall("ole32\CoUninitialize")
}

;==================================================

JEE_ExpOpenContainingFolder2(vPath)
{
    local vComSpec
    if FileExist(vPath)
    {
        vComSpec := A_ComSpec ? A_ComSpec : ComSpec
        Run, % vComSpec " /c explorer.exe /select, " Chr(34) vPath Chr(34),, Hide
    }
    else if InStr(FileExist(vDir), "D")
        Run, % vDir
}

;==================================================

JEE_ExpOpenContainingFolderEx(vPath)
{
    local oItems, oWin, vComSpec, vDir, vName
    SplitPath, vPath, vName, vDir
    if !FileExist(vPath)
    {
        if FileExist(vDir)
            Run, % vDir
        return
    }
    for oWin in ComObjCreate("Shell.Application").Windows
        if (oWin.Name = "Windows Explorer")
        && (vDir = oWin.Document.Folder.Self.Path)
        {
            WinActivate, % "ahk_id " oWin.HWND
            ;SVSI_FOCUSED = 0x10 ;SVSI_ENSUREVISIBLE := 0x8
            ;SVSI_DESELECTOTHERS := 0x4 ;SVSI_EDIT := 0x3
            ;SVSI_SELECT := 0x1 ;SVSI_DESELECT := 0x0
            oItems := oWin.Document.Folder.Items
            oWin.Document.SelectItem(oItems.Item(vName), 0x1D)
            oWin := oItems := ""
            return
        }
    oWin := ""
    vComSpec := A_ComSpec ? A_ComSpec : ComSpec
    Run, % vComSpec " /c explorer.exe /select, " Chr(34) vPath Chr(34),, Hide
}

;==================================================

JEE_ExpListOpenDirs(vSep:="`n")
{
    local oWin, vOutput
    for oWin in ComObjCreate("Shell.Application").Windows
        if (oWin.Name = "Windows Explorer")
            vOutput .= oWin.Document.Folder.Self.Path vSep
    oWin := ""
    return SubStr(vOutput, 1, -StrLen(vSep))
}

;==================================================

JEE_ExpWinInvSel(hWnd:=0)
{
    (!hWnd) && hWnd := WinExist("A")
    SendMessage, 0x111, 28706, 0, SHELLDLL_DefView1, % "ahk_id " hWnd ;WM_COMMAND := 0x111 ;(invert selection)
}

;==================================================

JEE_ExpWinSelAll(hWnd:=0)
{
    (!hWnd) && hWnd := WinExist("A")
    SendMessage, 0x111, 28705, 0, SHELLDLL_DefView1, % "ahk_id " hWnd ;WM_COMMAND := 0x111 ;(select all)
}

;==================================================

JEE_ExpWinSelNone2(hWnd:=0)
{
    (!hWnd) && hWnd := WinExist("A")
    SendMessage, 0x111, 28705, 0, SHELLDLL_DefView1, % "ahk_id " hWnd ;select all
    SendMessage, 0x111, 28706, 0, SHELLDLL_DefView1, % "ahk_id " hWnd ;invert selection
}

;==================================================

JEE_ExpWinSelNone(hWnd:=0)
{
    local oItem, oItems, oWin, oWindows, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return

    ;SVSI_FOCUSED = 0x10 ;SVSI_ENSUREVISIBLE := 0x8
    ;SVSI_DESELECTOTHERS := 0x4 ;SVSI_EDIT := 0x3
    ;SVSI_SELECT := 0x1 ;SVSI_DESELECT := 0x0
    oItems := oWin.Document.Folder.Items
    oWin.Document.SelectItem(oItems.Item(0), 0x14)
    oWindows := oWin := oItems := ""
}

;==================================================

JEE_ExpWinGetDir(hWnd:=0)
{
    local oWin, oWindows, vDir, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return
    vDir := oWin.Document.Folder.Self.Path
    oWindows := oWin := ""
    return vDir
}

;==================================================

;sets dir (or opens file/url in new window)
JEE_ExpWinSetDir(hWnd, vDir)
{
    local oWin, vPIDL, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if !(vWinClass = "CabinetWClass") && !(vWinClass = "ExploreWClass")
        return
    for oWin in ComObjCreate("Shell.Application").Windows
        if (oWin.HWND = hWnd)
        {
            if !InStr(vDir, "#") || InStr(vDir, "://") ;folders that don't contain #, and urls
                oWin.Navigate(vDir)
            else ;folders that contain #
            {
                DllCall("shell32\SHParseDisplayName", WStr,vDir, Ptr,0, PtrP,vPIDL, UInt,0, Ptr,0)
                VarSetCapacity(SAFEARRAY, A_PtrSize=8?32:24, 0)
                NumPut(1, &SAFEARRAY, 0, "UShort") ;cDims
                NumPut(1, &SAFEARRAY, 4, "UInt") ;cbElements
                NumPut(vPIDL, &SAFEARRAY, A_PtrSize=8?16:12, "Ptr") ;pvData
                NumPut(DllCall("shell32\ILGetSize", Ptr,vPIDL, UInt), &SAFEARRAY, A_PtrSize=8?24:16, "Int") ;rgsabound[1]
                oWin.Navigate2(ComObject(0x2011, &SAFEARRAY), 0)
                DllCall("shell32\ILFree", Ptr,vPIDL)
            }
            break
        }
    oWin := ""
}

;==================================================

;e.g. JEE_ExpWinNewFile(hWnd, "New Text Document", "txt")
;e.g. JEE_ExpWinNewFile(hWnd, "New Folder", "", "d")
;e.g. JEE_ExpWinNewFile(hWnd, "New AutoHotkey Script", "ahk", "mc", "C:\Windows\ShellNew\Template.ahk")

;vOpt: d (dir), m/c (set modified/created dates to now, i.e. for copied template files)
JEE_ExpWinNewFile(hWnd, vNameNoExt, vExt, vOpt:="", vPathTemplate:="")
{
    local oItems, oWin, oWindows, vDir, vFlags, vName, vPath, vSfx, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return

    vDir := oWin.Document.Folder.Self.Path
    if !InStr(FileExist(vDir), "D")
    {
        oWindows := oWin := ""
        return
    }

    ;note: Explorer creates files e.g. 'File, File (2), File (3)', but with no 'File (1)'
    if !(vExt = "")
        vExt := "." vExt
    Loop
    {
        vSfx := (A_Index=1) ? "" : " (" A_Index ")"
        vName := vNameNoExt vSfx vExt
        vPath := vDir "\" vName
        if !FileExist(vPath)
            break
    }
    if InStr(vOpt, "d")
        FileCreateDir, % vPath
    else if (vPathTemplate = "")
        FileAppend,, % vPath
    else
        FileCopy, % vPathTemplate, % vPath

    if InStr(vOpt, "m")
        FileSetTime,, % vPath, M
    if InStr(vOpt, "c")
        FileSetTime,, % vPath, C

    ;SVSI_FOCUSED = 0x10 ;SVSI_ENSUREVISIBLE := 0x8
    ;SVSI_DESELECTOTHERS := 0x4 ;SVSI_EDIT := 0x3
    ;SVSI_SELECT := 0x1 ;SVSI_DESELECT := 0x0
    vFlags := WinActive("ahk_id " hWnd) ? 0x1F : 0x1D
    Loop, 30
    {
        oItems := oWin.Document.Folder.Items
        if !(oItems.Item(vName).path = "")
        {
            oWin.Document.SelectItem(oItems.Item(vName), vFlags)
            break
        }
        Sleep 100
    }
    oWindows := oWin := oItems := ""
}

;==================================================

;1=jump to first file, 0=jump to first dir, -1=toggle
;toggle jump to first file/folder
;note: slow on folders with lots of files/folders
JEE_ExpWinSelFirst(hWnd:=0, vOpt:=-1)
{
    local oItem, oItems, oWin, oWindows, vIsDir, vWinClass
    DetectHiddenWindows, On
    (!hWnd) && hWnd := WinExist("A")
    WinGetClass, vWinClass, % "ahk_id " hWnd
    if (vWinClass = "CabinetWClass") || (vWinClass = "ExploreWClass")
    {
        for oWin in ComObjCreate("Shell.Application").Windows
            if (oWin.HWND = hWnd)
                break
    }
    else if (vWinClass = "Progman") || (vWinClass = "WorkerW")
    {
        oWindows := ComObjCreate("Shell.Application").Windows
        VarSetCapacity(hWnd, 4, 0)
        ;SWC_DESKTOP := 0x8 ;VT_BYREF := 0x4000 ;VT_I4 := 0x3 ;SWFO_NEEDDISPATCH := 0x1
        oWin := oWindows.FindWindowSW(0, "", 8, ComObject(0x4003, &hWnd), 1)
    }
    else
        return

    if (vOpt = -1)
        vIsDir := -!oWin.Document.FocusedItem.IsFolder ;IsFolder returns -1=true, 0=false
    else if (vOpt = 1)
        vIsDir := 0
    else if (vOpt = 0)
        vIsDir := -1
    for oItem in oWin.Document.Folder.Items
        if (vIsDir = oItem.IsFolder)
        {
            ;SVSI_FOCUSED = 0x10 ;SVSI_ENSUREVISIBLE := 0x8
            ;SVSI_DESELECTOTHERS := 0x4 ;SVSI_EDIT := 0x3
            ;SVSI_SELECT := 0x1 ;SVSI_DESELECT := 0x0
            oWin.Document.SelectItem(oItem, 0x1D)
            break
        }
    oWindows := oWin := oItems := ""
}

;==================================================

;~=== get selected text
Clip(Text="", Reselect="")
{
    Static BackUpClip, Stored, LastClip
    If (A_ThisLabel = A_ThisFunc) {
        If (Clipboard == LastClip)
            Clipboard := BackUpClip
        BackUpClip := LastClip := Stored := ""
    } Else {
        If !Stored {
            Stored := True
            BackUpClip := ClipboardAll ; ClipboardAll must be on its own line
        } Else
            SetTimer, %A_ThisFunc%, Off
        LongCopy := A_TickCount, Clipboard := "", LongCopy -= A_TickCount ; LongCopy gauges the amount of time it takes to empty the clipboard which can predict how long the subsequent clipwait will need
        If (Text = "") {
            SendInput, ^c
            ClipWait, LongCopy ? 0.6 : 0.2, True
        } Else {
            Clipboard := LastClip := Text
            ClipWait, 10
            SendInput, ^v
        }
        SetTimer, %A_ThisFunc%, -700
        Sleep 20 ; Short sleep in case Clip() is followed by more keystrokes such as {Enter}
        If (Text = "")
            Return LastClip := Clipboard
        Else If ReSelect and ((ReSelect = True) or (StrLen(Text) < 3000))
            SendInput, % "{Shift Down}{Left " StrLen(StrReplace(Text, "`r")) "}{Shift Up}"
    }
    return
    Clip:
    return Clip()
}

getSelectedText()
{
 /* Returns selected text without disrupting the clipboard. However, if the clipboard contains a large amount of data, some of it may be lost
 */
    clipOld:=ClipboardAll
    Clipboard:=""
    Send, ^c
    ClipWait, 0.1, 1
    clipNew:=Clipboard
    Clipboard:=clipOld

    ;Special for explorer
    WinGet, w, ID, A
    WinGetClass, c, ahk_id %w%
    if c in Progman,WorkerW,Explorer,CabinetWClass
        SplitPath, clipNew,,,, clipNew2

    return clipNew2?clipNew2:clipNew
}


trayMenu(){
	;~ Listlines, Off
	;~ if it exists, import custom tray icon
	ifexist %A_ScriptDir%\resources\%SCR_Name%_Rounded.ico
		Menu, Tray, Icon, %A_ScriptDir%\resources\%SCR_Name%_Rounded.ico
	Menu, Tray, NoStandard
	Menu, Tray, Add, &Reload This Script, SCR_Reload
	Menu, Tray, Add, Open &Folder, SCR_OpenFolder
	Menu, Tray, Add
	Menu, Tray, NoMainWindow
	Menu, Tray, Standard
	; trayListen()
	; setTimer, trayListen, 1000  ;for better Stability
}

;~ === Tray

;~ use_TrayIcon("C:\Users\TG-PC\Documents\AutoHotkey.ahk", "Reload")
;-------------------------------------------------------------------------------
use_TrayIcon(Script, Action) { ; use tray icon actions of a running AHK script
	static a := { Open: 65300, Help:    65301, Spy:   65302, Reload: 65303
				, Edit: 65304, Suspend: 65305, Pause: 65306, Exit:   65307 }

	DetectHiddenWindows, On
	PostMessage, 0x111, % a[Action],,, %Script% - AutoHotkey
}

SCR_caseMenu(){
	caseMenu.show()
}
SCR_OpenFolder(){
	Run, %A_ScriptDir%\AutoHotkey\
}
SCR_Reload(){                 ; Use Control+S to save and reload script
	Progress, m2 b X1550 Y975 zh10,, Reloading %SCR_Name%.ahk
	Progress, 100
	Sleep, 1500
	Reload
}
trayListen(){
	;~ Listlines, Off
	OnMessage(0x404, "mouseOverTray")  ;Mouse over tray icon
	return
}
mouseOverTray(wParam,lParam){
	;~ Listlines, Off
	if (lParam=0x205) {        ; Single click
	} else if (lParam=0x205) {    ; Double click
	} else if (lParam=0x205) {    ; Right click
	} else updateTray()
}
updateTray(){
	;~ Listlines, Off
	MouseGetPos, mx, my

	tip.=SCR_Name " Script`n"

	obj:=Togglekeys_check()
	tip.="ToggleKeys: " (obj.n?"N":"") (obj.c?"C":"") (obj.s?"S":"") (obj.i?"I":"") "`n"

	obj:=func("showTrayTip").bind(tip,mx,my)
	setTimer, % obj, -200
	return
}
showTrayTip(tip,mx,my){
	;~ Listlines, Off
	MouseGetPos, mx, my
}

;~ == Toggle Keys

;~ Listlines, Off
class caseMenu{
	__new(){
		;~ Listlines, Off

		for _, j in [["searchVXDealer","Search VXDealer"],["searchHubspot"s,"Search Hubspot"],["dealerstats","Search Dealer Stats"],["truePeopleSearchFunc","Search &True People"],["phoneFormat","###-###-####"],["phoneFormatPara","(###) ###-####"]]

		{
			act:=ObjBindMethod(this,"textFormat",j[1])
			Menu, caseMenu, Add, % j[2], % act
		}

		;Seperator One
		Menu, caseMenu, Add

		for _, j in [["cutFunc","C&ut"],["copyFunc","&Copy"],["pasteFunc","&Paste"],["deleteFunc","&Delete"]]

		{
			act:=ObjBindMethod(this,"textFormat",j[1])
			Menu, caseMenu, Add, % j[2], % act
		}

		;Seperator One
		Menu, caseMenu, Add

		for _, j in [["fileRename","File &Rename"],["removeformat","Remove Format"],["cleanRename","Clean Rename"],["urldecode","URL Decode"],["urlencode","URL Encode"],["smsSplit","SMS Split"]]

		{
		act:=ObjBindMethod(this,"textFormat",j[1])
		Menu, Submenu1, Add, % j[2], % act
		Menu, caseMenu, Add, &Format, :Submenu1
		}

		for _, j in [["U","&UPPER CASE"],["L","&lower case"],["T","&Title Case"],["S","&Sentence case."]]

		{
			act:=ObjBindMethod(this,"caseChange",j[1])
			Menu, Submenu2, Add, % j[2], % act
			Menu, caseMenu, Add, &Change Case, :Submenu2

		}

		for _, j in [["TGL_ExpWinGetSel","Get File(s) Full Path(s)"],["TGL_ExpWinGetDir","Get File Path"]]

		{
			act:=ObjBindMethod(this,"textFormat",j[1])
			Menu, Submenu6, Add, % j[2], % act
			Menu, caseMenu, Add, &Capture Fuction, :Submenu6

		}

		for _, j in []

		{
			act:=ObjBindMethod(this,"textFormat",j[1])
			Menu, caseMenu, Add, % j[2], % act
		}

		Menu, Submenu1, Add

		for _, j in [["addQuotes","Add Quotes"],["prefaceTime","TimeStamp"]]

	{
		act:=ObjBindMethod(this,"textFormat",j[1])
		Menu, Submenu1, Add, % j[2], % act
	}

		Menu, caseMenu, Add

		for _, i in ["&Capslock","&Numlock","Sc&rollLock","I&nsert"] {
			act:=ObjBindMethod(this,"toggle",strReplace(i,"&"))
			Menu, caseMenu, Add, % i, % act
		}

		return
	}
	show(){
		for _, i in ["&Numlock","Sc&rollLock","I&nsert"]
			;~ Toast.show("caseMenu")
		;~ sleep, 500*
		for _, i in ["&Numlock","Sc&rollLock","I&nsert"]
			Menu, caseMenu, % GetKeyState(strReplace(i,"&"),"T")?"Check":"Uncheck", % i
		Menu, caseMenu, Show, % A_CaretX, % (A_Carety+25)
		return
	}

caseChange(type){ ; type: U=UPPER, L=Lower, T=Title, S=Sentence, I=Invert
		text:=caseChange(getSelectedText(), type)
		oldClip:=ClipboardAll
		clipboard:=text
		Send ^v
		sleep 100
		Clipboard:=oldClip
		return
	}

	toggle(key){
		if (key=Insert)
			Send, {Insert}
		else if (key=Capslock)
			SetCapsLockState, % GetKeyState("CapsLock","T")?"Off":"On"
		else if (key=Numlock)
			SetNumLockState, % GetKeyState("NumLock","T")?"Off":"On"
		else if (key=Scrolllock)
			SetScrollLockState, % GetKeyState("ScrollLock","T")?"Off":"On"
		return
	}

	textFormat(type){
			%type%()
		return
	}

}
Togglekeys_check(){
	return {c:getkeyState("Capslock","T"), n:getkeyState("Numlock","T"), s:getkeyState("ScrollLock","T"), i:getkeyState("Insert","T")}
}

strRemove(parent,strlist) {
	for _,str in strlist
		parent:=strReplace(parent,str)
	return parent
}

caseChange(text,type){ ; type: U=UPPER, L=Lower, T=Title, S=Sentence, I=Invert
	static X:= ["iOS","iPhone","iPad","I","AHK","AutoHotkey","Dr","Mr","Ms","Mrs","BK","DEA","AKJ","via","v","vs","GPG","PGP","to","of","and","on","with","USB","VIN" ]
	;~ list of words that should not be modified for S,T
	if (type="S") { ;Sentence case.
		text := RegExReplace(RegExReplace(text, "(.*)", "$L{1}"), "(?<=[^a-zA-Z0-9_-]\s|\n).|^.", "$U{0}")
	} else if (type="I") ;iNVERSE
		text:=RegExReplace(text, "([A-Z])|([a-z])", "$L1$U2")
	else text:=RegExReplace(text, "(.*)", "$" type "{1}")

	if (type="S" OR type="T")
		for _, word in X ;Parse the exceptions
			text:= RegExReplace(text,"i)\b" word "\b", word)
	return text
}

urldecode(){
	oldClip := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		oldClip := oldClip2
	oldClip := URI_URLDecode(oldClip)
	Clipboard := oldClip
	Clip(oldClip)
}

TGL_ExpWinGetSel(){
	WinGet, hwndtmp, ID, A
	WinActivate, %hwndtmp%,
	Clipboard := JEE_ExpWinGetSel(hwndtmp)
	Toast.show({title:{size:12,text:(Clipboard)},life:1000})
}

TGL_ExpWinGetDir(){
	Clipboard := JEE_ExpWinGetDir()
	Toast.show({title:{size:12,text:(Clipboard)},life:1000})
}

urlencode(){
	oldClip := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		oldClip := oldClip2
	oldClip := URI_URLEncode(oldClip)
	Clipboard := oldClip
	Clip(oldClip)
}

cutFunc(){
	Send, ^x
}
copyFunc(){
	fileExplorerName := getSelectedText()
	Clipboard := fileExplorerName
}
pasteFunc(){
	Clip(ClipBoard)
}

deleteFunc(){
	Send, {Del}
}

oldClipFunc(){
	oldClip := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		oldClip := oldClip2
	Clipboard := oldClip
}

addQuotes(){
	oldClip := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		oldClip := oldClip2
	oldClip = "%oldClip%"
	Clipboard := oldClip
	Clip(oldClip)
}

prefaceTime(){
	oldClip := Clip()
	FormatTime, TimeString, R, h:mm tt '-' M'/'dd'/'yyyy
	oldClip = %oldClip%
	oldClip := TimeString ": " oldClip
	Clip(oldClip)
}

removeformat(){
	oldClip := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		oldClip := oldClip2
	oldClip := Clip()
	oldClip := RegExReplace(ClipBoard , "\R\R\K\R+")
	Clipboard := oldClip
	Clip(oldClip)
}

searchHubspot(){
	searchTerm := searchTermClean()
	Run, https://app.hubspot.com/reports-dashboard/5712725/view/4177402?globalSearchQuery=%searchTerm%
}

searchVXDealer(){
	searchTerm := searchTermClean()
	Run, https://admin.vxdealer.com/Home/Portal#/dealerships?search=%searchTerm%
	Sleep, 1000
	WinGetPos, wX, wY, wWidth, wHeight, Admin
	PixelSearch, Px, Py, wX, (wY+100), (wX+wWidth), (wY+wHeight), 0xFF2020,50, Fast
	if ErrorLevel {
	    Toast.show({title:{size:10,text:"Not Found"},message:{text:[""],def_size:8,offset:48,def_offset:0},pos:{x:mx,y:my},sound:False,margin:{x:1,y:1},life:1000,trans:200})
	} else {
	    MouseMove, Px, Py
	    MouseClick, L, Px, Py
	 }
	 Sleep, 1000
	Send, {PgDn}
}

dealerstats(){
	searchTerm := searchTermClean()
	Run, http://ops.pearlsolutions.com/rdPage.aspx?rdReport=Caroffer.DealerStatsSummary
	Sleep, 5000
	Send, ^f
	Sleep, 200
	Clip(searchTerm)
	Sleep, 100
	Send, {Enter}
	; https://app.hubspot.com/contacts/5712725/contact/982551/?engagement=%searchTerm%
	; if !RegExMatch(oldClip, "^\d{9,11}$")
		; return
}

searchTermClean(){
	searchTerm := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		searchTerm := oldClip2
	searchTerm := RegExReplace(RegExReplace(searchTerm, "[\']", ""), "\W", " ")
	searchTerm := StrSplit(searchTerm, A_Space)
	searchTerm := searchTerm[1] " "  searchTerm[2] " " searchTerm[3]
	return searchTerm
}

phoneBaseFormat(){
	oldClip := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		oldClip := oldClip2
	oldClip := RegExReplace(oldClip, "[^0-9]", "")
	if (!RegExMatch(oldClip, "^\d{9,11}$"))
		return
	phoneSection1 := SubStr(oldClip, 1, 3)
	phoneSection2 := SubStr(oldClip, 4, 3)
	phoneSection3 := SubStr(oldClip, 7, 4)
	phoneNumber1 := RegExReplace(oldClip, "^.*(\d{3})[^\d]*(\d{3})[^\d]*(\d{4})$", "$1-$2-$3", 1)
	phoneNumber2 := RegExReplace(oldClip, "^.*(\d{3})[^\d]*(\d{3})[^\d]*(\d{4})$", "($1) $2-$3", 1)
	phoneNumber3 := RegExReplace(oldClip, "^.*(\d{3})[^\d]*(\d{3})[^\d]*(\d{4})$", "($1)$2-$3", 1)
	Clipboard := oldClip
	return [phoneNumber1, phoneNumber2, phoneNumber3, phoneSection1, phoneSection2, phoneSection3]
}

phoneFormat(){
	phoneFormat := phoneBaseFormat()
	Toast.show({title:{text:(InStr(phoneFormat[1], true)? phoneFormat[1] : "No Phone #")},life:2000,pos:{x:A_CaretX,y:A_CaretY-70}})
	Clip(phoneFormat[1])
}

phoneFormatPara(){
	phoneFormat := phoneBaseFormat()
	Sleep, 100
	Toast.show({title:{text:(InStr(phoneFormat[2], true)? phoneFormat[2] : "No Phone #")},life:2000,pos:{x:A_CaretX,y:A_CaretY-70}})
	Sleep, 200
	Clip(phoneFormat[2], true)
}

fileRename(){
	Send, {F2}
	oldClip := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		oldClip := oldClip2
	oldClip := StrReplace(StrReplace(StrReplace(StrReplace(oldClip, "_", " "), "-", " "), "   ", " "), "  ", " ")
		oldClip := StrReplace(StrReplace(caseChange(oldClip, "T"), "#", " "), "-", " ")
		oldClip = %oldClip%
		oldClip := StrReplace(oldClip, A_Space, "_")
	Clipboard := oldClip
	Clip(oldClip, true)
	;~ ControlSend, DirectUIHWND3, {Tab}, ahk_class CabinetWClass
}

cleanRename(){
	oldClip := Clipboard
	oldClip2 := Clip()
	if (oldClip2 != "")
		oldClip := oldClip2
	oldClip := StrReplace(StrReplace(StrReplace(StrReplace(oldClip, "_", " "), "-", " "), "   ", " "), "  ", " ")
		oldClip := StrReplace(StrReplace(caseChange(oldClip, "T"), "#", " "), "-", " ")
		oldClip = %oldClip%
	Clipboard := oldClip
	Clip(oldClip, true)
}

WinGetFuctionPaste(){
	WinGetActiveStats, FoundTitle, FoundWidth, FoundHeight, FoundX, FoundY
	PastePos := FoundTitle ", " FoundWidth ", " FoundHeight ", " FoundX ", " FoundY
	Clipboard := "Winmove, " FoundTitle ", , " FoundX ", " FoundY ", "FoundWidth ", " FoundHeight
	Toast.show({title:{text:(Window Stats on Clipboard)},sound:True,life:2000,trans:200})
	return
}

smsSplit(){
	smsSplit := Clip()
	loop {
		if (StrLen(smsSplit) < 160)
			break
		if ( substr(smsSplit, currChar := 160-A_Index, 1) = " " )
			break
	}
	smsSplit1 := substr(smsSplit, 1, currChar-1 )
	smsSplit2 := substr(smsSplit, currChar+1)

	loop {
		if (StrLen(smsSplit2) < 160)
			break
		if ( substr(smsSplit2, currChar := 160-A_Index, 1) = " " )
			break
	}
	smsSplit3 := substr(smsSplit2, 1, currChar-1 )
	smsSplit4 := substr(smsSplit2, currChar+1)

	loop {
		if (StrLen(smsSplit4) < 160)
			break
		if ( substr(smsSplit4, currChar := 160-A_Index, 1) = " " )
			break
	}
	smsSplit5 := substr(smsSplit4, 1, currChar-1 )
	smsSplit6 := substr(smsSplit4, currChar+1)

	loop {
		if (StrLen(smsSplit6) < 160)
			break
		if ( substr(smsSplit6, currChar := 160-A_Index, 1) = " " )
			break
	}
	smsSplit7 := substr(smsSplit6, 1, currChar-1 )
	smsSplit8 := substr(smsSplit6, currChar+1)

		loop {
		if (StrLen(smsSplit8) < 160)
			break
		if ( substr(smsSplit8, currChar := 160-A_Index, 1) = " " )
			break
	}
	smsSplit9 := substr(smsSplit8, 1, currChar-1 )
	smsSplit10 := substr(smsSplit8, currChar+1)

	;~ MsgBox % smsSplit1 "`n" smsSplit3 "`n" smsSplit5 "`n" smsSplit7
	Clip(smsSplit1 "`r`r" smsSplit3 "`r`r" smsSplit5 "`r`r" smsSplit7 "`r`r" smsSplit9)
	return
}
