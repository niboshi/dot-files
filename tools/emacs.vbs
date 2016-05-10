' Executable path
exeDirPath = "C:\\ProgramData\\chocolatey\\bin\\"
emacsclient = exeDirPath & "emacsclient.exe"
runemacs    = exeDirPath & "runemacs.exe"
commandBase = emacsclient & " -na " & runemacs

' Choose file path
defaultFilePath = "~/.emacs.d/init.el"

Set args = WScript.Arguments
If args.Count = 0 Then
	filePath = defaultFilePath
Else
	filePath = args(0)
End If

' Run
Set shell = WScript.CreateObject("WScript.Shell")
shell.Run(commandBase & " " & filePath)
