; Inno Setup Script for Audacity Cloud AI
; https://jrsoftware.org/isinfo.php

#define MyAppName "Audacity Cloud AI"
#define MyAppVersion "1.0.0"
#define MyAppPublisher "Bjorn Brorsson"
#define MyAppURL "https://github.com/BjornBrorsson/cloud-audio-ai-audacity-addon"
#define MyAppExeName "Start GUI.bat"

[Setup]
AppId={{A7B8C9D0-E1F2-4A5B-8C9D-0E1F2A3B4C5D}
AppName={#MyAppName}
AppVersion={#MyAppVersion}
AppPublisher={#MyAppPublisher}
AppPublisherURL={#MyAppURL}
AppSupportURL={#MyAppURL}/issues
AppUpdatesURL={#MyAppURL}/releases
DefaultDirName={autopf}\{#MyAppName}
DefaultGroupName={#MyAppName}
LicenseFile=..\..\LICENSE
OutputDir=..\..\dist
OutputBaseFilename=AudacityCloudAI-Setup-v{#MyAppVersion}
Compression=lzma
SolidCompression=yes
WizardStyle=modern
ArchitecturesInstallIn64BitMode=x64
PrivilegesRequired=lowest
DisableProgramGroupPage=yes

[Languages]
Name: "english"; MessagesFile: "compiler:Default.isl"

[Tasks]
Name: "desktopicon"; Description: "{cm:CreateDesktopIcon}"; GroupDescription: "{cm:AdditionalIcons}"
Name: "quicklaunchicon"; Description: "{cm:CreateQuickLaunchIcon}"; GroupDescription: "{cm:AdditionalIcons}"; Flags: unchecked
Name: "audacityintegration"; Description: "Install Nyquist plugins to Audacity (if found)"; GroupDescription: "Audacity Integration"; Flags: checkedonce

[Files]
Source: "..\..\*"; DestDir: "{app}"; Flags: ignoreversion recursesubdirs createallsubdirs
; Exclude git files
Source: "..\..\src\*"; DestDir: "{app}\src"; Flags: ignoreversion recursesubdirs
Source: "..\..\docs\*"; DestDir: "{app}\docs"; Flags: ignoreversion recursesubdirs
Source: "..\..\examples\*"; DestDir: "{app}\examples"; Flags: ignoreversion recursesubdirs
Source: "..\..\nyquist\*"; DestDir: "{app}\nyquist"; Flags: ignoreversion recursesubdirs

[Icons]
Name: "{group}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"
Name: "{group}\{cm:UninstallProgram,{#MyAppName}}"; Filename: "{uninstallexe}"
Name: "{autodesktop}\{#MyAppName}"; Filename: "{app}\{#MyAppExeName}"; Tasks: desktopicon

[Run]
Filename: "{app}\install.bat"; Description: "Install dependencies"; Flags: postinstall shellexec
Filename: "{app}\{#MyAppExeName}"; Description: "{cm:LaunchProgram,{#StringChange(MyAppName, '&', '&&')}}"; Flags: nowait postinstall skipifsilent

[Code]
function IsPythonInstalled: Boolean;
var
  ResultCode: Integer;
begin
  Result := Exec('python', '--version', '', SW_HIDE, ewWaitUntilTerminated, ResultCode) and (ResultCode = 0);
end;

function GetAudacityPluginPath: String;
var
  AudacityPath: String;
  PluginPath: String;
  AppDataPath: String;
begin
  Result := '';
  
  // Check Program Files
  if DirExists(ExpandConstant('{pf}\Audacity\Plug-Ins')) then
    Result := ExpandConstant('{pf}\Audacity\Plug-Ins')
  else if DirExists(ExpandConstant('{pf32}\Audacity\Plug-Ins')) then
    Result := ExpandConstant('{pf32}\Audacity\Plug-Ins')
  else begin
    // Check user AppData
    AppDataPath := ExpandConstant('{userappdata}\audacity\Plug-Ins');
    if DirExists(AppDataPath) then
      Result := AppDataPath
    else begin
      // Check LocalAppData
      AppDataPath := ExpandConstant('{localappdata}\Audacity\Plug-Ins');
      if DirExists(AppDataPath) then
        Result := AppDataPath;
    end;
  end;
  
  Log('Audacity Plugin Path: ' + Result);
end;

procedure InstallAudacityPlugins;
var
  PluginPath: String;
  SourcePath: String;
  FindRec: TFindRec;
  FilesInstalled: Integer;
begin
  if not IsTaskSelected('audacityintegration') then
    Exit;
    
  PluginPath := GetAudacityPluginPath();
  
  if PluginPath = '' then
  begin
    MsgBox('Audacity installation not found.' + #13#10 + 
           'You can manually copy .ny files from:' + #13#10 +
           ExpandConstant('{app}\nyquist\') + #13#10 +
           'to your Audacity Plug-Ins folder.', 
           mbInformation, MB_OK);
    Exit;
  end;
  
  // Copy all .ny files
  SourcePath := ExpandConstant('{app}\nyquist\');
  FilesInstalled := 0;
  
  if FindFirst(SourcePath + '*.ny', FindRec) then
  begin
    try
      repeat
        if FileCopy(SourcePath + FindRec.Name, PluginPath + '\' + FindRec.Name, False) then
          FilesInstalled := FilesInstalled + 1;
      until not FindNext(FindRec);
    finally
      FindClose(FindRec);
    end;
  end;
  
  if FilesInstalled > 0 then
    MsgBox(IntToStr(FilesInstalled) + ' Nyquist plugin(s) installed to Audacity!' + #13#10 +
           'Location: ' + PluginPath + #13#10 + #13#10 +
           'Restart Audacity to see the new AI menu items.', 
           mbInformation, MB_OK);
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
  if CurStep = ssPostInstall then
  begin
    InstallAudacityPlugins();
  end;
end;

function InitializeSetup: Boolean;
begin
  Result := True;
  if not IsPythonInstalled then
  begin
    if MsgBox('Python 3.8 or later is required but not found. Do you want to continue anyway?' + #13#10 + 
              'You will need to install Python manually from https://python.org', mbConfirmation, MB_YESNO) = IDNO then
      Result := False;
  end;
end;
