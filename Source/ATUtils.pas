{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATUtils.pas                                               }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   Public functions, procedures and etc.                     }
{                                                                             }
{ *************************************************************************** }

(* ***** BEGIN LICENSE BLOCK *****
 *
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is ATLogger.
 * Unit owner : ZY (zylove619@hotmail.com) All rights reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

unit ATUtils;

{$I AT.inc}

{$IFDEF USE_DELPHI}
  {$IFDEF CONDITIONALEXPRESSIONS}
    {$IF CompilerVersion >= 25.0}
      {$LEGACYIFEND ON}
    {$IFEND}
  {$ENDIF}
{$ENDIF}

interface

uses

  { System }
  SysUtils, Classes, DateUtils
{$IFDEF MSWINDOWS}
  , Windows, ShellAPI, Messages, Registry, ShlObj, SHFolder
{$ENDIF}

{$IFDEF USE_DELPHI}
  {$IFNDEF MSWINDOWS}, System.IOUtils, FMX.Platform{$ENDIF}
  {$IFDEF ANDROID}
    , Androidapi.Log
  {$ENDIF}
  {$IFDEF IOS}
  , iOSApi.Foundation
  {$ENDIF}
  {$IFDEF MACOS}
  , Macapi.ObjectiveC
  {$ENDIF}
{$ELSE}
  , FileUtil, LazFileUtils, process
  {$IFNDEF MSWINDOWS}
  , LazLogger
  {$ENDIF}
{$ENDIF}

  { ATLib }
  , ATCommon, ATConsts;

{ *************** File And Path *************** }

/// <summary> Get module full name. </summary>
/// <param name="AHInstance"> module handle, 0 is current moudle(only used on MSWindows).</param>
/// <remarks>
///  1. MSWindows: return bin or library's full file name.
///  2. FPC: Resurn ParamStr(0)
/// </remarks>
function ATGetModuleFullName(AHInstance: HMODULE = 0): string;

/// <summary> Get pure module name, without path and ext. </summary>
function ATGetPureModuleName(AHInstance: HMODULE): string;

/// <summary> Get module path. </summary>
function ATGetModulePath(AHInstance: HMODULE = 0): string;

/// <summary> Return ParamStr(0). </summary>
function ATGetAppFullName: string;

/// <summary> Get pure app name, without path and ext. </summary>
function ATGetPureAppName: string;

/// <summary> Get app path. </summary>
function ATGetAppPath: string;

/// <summary> Get user home path. </summary>
function ATGetHomePath: string;

/// <summary> Get file size, -1 if failed. </summary>
function ATGetFileSize(const AFileName: string): Int64;

{ ****************** strings ******************* }

{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
function ATFPCStrToWinWideStr(const AFPCStr: string): WideString;
function ATFPCWinWideStrToStr(const AWinWideStr: WideString): string;
  {$ENDIF}
{$ENDIF}

{ ****************** Windows HWND ******************* }

{$IFDEF MSWINDOWS}
/// <summary> Create HWnd. </summary>
function ATAllocateHWnd(AWndProcMethod: TWndMethod): HWND;

/// <summary> Free HWnd which created by ATAllocateHWnd. </summary>
procedure ATDeallocateHWnd(AWindow: HWND);
{$ENDIF}

{ ****************** DateTime ****************** }

/// <summary> Get the local time zone relative to UTC (Greenwich Mean Time - Local Time) in minutes,
/// for example, -480 in East Eighth Zone.
/// </summary>
function ATGetTimeZoneBiasInMinutes: Integer;

/// <summary> Get timeZone bias str. </summary>
/// <param name="ABias"> Bias in minutes. </param>
function ATGetTimeZoneBiasStr(ABias: Integer): string;

/// <summary> Get local timeZone bias str. </summary>
function ATGetLocalTimeZoneBiasStr: string;

{ ************** System And User ************** }

/// <summary> Get host name. </summary>
function ATGetHostName: string;

/// <summary> Get OS name. </summary>
function ATGetOSName: string;

/// <summary> Get format settings(Clone). </summary>
function ATGetFormatSettings: TFormatSettings;

{ *************** Error And Log *************** }

type

  // <summary> ATLib log level type. </summary>
  TATLibLogLevel = (atllTrace, atllDebug, atllInfo, atllWarn, atllError);

  // <summary> ATLib log event. </summary>
  TATLibLogEvent = procedure(const ALog: string; ALogLevel: TATLibLogLevel) of object;

const

  AT_DEFAULT_LOGLEVEL = atllDebug;

/// <summary> Get Log message. </summary>
function ATGetLogLevelStr(ALogLevel: TATLibLogLevel): string;

/// <summary> Get last OS error message. </summary>
function ATGetLastErrorMsg: string;

/// <summary> Output debug string. </summary>
procedure ATOutputDebugStr(const AStr: string); overload;
procedure ATOutputDebugStr(const AStr: string; const AArgs: array of const); overload;

/// <summary> Output ATLib error. </summary>
procedure ATOutputLibsError(const ARoutineName, AParamsStr: string; const AParamArgs: array of const; const AError: string); overload;
procedure ATOutputLibsError(const ARoutineName, AParams: string; const AParamArgs: array of const; const AError: string; const AErrorArgs: array of const); overload;
procedure ATOutputLibsError(const ARoutineName, AError: string); overload;

var
  GATLibLogHookProc: TATLibLogEvent;

implementation

var
  GATLibFormatSettings: TFormatSettings; 

procedure ATOutputDebugLibsMsg(const ADebugMsg: string; const ACategory: string = ''; ALogLevel: TATLibLogLevel = AT_DEFAULT_LOGLEVEL);
var
  LMsg: string;
begin
  if Assigned(GATLibLogHookProc) then
  begin
    if ACategory <> '' then
      LMsg := Format('[%s] %s\%s', [ATGetLogLevelStr(ALogLevel), ACategory, ADebugMsg], GATLibFormatSettings)
    else
      LMsg := Format('[%s] %s', [ATGetLogLevelStr(ALogLevel), ADebugMsg], GATLibFormatSettings);

    GATLibLogHookProc(LMsg, ALogLevel);
  end;
end;

procedure ATOutputLibsError(const ARoutineName, AParamsStr: string; const AParamArgs: array of const; const AError: string);
begin
  if AParamsStr = '' then
    ATOutputDebugLibsMsg(Format('%s: %s', [ARoutineName, AError], GATLibFormatSettings), '', atllError)
  else
    ATOutputDebugLibsMsg(Format('%s(%s): %s',
      [ARoutineName, Format(AParamsStr, AParamArgs, GATLibFormatSettings), AError], GATLibFormatSettings), '', atllError);
end;

procedure ATOutputLibsError(const ARoutineName, AParams: string; const AParamArgs: array of const; const AError: string; const AErrorArgs: array of const);
begin
  ATOutputLibsError(ARoutineName, AParams, AParamArgs, Format(AError, AErrorArgs, GATLibFormatSettings));
end;

procedure ATOutputLibsError(const ARoutineName, AError: string);
begin
  ATOutputLibsError(ARoutineName, '', [], AError);
end;

{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
function ATFPCDefStrToWinWideStr(const AFPCStr: string): WideString;
begin
  Result := UTF8Decode(AFPCStr);
end;

function ATFPCWinWideStrToDefStr(const AWinWideStr: WideString): string;
begin
  Result := UTF8Encode(AWinWideStr);
end;
  {$ELSE}
function GetLastError: Integer;
begin
  Result := GetLastOSError;
end;
  {$ENDIF MSWINDOWS}

function ATFPCRunCommand(const exename:TProcessString;
    const commands:array of TProcessString; out outputstring:string; Options : TProcessOptions = [poWaitOnExit, poUsePipes];
    SWOptions:TShowWindowOptions=swoNone):boolean;
begin
  Result := RunCommand(exename, commands, outputstring, Options, SWOptions);
  if Result then
    outputstring := Trim(outputstring);
end;
{$ENDIF FPC}

function ATGetModuleFullName(AHInstance: HMODULE = 0): string;
begin
{$IFDEF USE_DELPHI}
  if AHInstance = 0 then
    Result := GetModuleName(HInstance)
  else
    Result := GetModuleName(AHInstance);
{$ELSE}
  if not IsLibrary then
    Result := ParamStr(0)
  else
  begin
    Result := '';
    ATOutputLibsError('ATGetModuleFullName', 'in library not supported');
    {$Message Warn 'FPC in library ATGetModuleFullName not implemented'}
  end;
{$ENDIF}
end;

function ATGetPureModuleName(AHInstance: HMODULE): string;
begin
  Result := ChangeFileExt(ExtractFileName(ATGetModuleFullName(AHInstance)), '');
end;

function ATGetModulePath(AHInstance: HMODULE): string;
begin
  Result := ExtractFilePath(ATGetModulePath(AHInstance));
end;

function ATGetAppFullName: string;
begin
  Result := ParamStr(0);
end;

function ATGetPureAppName: string;
{$IF Defined(USE_DELPHI) and not Defined(MSWINDOWS)}
var
  LAppService: IFMXApplicationService;
{$IFEND}
begin
  Result := ChangeFileExt(ExtractFileName(ATGetAppFullName()), '');
{$IF Defined(USE_DELPHI) and not Defined(MSWINDOWS)}
  if Result = '' then
  begin
    if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationService, LAppService) then
    begin
      Result := LAppService.Title;
      if Result = '' then
        Result := LAppService.DefaultTitle;
    end;
  end;
{$IFEND}
end;

function ATGetAppPath: string;
begin
  Result := ExtractFilePath(ATGetAppFullName());
end;

function ATGetHomePath: string;
{$IFDEF MSWINDOWS}
const
  SHGFP_TYPE_CURRENT = 0;
var
  LPath: array[0..MAX_PATH] of WideChar;
  LResultStr: WideString;
begin
  if SHGetFolderPathW(0, CSIDL_APPDATA, 0, SHGFP_TYPE_CURRENT, @LPath) = S_OK then
  begin
    LResultStr := LPath;
    Result := {$IFDEF FPC}ATFPCWinWideStrToDefStr{$ENDIF}(LResultStr);
    Result := IncludeTrailingPathDelimiter(Result);
  end else
    Result := '';
end;
{$ELSE}
begin
  {$IFDEF USE_DELPHI}
  Result := TPath.GetHomePath;
  {$ELSE}
  Result := IncludeTrailingPathDelimiter(GetUserDir);
  {$ENDIF USE_DELPHI}
end;
{$ENDIF MSWINDOWS}

function ATGetFileSize(const AFileName: string): Int64;
var
  LSR: SysUtils.TSearchRec;
begin
  if SysUtils.FindFirst(AFileName, faAnyFile, LSR) = 0 then
  begin
{$IFDEF USE_DELPHI}
  {$IFDEF D2006AndUp}
    Result := LSR.Size;
  {$ELSE}
    Result := (Int64(LSR.FindData.nFileSizeHigh) shl 32) + LSR.FindData.nFileSizeLow;
  {$ENDIF}
{$ELSE}
    Result := LSR.Size;
{$ENDIF}
    SysUtils.FindClose(LSR);
  end else
    Result := -1;
end;

{$IFDEF FPC}
  {$IFDEF MSWINDOWS}
function ATFPCStrToWinWideStr(const AFPCStr: string): WideString;
begin
  Result := UTF8Decode(AFPCStr);
end;

function ATFPCWinWideStrToStr(const AWinWideStr: WideString): string;
begin
  Result := UTF8Encode(AWinWideStr);
end;
  {$ENDIF MSWINDOWS}
{$ENDIF FPC}

{$IFDEF MSWINDOWS}

{ ************** Thread-safe AllocateHwnd and DeallocateHWnd BEGIN *************}
const //DSiAllocateHwnd window extra data offsets
  GWL_METHODCODE = SizeOf(pointer) * 0;
  GWL_METHODDATA = SizeOf(pointer) * 1;

  //DSiAllocateHwnd hidden window (and window class) name
  CDSiHiddenWindowName = 'ATLibrary_Global_DSiUtilWindow';

var
  //DSiAllocateHwnd lock
  GDSiWndHandlerCritSect: TRTLCriticalSection;
  //Count of registered windows in this instance
  GDSiWndHandlerCount: integer;

{:Class message dispatcher for the DSiUtilWindow class. Fetches instance's WndProc from
  the window extra data and calls it.
}
function DSiClassWndProc(Window: HWND; Message: UINT; WParam: WPARAM; LParam: LPARAM): LRESULT; stdcall;
var
  instanceWndProc: TMethod;
  msg            : TMessage;
begin
{$IFDEF CPU_IS_64BITS}
  instanceWndProc.Code := {%H-}pointer(GetWindowLongPtr(Window, GWL_METHODCODE));
  instanceWndProc.Data := {%H-}pointer(GetWindowLongPtr(Window, GWL_METHODDATA));
{$ELSE}
  instanceWndProc.Code := {%H-}pointer(GetWindowLong(Window, GWL_METHODCODE));
  instanceWndProc.Data := {%H-}pointer(GetWindowLong(Window, GWL_METHODDATA));
{$ENDIF}

  if Assigned(TWndMethod(instanceWndProc)) then
  begin
    msg.msg := Message;
    msg.wParam := WParam;
    msg.lParam := LParam;
    msg.Result := 0;
    TWndMethod(instanceWndProc)(msg);
    Result := msg.Result
  end
  else
    Result := DefWindowProc(Window, Message, WParam, LParam);
end; { DSiClassWndProc }

{:Thread-safe AllocateHwnd.
  @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
                 TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
  @since   2007-05-30
}
{$HINTS OFF} // Hide "Value assigned to 'DSiAllocateHWnd' never used."
function DSiAllocateHWnd(wndProcMethod: TWndMethod): HWND;
var
  alreadyRegistered: boolean;
  tempClass        : TWndClass;
  utilWindowClass  : TWndClass;
begin
  Result := 0;
  FillChar(utilWindowClass, SizeOf(utilWindowClass), 0);
  EnterCriticalSection(GDSiWndHandlerCritSect);
  try
    alreadyRegistered := GetClassInfo(HInstance, CDSiHiddenWindowName, tempClass);
    if (not alreadyRegistered) or (@tempClass.lpfnWndProc <> @DSiClassWndProc) then begin
      if alreadyRegistered then
        Windows.UnregisterClass(CDSiHiddenWindowName, HInstance);
      utilWindowClass.lpszClassName := CDSiHiddenWindowName;
      utilWindowClass.hInstance := HInstance;
      utilWindowClass.lpfnWndProc := @DSiClassWndProc;
      utilWindowClass.cbWndExtra := SizeOf(TMethod);
      if Windows.RegisterClass(utilWindowClass) = 0 then
        raise Exception.CreateFmt('Unable to register DSiWin32 hidden window class. %s',
          [SysErrorMessage(GetLastError)]);
    end;
    Result := CreateWindowEx(WS_EX_TOOLWINDOW, CDSiHiddenWindowName, '', WS_POPUP,
      0, 0, 0, 0, 0, 0, HInstance, nil);
    if Result = 0 then
      raise Exception.CreateFmt('Unable to create DSiWin32 hidden window. %s',
              [SysErrorMessage(GetLastError)]);
    {$IFDEF CPU_IS_64BITS}
    SetWindowLongPtr(Result, GWL_METHODDATA, NativeInt(TMethod(wndProcMethod).Data));
    SetWindowLongPtr(Result, GWL_METHODCODE, NativeInt(TMethod(wndProcMethod).Code));
    {$ELSE}
    SetWindowLong(Result, GWL_METHODDATA, cardinal(TMethod(wndProcMethod).Data));
    SetWindowLong(Result, GWL_METHODCODE, cardinal(TMethod(wndProcMethod).Code));
    {$ENDIF}
    Inc(GDSiWndHandlerCount);
  finally LeaveCriticalSection(GDSiWndHandlerCritSect); end;
end; { DSiAllocateHWnd }
{$HINTS ON}

{:Thread-safe DeallocateHwnd.
  @author  gabr [based on http://fidoforum.ru/pages/new46s35o217746.ru.delphi and
                 TIcsWndHandler.AllocateHWnd from ICS v6 (http://www.overbyte.be)]
  @since   2007-05-30
}
procedure DSiDeallocateHWnd(wnd: HWND);
begin
  if wnd = 0 then
    Exit;
  DestroyWindow(wnd);
  EnterCriticalSection(GDSiWndHandlerCritSect);
  try
    Dec(GDSiWndHandlerCount);
    if GDSiWndHandlerCount <= 0 then
      Windows.UnregisterClass(CDSiHiddenWindowName, HInstance);
  finally LeaveCriticalSection(GDSiWndHandlerCritSect); end;
end; { DSiDeallocateHWnd }
{ ************** Thread-safe AllocateHwnd and DeallocateHWnd END***************}

function ATAllocateHWnd(AWndProcMethod: TWndMethod): HWND;
begin
  Result := DSiAllocateHWnd(AWndProcMethod);
end;

procedure ATDeallocateHWnd(AWindow: HWND);
begin
  DSiDeallocateHWnd(AWindow);
end;

{$ENDIF MSWINDOWS}

function ATGetTimeZoneBiasInMinutes: Integer;
{$IFDEF USE_DELPHI}
var
  {$IFDEF MSWINDOWS}
  LInfo: TTimeZoneInformation;
  {$ELSE}
  LTimeZone: TTimeZone;
  {$ENDIF}
begin
{$IFDEF MSWINDOWS}
  FillChar(LInfo, SizeOf(LInfo), 0);
  case GetTimeZoneInformation(LInfo) of
    TIME_ZONE_ID_DAYLIGHT: Result := LInfo.Bias + LInfo.DaylightBias;
    TIME_ZONE_ID_STANDARD: Result := LInfo.Bias + LInfo.StandardBias;
    TIME_ZONE_ID_UNKNOWN:  Result := LInfo.Bias;
    else raise EATException.CreateResFmt(@atsGetTimeZoneBiasFailed, [ATGetLastErrorMsg]); { TIME_ZONE_ID_INVALID }
  end;
{$ELSE}
  LTimeZone := TTimeZone.Local;
  Result := Trunc(LTimeZone.UtcOffset.Negate.TotalMinutes);
{$ENDIF}
end;
{$ELSE}
var
  LDateTime, LUtc: TDateTime;
begin
  LDateTime := Now();
  LUtc := LocalTimeToUniversal(LDateTime);
  Result := MinutesBetween(LDateTime, LUtc);
  if LDateTime > LUtc then
    Result := -Result;
end;
{$ENDIF USE_DELPHI}

{$IFDEF DELPHI_ANSI_VERSION}
type
  TComputerNameFormat = (ComputerNameNetBIOS,
    ComputerNameDnsHostname, ComputerNameDnsDomain,
    ComputerNameDnsFullyQualified, ComputerNamePhysicalNetBIOS,
    ComputerNamePhysicalDnsHostname, ComputerNamePhysicalDnsDomain,
    ComputerNamePhysicalDnsFullyQualified, ComputerNameMax);

function GetComputerNameExW(NameType: TComputerNameFormat; lpBuffer: LPWSTR; var nSize: DWORD): BOOL; stdcall;
  external kernel32;
{$ENDIF}

function ATGetHostName: string;
{$IFDEF MSWINDOWS}
var
  LLen: DWORD;
  LResultStr: WideString;
begin
  Result := '';
  LLen := 0;
  GetComputerNameExW(ComputerNameDnsHostname, nil, {$IFDEF USE_DELPHI}LLen{$ELSE}@LLen{$ENDIF});
  if LLen > 0 then
  begin
    SetLength(LResultStr, LLen - 1);
    if GetComputerNameExW(ComputerNameDnsHostname, PWideChar(LResultStr),
      {$IFDEF USE_DELPHI}LLen{$ELSE}@LLen{$ENDIF}) then
      Result := string(LResultStr);
  end;
end;
{$ELSE}
  {$IFDEF FPC}
begin
  if not ATFPCRunCommand('hostname', [], Result) then  { Do not localize }
    Result := '';
end;
  {$ELSE}
begin
    Result := '';
    {$Message Warn 'ATGetHostName not implemented'}
end;
  {$ENDIF FPC}
{$ENDIF MSWINDOWS}

function ATGetOSName: string;
const
  CUnknownOsName = 'Unknown OS';

  {$IFDEF MSWINDOWS}
  function GetWinOSArchitecture: string;
  const
    CProcessorArchitectureAMD64 = 9;
    CArchitecture32     = '32-bit';
    CArchitecture64     = '64-bit';
    CArchitectureUnknow = 'Unknown-bit';
  var
    LSystemInfo: TSystemInfo;
    LGetNativeSystemInfo: procedure(var lpSystemInformation: TSystemInfo); stdcall;
  begin
    LGetNativeSystemInfo := GetProcAddress(GetModuleHandle(Kernel32), 'GetNativeSystemInfo');
    if Assigned(LGetNativeSystemInfo) then
    begin
      ZeroMemory(@LSystemInfo, SizeOf(LSystemInfo));
      LGetNativeSystemInfo(LSystemInfo);
      if (LSystemInfo.wProcessorArchitecture = CProcessorArchitectureAMD64) then
        Result := CArchitecture64
      else
        Result := CArchitecture32;
    end else
      Result := CArchitectureUnknow;
  end;

  function GetWinOSVersion: string;
  var
    LRegistry: TRegistry;
    LProductName,
    LVersion,
    LCSDVersion: string;
  begin
    LRegistry := TRegistry.Create(KEY_READ);
    try
      LRegistry.RootKey := HKEY_LOCAL_MACHINE;
      if LRegistry.OpenKey('SOFTWARE\Microsoft\Windows NT\CurrentVersion', False) then
        try
          LProductName := LRegistry.ReadString('ProductName');

          if LRegistry.ValueExists('CurrentMajorVersionNumber') then
            LVersion := Format('%d.%d.%s', [LRegistry.ReadInteger('CurrentMajorVersionNumber'),
                                            LRegistry.ReadInteger('CurrentMinorVersionNumber'),
                                            LRegistry.ReadString ('CurrentBuildNumber')], GATLibFormatSettings)
          else
            LVersion := Format('%s.%s', [LRegistry.ReadString('CurrentVersion'),
                                         LRegistry.ReadString('CurrentBuildNumber')], GATLibFormatSettings);

          if LRegistry.ValueExists('CSDVersion') then
          begin
            LCSDVersion := LRegistry.ReadString('CSDVersion');
            if LCSDVersion <> '' then
              LCSDVersion := ' ' + LCSDVersion + ' ';
          end;

          Result := Format('%s%s(Version %s, %s)', [LProductName, LCSDVersion,
                                                    LVersion, GetWinOSArchitecture], GATLibFormatSettings);
        except
          Result := CUnknownOsName;
        end
      else
        Result := CUnknownOsName;
    finally
      LRegistry.Free;
    end;
  end;
  {$ENDIF MSWINDOWS}

begin
{$IFDEF MSWINDOWS}
  Result := GetWinOSVersion;
{$ELSE}
  {$IFDEF USE_DELPHI}
    {$IFDEF HAS_TOSVERSION}
    Result := TOSVersion.ToString;
    {$ELSE}
    Result := CUnknownOsName;
    {$ENDIF}
  {$ELSE}
    if not ATFPCRunCommand('uname', ['-m', '-o', '-v'], Result) then  { Do not localize }
      Result := CUnknownOsName;
  {$ENDIF}
{$ENDIF}
end;

function ATGetFormatSettings: TFormatSettings;
{$IFDEF USE_DELPHI}
begin
  {$IFDEF DXEAndUp}
  Result := TFormatSettings.Create;
  {$ELSE}
  GetLocaleFormatSettings(SysLocale.DefaultLCID, Result);
  {$ENDIF}
end;
{$ELSE}
var
  I: Integer;
begin
  Result := DefaultFormatSettings;
  UniqueString(Result.CurrencyString);
  UniqueString(Result.ShortDateFormat);
  UniqueString(Result.LongDateFormat);
  UniqueString(Result.TimeAMString);
  UniqueString(Result.TimePMString);
  UniqueString(Result.ShortTimeFormat);
  UniqueString(Result.LongTimeFormat);

  for I := Low(Result.ShortMonthNames) to High(Result.ShortMonthNames) do
    UniqueString(Result.ShortMonthNames[I]);

  for I := Low(Result.LongMonthNames) to High(Result.LongMonthNames) do
    UniqueString(Result.LongMonthNames[I]);

  for I := Low(Result.ShortDayNames) to High(Result.ShortDayNames) do
    UniqueString(Result.ShortDayNames[I]);

  for I := Low(Result.LongDayNames) to High(Result.LongDayNames) do
    UniqueString(Result.LongDayNames[I]);
end;
{$ENDIF USE_DELPHI}

function ATGetTimeZoneBiasStr(ABias: Integer): string;
const
  COffsetFormat: string = '%s%.02d:%.02d';     { Do not localize }
  CNeg: array[Boolean] of string = ('+', '-'); { Do not localize }
begin
  Result := Format(COffsetFormat, [CNeg[ABias > 0], Abs(ABias) div MinsPerHour, Abs(ABias) mod MinsPerHour],
    GATLibFormatSettings);
end;

function ATGetLocalTimeZoneBiasStr: string;
begin
  Result := ATGetTimeZoneBiasStr(ATGetTimeZoneBiasInMinutes());
end;

function ATGetLogLevelStr(ALogLevel: TATLibLogLevel): string;
const
  CLevelStrs: array[TATLibLogLevel] of string =
    (atsLogLevelTrace, atsLogLevelDebug, atsLogLevelInfo, atsLogLevelWarn, atsLogLevelError);
begin
  Result := CLevelStrs[ALogLevel];
end;

function ATGetLastErrorMsg: string;
begin
  Result := SysErrorMessage(GetLastError());
end;

procedure ATNativeOutputStr(const AStr: string); {$IFDEF HAS_INLINE}inline;{$ENDIF}
{$IFDEF USE_DELPHI}

{$IFDEF IOS}
  function PNSStr(const AStr: string): PNSString;
  begin
    Result := (NSStr(AStr) as ILocalObject).GetObjectID;
  end;
{$ENDIF}

{$IFDEF ANDROID}
var
  LMarshaller: TMarshaller;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  OutputDebugString(PChar(AStr));
{$ENDIF}
{$IFDEF IOS}
  NSLog(PNSStr(AStr));
{$ENDIF}
{$IFDEF MACOS}
  {$IFDEF DXE3AndUp}
  Log.d(AStr);
  {$ELSE}
    {$Message Hint 'ATNativeOutputStr not implemented'}
  {$ENDIF}
{$ENDIF}
{$IFDEF ANDROID}
  LOGI(LMarshaller.AsAnsi(AStr).ToPointer);
{$ENDIF}
end;
{$ELSE}
begin
{$IFDEF MSWINDOWS}
  Windows.OutputDebugStringW(PWideChar(ATFPCStrToWinWideStr(AStr)));
{$ELSE}
  DbgStr(AStr);
{$ENDIF}
end;
{$ENDIF USE_DELPHI}

procedure ATOutputDebugStr(const AStr: string); overload;
begin
  ATNativeOutputStr(AStr);
end;

procedure ATOutputDebugStr(const AStr: string; const AArgs: array of const); overload;
begin
  ATOutputDebugStr(Format(AStr, AArgs, GATLibFormatSettings));
end;

initialization
  GATLibFormatSettings := ATGetFormatSettings;
{$IFDEF MSWINDOWS}
  InitializeCriticalSection(GDSiWndHandlerCritSect);
{$ENDIF}

finalization
{$IFDEF MSWINDOWS}
  DeleteCriticalSection(GDSiWndHandlerCritSect);
{$ENDIF}

end.
