{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATOnlyOneAppInst.pas                                      }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   Ensure only one single instance of the application runs.  }
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
 * The Original Code is ATOnlyOneAppInst.
 * Unit owner : ZY (zylove619@hotmail.com) All rights reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

(*

  Change log:
  
  Version 1.001 by ZY:
    (2016.05.10) + First version created.

  Version 1.002 by ZY:
    (2021.06.01) + Support application cmd line param.
                 - remove all functions that related to VCL, as
                   we need support fmx.

  Version 1.003 by ZY:
    (2021.12.31) + Add application check proc.
                 * fixed a receive message lag issue.

  Version 1.004 by ZY:
    (2022.01.15) * Change param string type.

*)

unit ATOnlyOneAppInst;

{$I AT.inc}

interface

{$IFNDEF MSWINDOWS}
  {$MESSAGE ERROR 'ATOnlyOneAppInst currently only supported on MSWINDOWS.'}
{$ENDIF}

const

  ATOnlyOneAppInstVersion = '1.004';

type

{$IFDEF UNICODE}
  ATOOAIParamString = string;
  PATOOAIParamChar  = PChar;
{$ELSE}
  ATOOAIParamString = WideString;
  PATOOAIParamChar  = PWideChar;
{$ENDIF}

  /// <summary> Application callback event. </summary>
  /// <param name="ANextPID"> The PID of the next app, zero if no param passed. </param>
  /// <param name="ANextParam"> The param of the next app, empty if no param passed. </param>
  /// <remarks> This callback executed on the first application context. </remarks>
  TATAppCallback = {$IFDEF HAS_ANONYMOUSMETHOD}reference to{$ENDIF}
                   procedure(ANextPID: UInt64; const ANextParam: ATOOAIParamString);

  /// <summary> Application check procedure. </summary>
  /// <param name="IsAppRunning"> Indicates whether application is already running. </param>
  /// <param name="ANeedNotify"> If IsAppRunning = true and ANeedNotify = true then we will
  ///                            notify the first application, default is true.
  /// </param>
  /// <remarks> This proc executed on each application context, when it's the first one
  ///           to run, "IsAppRunning" is false. otherwise is true and you can use this  proc
  ///           to do some initializations before the notification.
  /// </remarks>
  TATAppCheckProc = {$IFDEF HAS_ANONYMOUSMETHOD}reference to{$ENDIF}
                   procedure(IsAppRunning: Boolean; var ANeedNotify: Boolean);

  /// <summary> The only one application instance interface. </summary>
  IATOnlyOneAppInst = interface
    ['{B1FBB24B-F793-48E3-88C2-D9C4E2B7262A}']
    /// <summary> Check if an application is already running. </summary>
    function IsAppRunning: Boolean;
  end;

/// <summary> Get the only one application instance interface. </summary>
/// <param name="AAppGlobalUniqueID"> App global unique id, if empty then a default id will be used. </param>
/// <param name="AOnNextAppCall"> App callback event. </param>
/// <param name="AOnAppCheckProc"> App check proc. </param>
/// <returns> Return an interface(IATOnlyOneAppInst) </returns>
///
/// <remarks> NOTE 1. AppGlobalUniqueID is a global unique id, it is best to use
///                   a guid combination string and should not already used by
///                   other objects, like Event, Mutex etc.
///
///                2. Usually it only needs to be used once in dpr file, and do
///                   not used in thread, it is pointless.
///
///                3. Application callback not supported if your application is
///                   not a GUI application(required message loop), in other words,
///                   you can only check if application is running or not.
///
///                4. On MSWindows, it only affects the same session, in other words,
///                   both userA and userB can run the same appcalition on their own session.
///
///                5. When cmd param is not empty, the app callback event should not
///                   do heavy work, because the next application will wait for a while
///                   until it finished processing callback or times out.
///
///                6. When use the same global unique id, you can transfer unicode param
///                   from Unicode-App to Ansi-App.
///
///                7. You can also mix use it in both 32-bit and 64-bit app;
///
/// </remarks>
function OnlyOneAppInst(const AAppGlobalUniqueID: string = '';
  AOnNextAppCall: TATAppCallback = nil; AOnAppCheckProc: TATAppCheckProc = nil): IATOnlyOneAppInst;

(* Simple usage(See samples folder for more usages):

in dpr:

procedure MyOnAppCall(ANextPID: UInt64; const ANextParam: ATOOAIParamString);
begin
  { App is already running, you can use the param if exists. }
end;

begin                                  `
  Application.Initialize;

  if OnlyOneAppInst('Your Global Unique ID', MyOnAppCall).IsAppRunning then
    Exit;

  { ... }
end;
*)

implementation

uses
  Windows, Messages, SysUtils, Classes;

resourcestring
  sCreateMutexError           = 'Create mutex ''%s'' failed, error msg: %s.';
  sCreateHWndError            = 'Create HWnd failed, error msg: %s.';
  sRegisterWindowMessageError = 'Register window message ''%s'' failed, error msg: %s.';
  sCreateMemFileMappingFailed = 'Create mem file mapping ''%s'' failed, error msg: %s.';
  sOpenMemFileMappingFailed   = 'Open mem file mapping ''%s'' failed, error msg: %s.';
  sAccessMemDataFailed        = 'Access mem data failed, error msg: %s.';

//{$DEFINE Debug_ATOnlyOneAppInst}

procedure DebugOutput(const AMsg: string); overload;
begin
{$IFDEF Debug_ATOnlyOneAppInst}
  OutputDebugString(PChar(Format('[PID=%u] ', [GetCurrentProcessId]) + AMsg));
{$ENDIF}
end;

procedure DebugOutput(const AMsg: string; const AArgs: array of const); overload;
begin
  DebugOutput(Format(AMsg, AArgs));
end;

function GetGlobalMemFileMappingName(APID: DWORD): string;
begin
  Result := Format('{2B24DFE7-E726-4E10-A45D-2A11D2839495}_ATOnlyOneAppInst_MMN_%u', [APID]);
end;

function GenerateDefaultGlobalUniqueID: string;
var
  LPureAppName: string;
begin
  LPureAppName := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  Result := Format('{1130D592-9727-417F-BB65-5C38FEA63B96}_ATOnlyOneAppInst_Def_%s', [LPureAppName]);
end;

procedure ATShowException(AException: Exception; const ATitle: string = '');
begin
  if (AException = nil) or (AException is EAbort) then
    Exit;

  MessageBox(0, PChar(AException.Message), PChar(Pointer(ATitle)),
    MB_OK or MB_ICONSTOP or MB_TOPMOST);
end;

type

{$IFNDEF HAS_TBYTES}
  TBytes = array of Byte;
{$ENDIF}

  TATCustomOnlyOneAppInst = class(TInterfacedObject, IATOnlyOneAppInst)
  private
    FIsAppRunning: Boolean;
    FAppGlobalUniqueID: string;
    FOnAppCallback: TATAppCallback;
    FOnAppCheckProc: TATAppCheckProc;
  protected
    function GetPureParam: ATOOAIParamString; virtual;
    function ParamStrToBytes(const AStr: ATOOAIParamString): TBytes; virtual;
    function BytesToParamStr(AStrBytes: TBytes): ATOOAIParamString; virtual;
    function ParamStrToDefaultStr(const AStr: ATOOAIParamString): string; virtual;
    function DefaultStrToParamStr(const AStr: string): ATOOAIParamString; virtual;
    procedure DoAppCallback(ANextPID: DWORD; const ANextParam: ATOOAIParamString); virtual;
    procedure DoAppCheckProc(AIsAppRunning: Boolean; var ANeedNotify: Boolean); virtual;
    { IATOnlyOneAppInst }
    function IsAppRunning: Boolean; virtual; abstract;
  public
    constructor Create(const AAppGlobalUniqueID: string; AOnAppCallback: TATAppCallback;
      AOnAppCheckProc: TATAppCheckProc); virtual;
  end;

  TATWinOnlyOneAppInst = class(TATCustomOnlyOneAppInst)
  private
    FAppMutex: THandle;
    FAppMsgHandle: HWND;
    FAppMsgID: UINT;
    procedure InternalWndProc(var AMsg: TMessage);
    procedure ProcessAppCallback(APID: DWORD; AParamSize: Integer);
  protected
    function IsAppRunning: Boolean; override;
  public
    constructor Create(const AAppGlobalUniqueID: string; AOnAppCallback: TATAppCallback;
      AOnAppCheckProc: TATAppCheckProc); override;
    destructor Destroy; override;
  end;

{ TATCustomOnlyOneAppInst }

function TATCustomOnlyOneAppInst.BytesToParamStr(AStrBytes: TBytes): ATOOAIParamString;
begin
  if Length(AStrBytes) > 0 then
    SetString(Result, PATOOAIParamChar(@AStrBytes[Low(AStrBytes)]),
      Length(AStrBytes) div (SizeOf(Result[1])))
  else
    Result := '';
end;

constructor TATCustomOnlyOneAppInst.Create(const AAppGlobalUniqueID: string; AOnAppCallback: TATAppCallback;
  AOnAppCheckProc: TATAppCheckProc);
begin
  inherited Create;
  FAppGlobalUniqueID := AAppGlobalUniqueID;
  FOnAppCallback := AOnAppCallback;
  FOnAppCheckProc := AOnAppCheckProc;
end;

procedure TATCustomOnlyOneAppInst.DoAppCallback(ANextPID: DWORD; const ANextParam: ATOOAIParamString);
begin
  if Assigned(FOnAppCallback) then
    try
      FOnAppCallback(ANextPID, ANextParam);
    except
      on E: Exception do
        ATShowException(E);
    end;
end;

procedure TATCustomOnlyOneAppInst.DoAppCheckProc(AIsAppRunning: Boolean; var ANeedNotify: Boolean);
begin
  if Assigned(FOnAppCheckProc) then
    try
      FOnAppCheckProc(AIsAppRunning, ANeedNotify);
    except
      on E: Exception do
        ATShowException(E);
    end;
end;

function TATCustomOnlyOneAppInst.GetPureParam: ATOOAIParamString;
var
  I, LParamCount: Integer;
  LStr: string;
begin
  LParamCount := ParamCount;
  if LParamCount > 0 then
  begin
    LStr := ParamStr(1);
    for I := 2 to ParamCount do
      LStr := LStr + ' ' + ParamStr(I);
    Result := DefaultStrToParamStr(LStr);
  end else
    Result := '';
end;

function TATCustomOnlyOneAppInst.ParamStrToBytes(const AStr: ATOOAIParamString): TBytes;
begin
  SetLength(Result, Length(AStr) * SizeOf(AStr[1]));
  if Length(Result) <> 0 then
    Move(Pointer(AStr)^, Result[Low(Result)], Length(Result));
end;

function TATCustomOnlyOneAppInst.ParamStrToDefaultStr(const AStr: ATOOAIParamString): string;
begin
  Result := AStr;
end;

function TATCustomOnlyOneAppInst.DefaultStrToParamStr(const AStr: string): ATOOAIParamString;
begin
  Result := AStr;
end;

{ TATWinOnlyOneAppInst }

constructor TATWinOnlyOneAppInst.Create(const AAppGlobalUniqueID: string;
  AOnAppCallback: TATAppCallback; AOnAppCheckProc: TATAppCheckProc);
var
  LErrorCode: DWORD;
begin
  inherited;

  { If the input global uniqueID is empty, create a default one. }
  if Trim(FAppGlobalUniqueID) = '' then
    FAppGlobalUniqueID := GenerateDefaultGlobalUniqueID;

  { Create mutex for multi app instances checking. }
  FAppMutex := CreateMutex(nil, False, PChar(FAppGlobalUniqueID));
  LErrorCode := GetLastError;
  if FAppMutex = 0 then
    raise Exception.CreateResFmt(@sCreateMutexError,
      [FAppGlobalUniqueID, SysErrorMessage(LErrorCode)]);

  DebugOutput('Mutex %u, name ''%s'' create successful. ', [FAppMutex, FAppGlobalUniqueID]);

  FIsAppRunning := (LErrorCode = ERROR_ALREADY_EXISTS);

  DebugOutput('App is running = %s.', [BoolToStr(FIsAppRunning, True)]);

  { Register a global msg id for broadcasting. }
  FAppMsgID := RegisterWindowMessage(PChar(FAppGlobalUniqueID));
  LErrorCode := GetLastError;
  if FAppMsgID = 0 then
    raise Exception.CreateResFmt(@sRegisterWindowMessageError,
      [FAppGlobalUniqueID, SysErrorMessage(LErrorCode)]);

  DebugOutput('RegisterWindowMessage ''%s'' successful.', [FAppGlobalUniqueID]);

  { Callback event and msg monitor only used in the first app. }
  if not FIsAppRunning then
  begin
    FAppMsgHandle := AllocateHWnd(InternalWndProc);
    if FAppMsgHandle = 0 then
      raise Exception.CreateResFmt(@sCreateHWndError, [SysErrorMessage(LErrorCode)]);
    DebugOutput('AllocateHWnd %u successful.', [FAppMsgHandle]);
  end;
end;

destructor TATWinOnlyOneAppInst.Destroy;
begin
  if FAppMsgHandle <> 0 then
  begin
    DeallocateHWnd(FAppMsgHandle);
    DebugOutput('DeallocateHWnd %u successful.', [FAppMsgHandle]);
    FAppMsgHandle := 0;
  end;

  if FAppMutex <> 0 then
  begin
    CloseHandle(FAppMutex);
    DebugOutput('Mutex %u Closed successful.', [FAppMutex]);
    FAppMutex := 0;
  end;

  inherited;
end;

procedure TATWinOnlyOneAppInst.InternalWndProc(var AMsg: TMessage);
begin
  if AMsg.Msg = FAppMsgID then
  begin
    try
      ProcessAppCallback(DWORD(AMsg.WParam), Integer(AMsg.LParam));
    except
      on E: Exception do
        ATShowException(E);
    end;
  end else
    AMsg.Result := DefWindowProc(FAppMsgHandle, AMsg.Msg, AMsg.WParam, AMsg.LParam);
end;

function TATWinOnlyOneAppInst.IsAppRunning: Boolean;
const
  CRecipients: DWORD = BSM_APPLICATIONS;
var
  LCurrentProcessId, LErrorCode: DWORD;
  LMemFileMappingHandle: THandle;
  LMemFileMappingName: string;
  LParamStr: ATOOAIParamString;
  LParamBytes: TBytes;
  LParamBytesSize: Integer;
  LMapViewOfFile: Pointer;
  LSendMsgResult: LRESULT;
  LBroadcastSystemMessageResult: LongInt;
{$IFNDEF DXE2AndUp}
  lpdwResult: DWORD;
{$ENDIF}
  LNeedNotify: Boolean;
begin
  Result := FIsAppRunning;

  { Call app check proc. }
  LNeedNotify := True;
  DoAppCheckProc(Result, LNeedNotify);

  { Appcalition not running, do nothing. }
  if not Result then
    Exit
  else
  begin
    { Appcalition is running. }
    if not LNeedNotify then
      Exit;
  end;

  if ParamCount = 0 then
  begin
    DebugOutput('No param found.');

    { Param is empty, post the registered msg to applications(except current app),
      the first app will has a notification. }
    LBroadcastSystemMessageResult := BroadcastSystemMessage(
      BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE, @CRecipients, FAppMsgID, 0, 0);

    if LBroadcastSystemMessageResult <> -1 then
      DebugOutput('BroadcastSystemMessage successful.')
    else
      DebugOutput('BroadcastSystemMessage failed, error: ''%s''.', [SysErrorMessage(GetLastError)]);
  end else
  begin
    { Param exists, we use "SendMessageTimeout" instead of "BroadcastSystemMessage",
      because it seems not support sync call. }

    { Make a mem file mapping name from the process id. }
    LCurrentProcessId := GetCurrentProcessId;
    LMemFileMappingName := GetGlobalMemFileMappingName(LCurrentProcessId);

    { Get param then convert to bytes. }
    LParamStr := GetPureParam;
    LParamBytes := ParamStrToBytes(LParamStr);
    LParamBytesSize := Length(LParamBytes);
    Assert(LParamBytesSize <> 0);

    DebugOutput('Param found ''%s''. ', [ParamStrToDefaultStr(LParamStr)]);

    { Create mem file mapping. }
    LMemFileMappingHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil,
      PAGE_READWRITE, 0, LParamBytesSize, PChar(LMemFileMappingName));
    LErrorCode := GetLastError;
    if LMemFileMappingHandle = 0 then
      raise Exception.CreateResFmt(@sCreateMemFileMappingFailed,
        [LMemFileMappingName, SysErrorMessage(LErrorCode)]);

    DebugOutput('CreateFileMapping ''%s'', size %u successful. ', [LMemFileMappingName, LParamBytesSize]);

    try
      { Try access the mem. }
      LMapViewOfFile := MapViewOfFile(LMemFileMappingHandle, FILE_MAP_WRITE, 0, 0, 0);
      if LMapViewOfFile = nil then
        raise Exception.CreateResFmt(@sAccessMemDataFailed, [SysErrorMessage(LErrorCode)]);

      try
        { Write param to mem. }
        Move(LParamBytes[Low(LParamBytes)], LMapViewOfFile^, LParamBytesSize);
        DebugOutput('MapViewOfFile write param successful, broadcasting message now...');

        { Sync broadcast message and waiting for the first app to query
          the param, we will exit normally if successed otherwise the first
          app may be busy or even hung, we should give up and exit immediately.

          SendMessageTimeout fuFlags (From docs.microsoft):
          SMTO_ABORTIFHUNG: The function returns without waiting for the time-out
                            period to elapse if the receiving thread appears to not
                            respond or "hangs."
          Note: After testing, if use other fuFlags(SMTO_BLOCK, SMTO_NORMAL...), the
                first app may receive message very lag under certain conditions,
                one case: TADOConnection created in a thread context(COM has been initialized).
        }
        LSendMsgResult := SendMessageTimeout(HWND_BROADCAST, FAppMsgID,
          WPARAM(LCurrentProcessId), LPARAM(LParamBytesSize), SMTO_ABORTIFHUNG, 10000,
          {$IFDEF DXE2AndUp}nil{$ELSE}lpdwResult{$ENDIF});

        { If the function succeeds, the return value is nonzero. }
        if LSendMsgResult <> 0 then
          DebugOutput('SendMessageTimeout successful.')
        else
          DebugOutput('SendMessageTimeout failed, error: ''%s''.', [SysErrorMessage(GetLastError)]);
      finally
        UnmapViewOfFile(LMapViewOfFile);
      end;
    finally
      CloseHandle(LMemFileMappingHandle);
    end;
  end;
end;

procedure TATWinOnlyOneAppInst.ProcessAppCallback(APID: DWORD; AParamSize: Integer);
var
  LMemFileMappingName: string;
  LParamStr: ATOOAIParamString;
  LMemFileMappingHandle: THandle;
  LMapViewOfFile: Pointer;
  LParamBytes: TBytes;
  LErrorCode: DWORD;
begin
  { Ignore if it is a self msg. }
  if APID = GetCurrentProcessId then
    Exit;

  { No param callback. }
  if AParamSize = 0 then
    DoAppCallback(APID, '')
  else
  begin
    { Try open the mem that created by the new app. }
    LMemFileMappingName := GetGlobalMemFileMappingName(APID);
    LMemFileMappingHandle := OpenFileMapping(FILE_MAP_READ, False,
      PChar(LMemFileMappingName));
    LErrorCode := GetLastError;
    if LMemFileMappingHandle = 0 then
      raise Exception.CreateResFmt(@sOpenMemFileMappingFailed,
        [LMemFileMappingName, SysErrorMessage(LErrorCode)]);
    DebugOutput('OpenFileMapping ''%s'' successful.', [LMemFileMappingName]);

    try
      { Try access the mem. }
      LMapViewOfFile := MapViewOfFile(LMemFileMappingHandle, FILE_MAP_READ, 0, 0, 0);
      LErrorCode := GetLastError;
      if LMapViewOfFile = nil then
        raise Exception.CreateResFmt(@sAccessMemDataFailed,
          [SysErrorMessage(LErrorCode)]);
      DebugOutput('MapViewOfFile ''%s'' with readmode successful.', [LMemFileMappingName]);

      try
        { Read param from mem. }
        SetLength(LParamBytes, AParamSize);
        Move(LMapViewOfFile^, LParamBytes[Low(LParamBytes)], AParamSize);
        LParamStr := BytesToParamStr(LParamBytes);
        DebugOutput('Read param ''%s'' successful.', [ParamStrToDefaultStr(LParamStr)]);
      finally
        UnmapViewOfFile(LMapViewOfFile);
      end;
    finally
      CloseHandle(LMemFileMappingHandle);
    end;

    { Execute the callback event. }
    DoAppCallback(APID, LParamStr);
  end;
end;

function OnlyOneAppInst(const AAppGlobalUniqueID: string; AOnNextAppCall: TATAppCallback; AOnAppCheckProc: TATAppCheckProc): IATOnlyOneAppInst;
begin
  Result := TATWinOnlyOneAppInst.Create(AAppGlobalUniqueID, AOnNextAppCall, AOnAppCheckProc) as IATOnlyOneAppInst;
end;

end.




