{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATOnlyOneAppInst.pas                                      }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   Ensure only a single instance of the application runs.    }
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
*) 

unit ATOnlyOneAppInst;

{$I AT.inc}

interface

{$IFNDEF MSWINDOWS}
  {$MESSAGE ERROR 'ATOnlyOneAppInst currently only supported in MS WINDOWS.'}
{$ENDIF}

const

  ATOnlyOneAppInstVersion = '1.002';

type

  /// <summary> Application callback event. </summary>
  /// <param name="ANextPID"> The PID of the next app, zero if no param passed. </param>
  /// <param name="ANextParam"> The param of the next app, empty if no param passed. </param>
  TATAppCallback = {$IFDEF HAS_ANONYMOUSMETHOD}reference to{$ENDIF}
                   procedure(ANextPID: UInt64; const ANextParam: string);

  /// <summary> The only one application instance interface. </summary>
  IATOnlyOneAppInst = interface
    ['{B1FBB24B-F793-48E3-88C2-D9C4E2B7262A}']
    /// <summary> Check if an application is already running. </summary>
    function IsAppRunning: Boolean;
  end;

/// <summary> Get the only one application instance interface. </summary>
/// <param name="AAppGlobalUniqueID"> App global unique id, if empty then a default id will be used. </param>
/// <param name="AOnNextAppCall"> App callback event. </param>
/// <returns> Return an interface(IATOnlyOneAppInst) </returns>
///
/// <remarks> NOTE 1. AppGlobalUniqueID is a global unique id, it is best to use
///                   a guid combination string and should not already used by
///                   other objects, like Event, Mutex etc.
///
///                2. Usually it only needs to be used once in dpr file, and do
///                   not used in thread, it is meaningless.
///
///                3. On MSWindows, it's not supported if your application
///                   is not a GUI application(required message loop).
///
///                4. if param is not empty, the app callback event should not
///                   do heavy work, because the next application will wait
///                   for a while until it times out.
///
/// </remarks>
function OnlyOneAppInst(const AAppGlobalUniqueID: string = '';
  AOnNextAppCall: TATAppCallback = nil): IATOnlyOneAppInst;

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

  function GetPureAppName: string;
  begin
    Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
  end;

begin
  Result := Format('{1130D592-9727-417F-BB65-5C38FEA63B96}_ATOnlyOneAppInst_Def_%s', [GetPureAppName]);
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
  protected
    function GetPureParam: string; virtual;
    function StrToBytes(const AStr: string): TBytes; virtual; abstract;
    function BytesToStr(AStrBytes: TBytes): string; virtual; abstract;
    { IATOnlyOneAppInst }
    function IsAppRunning: Boolean; virtual; abstract;
  public
    constructor Create(const AAppGlobalUniqueID: string; AOnAppCallback: TATAppCallback); virtual;
  end;

  TATWinOnlyOneAppInst = class(TATCustomOnlyOneAppInst)
  private
    FAppMutex: THandle;
    FAppMsgHandle: HWND;
    FAppMsgID: UINT;
    procedure InternalWndProc(var AMsg: TMessage);
    procedure DoAppCallback(ANextPID: DWORD; const ANextParam: string);
    procedure ProcessAppCallback(APID: DWORD; AParamSize: Integer);
  protected
    function StrToBytes(const AStr: string): TBytes; override;
    function BytesToStr(AStrBytes: TBytes): string; override;
    function IsAppRunning: Boolean; override;
  public
    constructor Create(const AAppGlobalUniqueID: string; AOnAppCallback: TATAppCallback); override;
    destructor Destroy; override;
  end;

{ TATCustomOnlyOneAppInst }

constructor TATCustomOnlyOneAppInst.Create(const AAppGlobalUniqueID: string; AOnAppCallback: TATAppCallback);
begin
  inherited Create;
  FAppGlobalUniqueID := AAppGlobalUniqueID;
  FOnAppCallback := AOnAppCallback;
end;

function TATCustomOnlyOneAppInst.GetPureParam: string;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to ParamCount do
    Result := Result + ParamStr(I) + ' ';
  Result := Trim(Result);
end;

{ TATOnlyOneAppInst }

function TATWinOnlyOneAppInst.BytesToStr(AStrBytes: TBytes): string;
var
  LWideStr: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
begin
  if Length(AStrBytes) > 0 then
    SetString(LWideStr, PWideChar(@AStrBytes[Low(AStrBytes)]),
      Length(AStrBytes) div (SizeOf(LWideStr[1])))
  else
    LWideStr := '';
  Result := LWideStr;
end;

constructor TATWinOnlyOneAppInst.Create(const AAppGlobalUniqueID: string;
  AOnAppCallback: TATAppCallback);
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

  { Callback event and msg monitor only used in the previous app. }
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

procedure TATWinOnlyOneAppInst.DoAppCallback(ANextPID: DWORD; const ANextParam: string);
begin
  if Assigned(FOnAppCallback) then
    FOnAppCallback(ANextPID, ANextParam);
end;

procedure TATWinOnlyOneAppInst.InternalWndProc(var AMsg: TMessage);
begin
  if AMsg.Msg = FAppMsgID then
  begin
    try
      ProcessAppCallback(DWORD(AMsg.WParam), Integer(AMsg.LParam));
    except
      on E: Exception do
        MessageBox(0, PChar(E.Message), nil, MB_OK + MB_ICONERROR + MB_TOPMOST);
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
  LMemFileMappingName, LParamStr: string;
  LParamBytes: TBytes;
  LParamBytesSize: Integer;
  LMapViewOfFile: Pointer;
  LSendMsgResult: LRESULT;
  LBroadcastSystemMessageResult: LongInt;
{$IFNDEF DXE2AndUp}
  lpdwResult: DWORD;
{$ENDIF}
begin
  Result := FIsAppRunning;

  { No previous app exists, do nothing. }
  if not Result then
    Exit;

  if ParamCount = 0 then
  begin
    DebugOutput('No Param found.');

    { Param is empty, post the registered msg to applications(except current app),
      the previous app will has a notification. }
    LBroadcastSystemMessageResult := BroadcastSystemMessage(
      BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE, @CRecipients, FAppMsgID, 0, 0);

    if LBroadcastSystemMessageResult <> -1 then
      DebugOutput('BroadcastSystemMessage successful.')
    else
      DebugOutput('BroadcastSystemMessage failed, error: ''%s''.', [SysErrorMessage(GetLastError)]);
  end else
  begin
    { Param is not empty, it seems that BroadcastSystemMessage not
      support sync call, so we need do sync send message. }

    LCurrentProcessId := GetCurrentProcessId;
    LMemFileMappingName := GetGlobalMemFileMappingName(LCurrentProcessId);

    { Get param and convert to bytes. }
    LParamStr := GetPureParam;
    LParamBytes := StrToBytes(LParamStr);
    LParamBytesSize := Length(LParamBytes);
    Assert(LParamBytesSize <> 0);

    DebugOutput('Param found ''%s''. ', [LParamStr]);

    { Create mem file mapping. }
    LMemFileMappingHandle := CreateFileMapping(INVALID_HANDLE_VALUE, nil,
      PAGE_READWRITE, 0, LParamBytesSize, PChar(LMemFileMappingName));
    LErrorCode := GetLastError;
    if LMemFileMappingHandle = 0 then
      raise Exception.CreateResFmt(@sCreateMemFileMappingFailed,
        [LMemFileMappingName, SysErrorMessage(LErrorCode)]);

    DebugOutput('CreateFileMapping ''%s'', size %u successful. ', [LMemFileMappingName, LParamBytesSize]);

    try
      { Maybe we're the third app and the second app is still waiting,
        we should exit immediately. }
      if LErrorCode = ERROR_ALREADY_EXISTS then
        Exit;

      { Try access the mem. }
      LMapViewOfFile := MapViewOfFile(LMemFileMappingHandle, FILE_MAP_WRITE, 0, 0, 0);
      if LMapViewOfFile = nil then
        raise Exception.CreateResFmt(@sAccessMemDataFailed, [SysErrorMessage(LErrorCode)]);

      try
        { Write param to mem. }
        Move(LParamBytes[Low(LParamBytes)], LMapViewOfFile^, LParamBytesSize);
        DebugOutput('MapViewOfFile write param successful, broadcasting message now...');

        { Sync broadcast message and waiting for the previous app to query
          the param, we will exit normally if successed otherwise previous
          app may be busy or even hung, we should give up and exit immediately. }
        LSendMsgResult := SendMessageTimeout(HWND_BROADCAST, FAppMsgID,
          WPARAM(LCurrentProcessId), LPARAM(LParamBytesSize), SMTO_NORMAL, 10000,
          {$IFDEF DXE2AndUp}nil{$ELSE}lpdwResult{$ENDIF});

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
  LMemFileMappingName, LParamStr: string;
  LMemFileMappingHandle: THandle;
  LMapViewOfFile: Pointer;
  LParamBytes: TBytes;
  LErrorCode: DWORD;
begin
  { Ignore if it is a self msg. }
  if APID = GetCurrentProcessId then
    Exit;

  { No param callback. }
  if APID = 0 then
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
      LMapViewOfFile := MapViewOfFile(LMemFileMappingHandle, FILE_MAP_READ, 0, 0, 0);
      LErrorCode := GetLastError;
      if LMapViewOfFile = nil then
        raise Exception.CreateResFmt(@sAccessMemDataFailed,
          [SysErrorMessage(LErrorCode)]);
      DebugOutput('MapViewOfFile ''%s'' with readmode successful.', [LMemFileMappingName]);

      try
        SetLength(LParamBytes, AParamSize);
        Move(LMapViewOfFile^, LParamBytes[Low(LParamBytes)], AParamSize);
        LParamStr := BytesToStr(LParamBytes);
        DebugOutput('Read param ''%s'' successful.', [LParamStr]);
        DoAppCallback(APID, LParamStr);
      finally
        UnmapViewOfFile(LMapViewOfFile);
      end;
    finally
      CloseHandle(LMemFileMappingHandle);
    end;
  end;
end;

function TATWinOnlyOneAppInst.StrToBytes(const AStr: string): TBytes;
var
  LWideStr: {$IFDEF UNICODE}string{$ELSE}WideString{$ENDIF};
begin
  { In windows, use default wide string. }
  LWideStr := AStr;
  SetLength(Result, Length(LWideStr) * SizeOf(LWideStr[1]));
  if Length(Result) <> 0 then
    Move(Pointer(LWideStr)^, Result[Low(Result)], Length(Result));
end;

function OnlyOneAppInst(const AAppGlobalUniqueID: string; AOnNextAppCall: TATAppCallback): IATOnlyOneAppInst;
begin
  Result := TATWinOnlyOneAppInst.Create(AAppGlobalUniqueID, AOnNextAppCall) as IATOnlyOneAppInst;
end;

end.




