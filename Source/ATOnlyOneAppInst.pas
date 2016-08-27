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

*) 

unit ATOnlyOneAppInst;

{$I AT.inc}

interface

{$IFDEF MSWINDOWS}

uses
  Forms;

const

  ATOnlyOneAppInstVersion = '1.001';  

type

{$IFDEF HAS_ANONYMOUSMETHOD}
  TATAppCallback = reference to procedure;
{$ELSE}
  TATAppCallback = procedure;
{$ENDIF}

  IATOnlyOneAppInst = interface
    ['{B1FBB24B-F793-48E3-88C2-D9C4E2B7262A}']
    function IsAppRunning: Boolean;
  end;

function OnlyOneAppInst(const AAppGlobalUniqueID: string = '';
  AOnPreviousAppCall: TATAppCallback = nil): IATOnlyOneAppInst;

procedure DefaultActiveApp(const ATips: string = ''; const ACaption: string = '';
  ANeedRestoreApp: Boolean = True; AFormNeededBringToTop: TCustomForm = nil);
procedure DefaultOnPreviousAppCall;

{$ELSE}
  {$MESSAGE ERROR 'ATOnlyOneAppInst currently only supported in MS WINDOWS.'}
{$ENDIF MSWINDOWS}

implementation

{$IFDEF MSWINDOWS}

uses
  Windows, Messages, SysUtils, Classes;

resourcestring
  sCreateMutexError           = '[%s] Create mutex ''%s'' failed, error msg: %s.';
  sRegisterWindowMessageError = '[%s] Register window message ''%s'' failed, error msg: %s.';
  sAppAlreadyRunning          = '''%s'' is already running.';

procedure DefaultActiveApp(const ATips: string; const ACaption: string;
  ANeedRestoreApp: Boolean; AFormNeededBringToTop: TCustomForm);

  procedure ShowTips;
  var
    LCaption: string;
  begin
    if Trim(ATips) <> '' then
    begin
      LCaption := ACaption;
      if Trim(LCaption) = '' then
        LCaption := Application.Title;

      MessageBox(0, PChar(ATips), PChar(LCaption), MB_OK + MB_ICONINFORMATION + MB_TOPMOST);
    end;
  end;

  function TryBringWindowToTop(AWindow: HWND): Boolean;
  var
    LInput: TInput;
  begin
    FillChar(LInput, SizeOf(LInput), 0);
    SendInput(1, LInput, SizeOf(LInput));
    Result := SetForegroundWindow(AWindow);
  end;

  procedure ActiveForm;
  begin
    if Assigned(AFormNeededBringToTop) then
    begin
      TForm(AFormNeededBringToTop).Visible := True;

      if AFormNeededBringToTop.WindowState = wsMinimized then
        AFormNeededBringToTop.WindowState := wsNormal;

      TryBringWindowToTop(AFormNeededBringToTop.Handle);
    end;
  end;

begin
  ShowTips;

  if ANeedRestoreApp then
    Application.Restore;

  ActiveForm;
end;

function GetPureAppName: string;
begin
  Result := ChangeFileExt(ExtractFileName(ParamStr(0)), '');
end;

procedure DefaultOnPreviousAppCall;
begin
  DefaultActiveApp(Format(sAppAlreadyRunning, [GetPureAppName]), '', True,
    Application.MainForm);
end;

type
  TATOnlyOneAppInst = class(TInterfacedObject, IATOnlyOneAppInst)
  private
    FAppMutex: THandle;
    FMsgHandle: HWND;
    FGlobalMsg: UINT;
    FIsAppRunning: Boolean;
    FOnAppCallback: TATAppCallback;
    procedure WndProc(var AMsg: TMessage); 
  public
    constructor Create(AAppGlobalUniqueID: string; AOnAppCallback: TATAppCallback);
    destructor Destroy; override;
    { IATOnlyOneAppInst }
    function IsAppRunning: Boolean;
  end;

{ TATOnlyOneAppInst }

constructor TATOnlyOneAppInst.Create(AAppGlobalUniqueID: string;
  AOnAppCallback: TATAppCallback);

  function GenerateDefaultGlobalUniqueID: string;
  begin
    Result := Format('{1130D592-9727-417F-BB65-5C38FEA63B96}_$%s$', [GetPureAppName]);
  end;
    
var
  LErrorCode: DWORD;
begin
  inherited Create;

  if Trim(AAppGlobalUniqueID) = '' then
    AAppGlobalUniqueID := GenerateDefaultGlobalUniqueID;

  { Use mutex for multi app instances checking. }
  FAppMutex := CreateMutex(nil, False, PChar(AAppGlobalUniqueID));
  LErrorCode := GetLastError;
  if FAppMutex = 0 then
    raise Exception.CreateResFmt(@sCreateMutexError,
      [ClassName, AAppGlobalUniqueID, SysErrorMessage(LErrorCode)]);

  FIsAppRunning := (LErrorCode = ERROR_ALREADY_EXISTS);

  { Register a global msg id for broadcasting. }
  FGlobalMsg := RegisterWindowMessage(PChar(AAppGlobalUniqueID));
  LErrorCode := GetLastError;
  if FGlobalMsg = 0 then
    raise Exception.CreateResFmt(@sRegisterWindowMessageError,
      [ClassName, AAppGlobalUniqueID, SysErrorMessage(LErrorCode)]);

  { Callback event and msg monitor only used in the previous app. }
  if not FIsAppRunning then
  begin
    FOnAppCallback := AOnAppCallback;
    FMsgHandle := AllocateHWnd(WndProc);
  end;
end;

destructor TATOnlyOneAppInst.Destroy;
begin
  if FMsgHandle <> 0 then
  begin
    DeallocateHWnd(FMsgHandle);
    FMsgHandle := 0;
  end;

  if FAppMutex <> 0 then
  begin
    CloseHandle(FAppMutex);
    FAppMutex := 0;
  end;
  inherited;
end;

procedure TATOnlyOneAppInst.WndProc(var AMsg: TMessage);
begin
  if (AMsg.Msg = FGlobalMsg) and Assigned(FOnAppCallback) then
  begin
    try
      FOnAppCallback();
    except
      Application.HandleException(Self);
    end;
  end else
    AMsg.Result := DefWindowProc(FMsgHandle, AMsg.Msg, AMsg.WParam, AMsg.LParam);
end;

function TATOnlyOneAppInst.IsAppRunning: Boolean;
const
  CRecipients: DWORD = BSM_APPLICATIONS;
begin
  Result := FIsAppRunning;

  if Result then
  { Post the registered msg to applications(except current app),
    so the previous app will has a change to receive the msg. }
    BroadcastSystemMessage(BSF_IGNORECURRENTTASK or BSF_POSTMESSAGE, @CRecipients,
      FGlobalMsg, 0, 0);
end;

function OnlyOneAppInst(const AAppGlobalUniqueID: string; AOnPreviousAppCall: TATAppCallback): IATOnlyOneAppInst;
begin
  Result := TATOnlyOneAppInst.Create(AAppGlobalUniqueID, AOnPreviousAppCall);
end;

{$ENDIF MSWINDOWS}

end.




