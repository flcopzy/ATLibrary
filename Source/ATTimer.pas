{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATTimer.pas                                               }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   Support a high-precision timer in MSWindows               }
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
 * The Original Code is ATTimer.
 * Unit owner : ZY (zylove619@hotmail.com) All rights reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

(*
  Change log:
  
  Version 1.001 by ZY:
    (2016.06.18) + First version created.

  Version 1.002 by ZY:
    (2021.07.10) * fixed a compile error when use default
                   TTimer on other platforms.

*)

unit ATTimer;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 25.0}
    {$LEGACYIFEND ON}
  {$IFEND}
{$ENDIF}

{$I AT.inc}

interface

uses

{$IFDEF MSWINDOWS}
  Windows, Messages, {$IFDEF HAS_UNIT_SCOPE}Vcl.Forms{$ELSE}Forms{$ENDIF}
  , SysUtils, Classes, MMSystem
{$ELSE}
  FMX.Types
{$ENDIF};

const
  ATTimerVersion = '1.002';

{$IFDEF MSWINDOWS}  
  ATTIMER_DEFAULT_INTERVAL = 1000 {ms};
{$ENDIF}

type

{$IFDEF MSWINDOWS}
  TATTimer = class(TComponent)
  private
    FMsgHandle: HWND;
    FEnabled: Boolean;
    FSysPeriodMin: UINT;
    FSysPeriodMax: UINT;
    FTimerID     : UINT;
    FInterval    : UINT;
    FResolution  : UINT;
    FOnTimer: TNotifyEvent;
    FTimeBeginPeriodSuccessful: Boolean;
    procedure SetEnabled(const AValue: Boolean);
    procedure SetInterval(AValue: UINT);
    procedure SetResolution(AValue: UINT);
    procedure SetOnTimer(const AValue: TNotifyEvent);
    procedure CheckIfRaiseMMResult(const ARoutine: string; AMMResult: MMRESULT);
    procedure TimerWndProc(var AMsg: TMessage);
  protected
    procedure Timer; dynamic;
    procedure KillTimer; virtual;
    procedure UpdateTimer; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOnTimer: TNotifyEvent; AInterval: UINT = ATTIMER_DEFAULT_INTERVAL;
      AEnabled: Boolean = True; AResolution: UINT = 0);
    destructor Destroy; override;
    /// <summary> System minimum period supported. </summary>
    property SysPeriodMin: UINT read FSysPeriodMin;
    /// <summary> System maximum period supported. </summary>
    property SysPeriodMax: UINT read FSysPeriodMax;
    /// <summary> Resolution of the timer event, in milliseconds. The resolution increases
    ///  with smaller values; a resolution of 0 indicates periodic events should
    ///  occur with the greatest possible accuracy. To reduce system overhead,
    ///  however, you should use the maximum value appropriate for your application.
    /// </summary>
    property Resolution: UINT read FResolution write SetResolution;    
  published
    /// <summary> Controls whether the timer generates OnTimer events periodically. </summary>
    property Enabled: Boolean read FEnabled write SetEnabled default True; 
    /// <summary> Event delay, in milliseconds. </summary>
    property Interval: UINT read FInterval write SetInterval default ATTIMER_DEFAULT_INTERVAL;
    /// <summary> Occurs when a specified amount of time, determined by the Interval property, has passed. </summary>
    property OnTimer: TNotifyEvent read FOnTimer write SetOnTimer;
  end;
{$ELSE}
  TATTimer = TTimer;
{$ENDIF}

implementation

{$IFDEF MSWINDOWS}

resourcestring

  sNoMultimediaTimers = 'Not enough multimedia timers available.';
  sCallProcFailed     = '%s call ''%s'' failed, error code: %u.';

const

{$IF not declared(TIME_KILL_SYNCHRONOUS)} { in MMSystem.pas }
  { This flag prevents the event from occurring
    after the user calls timeKillEvent() to destroy it.}
  TIME_KILL_SYNCHRONOUS = $0100;
{$IFEND}

  WM_ATTIMER_MSGID = WM_USER + 1000;

constructor TATTimer.Create(AOwner: TComponent);
var
  LTimeCaps: TTimeCaps;
begin
  inherited;

  { Try query the timer device to determine its resolution. }
  CheckIfRaiseMMResult('timeGetDevCaps', timeGetDevCaps(@LTimeCaps, SizeOf(LTimeCaps)));
  FSysPeriodMin := LTimeCaps.wPeriodMin;
  FSysPeriodMax := LTimeCaps.wPeriodMax;

  { Try set the best period.
    Note: 1. It must match each call to timeBeginPeriod with a call
             to timeEndPeriod, specifying the same minimum resolution
             in both calls.

          2. Starting with Windows 10, version 2004, this function
             no longer affects global timer resolution, see more:
             https://docs.microsoft.com/en-us/windows/win32/api/timeapi/nf-timeapi-timebeginperiod
  }
  CheckIfRaiseMMResult('timeBeginPeriod', timeBeginPeriod(FSysPeriodMin));
  FTimeBeginPeriodSuccessful := True;

  { Set deault properties, same as TTimer. }
  FEnabled   := True;
  FInterval  := ATTIMER_DEFAULT_INTERVAL;
  
  { Create the timer msg listener. }
  FMsgHandle := AllocateHWnd(TimerWndProc);
end;

constructor TATTimer.CreateNew(AOnTimer: TNotifyEvent; AInterval: UINT;
  AEnabled: Boolean; AResolution: UINT);
begin
  Create(nil);
  FInterval  := AInterval;
  Resolution := AResolution;
  FEnabled   := AEnabled;
  OnTimer    := AOnTimer;
end;

destructor TATTimer.Destroy;
begin
  FEnabled := False;
  UpdateTimer;

  if FMsgHandle <> 0 then
  begin
    DeallocateHWnd(FMsgHandle);
    FMsgHandle := 0;
  end;

  if FTimeBeginPeriodSuccessful then
    CheckIfRaiseMMResult('timeEndPeriod', timeEndPeriod(FSysPeriodMin));

  inherited;
end;

type

{$IFDEF D2010AndUp}
  ATUIntPtr = DWORD_PTR;
{$ELSE}
  ATUIntPtr = DWORD;
{$ENDIF}  

procedure InternalTimerCallback(uTimerID, uMessage: UINT;
  dwUser, dw1, dw2: ATUIntPtr) stdcall;
begin
  { Note: 1. The callback is executed on a separate thread,
             so do not access any VCL/FMX objects directly.

          2. Applications should not call any system-defined
             functions from inside a callback function, except
             for PostMessage, timeGetSystemTime, timeGetTime,
             timeSetEvent, timeKillEvent, midiOutShortMsg,
             midiOutLongMsg, and OutputDebugString.

          3. The dwUser here is NOT an object(TATTimer),
             just pass the msg handle and call PostMessage
             directly. }

  PostMessage(HWND(dwUser), WM_ATTIMER_MSGID, 0, 0);
end;

procedure TATTimer.SetEnabled(const AValue: Boolean);
begin
  if FEnabled <> AValue then
  begin
    FEnabled := AValue;
    UpdateTimer;
  end;
end;

procedure TATTimer.Timer;
begin
  if FEnabled and Assigned(FOnTimer) then
    FOnTimer(Self);
end;

procedure TATTimer.TimerWndProc(var AMsg: TMessage);
begin
  if AMsg.Msg = WM_ATTIMER_MSGID then
    try
      Timer;
    except
      Application.HandleException(Self);
    end
  else
    AMsg.Result := DefWindowProc(FMsgHandle, AMsg.Msg, AMsg.WParam, AMsg.LParam);
end;

procedure TATTimer.SetInterval(AValue: UINT);
begin
  if FInterval <> AValue then
  begin
    FInterval := AValue;
    UpdateTimer;
  end;
end;

procedure TATTimer.SetResolution(AValue: UINT);
begin
  if AValue > FSysPeriodMax then
    AValue := FSysPeriodMax;

  if FResolution <> AValue then
  begin
    FResolution := AValue;
    UpdateTimer;
  end;
end;

procedure TATTimer.SetOnTimer(const AValue: TNotifyEvent);
begin
  if not CompareMem(@@FOnTimer, @@AValue, SizeOf(TMethod)) then
  begin
    FOnTimer := AValue;
    UpdateTimer;
  end;
end;

procedure TATTimer.CheckIfRaiseMMResult(const ARoutine: string;
  AMMResult: MMRESULT);
begin
  if AMMResult <> TIMERR_NOERROR then
    raise Exception.CreateResFmt(@sCallProcFailed, [Name, ARoutine, AMMResult]);
end;

procedure TATTimer.KillTimer;
begin
  if FTimerID <> 0 then
  begin
    CheckIfRaiseMMResult('timeKillEvent', timeKillEvent(FTimerID));
    FTimerID := 0;
  end;
end;

procedure TATTimer.UpdateTimer;
begin
  KillTimer;

  if FEnabled and (FInterval > 0) and Assigned(FOnTimer) then
  begin
    FTimerID := timeSetEvent(FInterval, FResolution, @InternalTimerCallback,
      FMsgHandle, TIME_PERIODIC or TIME_KILL_SYNCHRONOUS);
    if FTimerID = 0 then
      raise EOutOfResources.CreateRes(@sNoMultimediaTimers);
  end;
end;
{$ENDIF MSWINDOWS TATTimer}

end.
