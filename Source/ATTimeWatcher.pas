{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATTimeWatcher                                             }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   A very lightweight stopwatch.                             }
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
 * The Original Code is ATTimeWatcher.
 * Unit owner : ZY (zylove619@hotmail.com) All rights reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

(*

  Change log:
  
  Version 1.001 by ZY:
    (2015.01.20) + First version created.

  Version 1.002 by ZY:
    (2016.11.29) + Add GetTickCount64 if Windows Vista or later.
                 + Add ElapsedMsFrom and ElapsedUsFrom.
*)

(* Useage:

  procedure DemoTimeWatcher;
  var
    LTimeWatcher: TATTimeWatcher;
  begin
    LTimeWatcher.Start;
    Sleep(A);
    // A
    Writeln(LTimeWatcher.Elapsed);

    // Restart
    LTimeWatcher.Start;
    Sleep(B);
    // B
    Writeln(LTimeWatcher.Elapsed);

    // Continue
    Sleep(C);
    // B + C
    Writeln(LTimeWatcher.Elapsed);

  end;

*)

unit ATTimeWatcher;

{$I AT.inc}

interface

const

  ATTimeWatcherVersion = '1.002';

type
  TATTimeWatcher = {$IFDEF D2006AndUp}record{$ELSE}object{$ENDIF}
  private
    FStartTimeStamp: Int64;
    function GetElapsedTicks: Int64;
    function GetElapsedMicroseconds: Extended;
    function GetElapsedMilliseconds: Int64;
    function GetElapsed: string;
  public
    procedure Start;
    function ElapsedMsFrom(ATimeStamp: Int64): Int64;
    function ElapsedUsFrom(ATimeStamp: Int64): Extended;
    function GetTimeStamp: Int64;

    property ElapsedTicks: Int64 read GetElapsedTicks;
    property ElapsedMicroseconds: Extended read GetElapsedMicroseconds;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
    { Elapsed milliseconds, retain 3 decimals e.g. 1.234 }
    property Elapsed: string read GetElapsed;
  end;

implementation

uses
  SysUtils
{$IFDEF MSWINDOWS}
  , Windows
{$ELSE}
  , Classes
{$ENDIF};

{ Internal global vars, initialized only once. }
var
  TWFormatSettings: TFormatSettings;
  IsHighResolution: Boolean;
  TickFrequency: Double;

{$IFDEF MSWINDOWS}
var
  GetTickCount64: function: Int64; stdcall = nil;

procedure GetProcGetTickCount64;
var
  LModule: HModule;
begin
  LModule := GetModuleHandle(kernel32);
  if LModule = 0 then
    RaiseLastOSError;
  @GetTickCount64 := GetProcAddress(LModule, 'GetTickCount64');
end;

function WinGetTickCount: Int64;
begin
  { Windows Vista or later, try use GetTickCount64. }
  if Assigned(GetTickCount64) then
    Result := GetTickCount64
  else
  { NOTE: Retrieves the number of milliseconds that have
          elapsed since the system was started, up to 49.7 days. }
    Result := GetTickCount;
end;
{$ENDIF}

procedure Init;
var
  LFrequency: Int64;
begin
  LFrequency := 1;
  
{$IFDEF DXEAndUp}
  TWFormatSettings := TFormatSettings.Create;
{$ELSE}
  GetLocaleFormatSettings(SysLocale.DefaultLCID, TWFormatSettings);
{$ENDIF}

{$IFDEF MSWINDOWS}
  GetProcGetTickCount64;
{$ENDIF}

{$IFDEF MSWINDOWS}
  IsHighResolution := QueryPerformanceFrequency(LFrequency);
{$ELSE}
  IsHighResolution := False;
{$ENDIF}

  if IsHighResolution then
    TickFrequency := 1000000 / LFrequency
  else
    TickFrequency := 1000.0;
end;

{ TATTimeWatcher }

function TATTimeWatcher.GetElapsedMilliseconds: Int64;
begin
  Result := ElapsedMsFrom(FStartTimeStamp);
end;

function TATTimeWatcher.ElapsedMsFrom(ATimeStamp: Int64): Int64;
begin
  Result := Round(ElapsedUsFrom(ATimeStamp) * 0.001);
end;

function TATTimeWatcher.ElapsedUsFrom(ATimeStamp: Int64): Extended;
var
  LTimeStamp: Int64;
begin
  if ATimeStamp >= 0 then
  begin
    LTimeStamp := GetTimeStamp;
    if LTimeStamp >= ATimeStamp then
      Result := (LTimeStamp - ATimeStamp) * TickFrequency
    else
      Result := 0;
  end else
    Result := 0;
end;

function TATTimeWatcher.GetElapsed: string;
begin
  Result := FormatFloat('0.000', ElapsedMicroseconds * 0.001, TWFormatSettings);
end;

function TATTimeWatcher.GetElapsedTicks: Int64;
begin
  Result := GetTimeStamp - FStartTimeStamp;
end;

function TATTimeWatcher.GetElapsedMicroseconds: Extended;
begin
  Result := ElapsedUsFrom(FStartTimeStamp);
end;

function TATTimeWatcher.GetTimeStamp: Int64;
begin
{$IFDEF MSWINDOWS}
  if IsHighResolution then
    QueryPerformanceCounter(Result)
  else
    Result := WinGetTickCount;
{$ELSE}
  Result := TThread.GetTickCount;
{$ENDIF}
end;

procedure TATTimeWatcher.Start;
begin
  FStartTimeStamp := GetTimeStamp;
end;

initialization
  Init;
end.
