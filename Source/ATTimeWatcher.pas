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

  ATTimeWatcherVersion = '1.001';

type
  TATTimeWatcher = {$IFDEF D2006AndUp}record{$ELSE}object{$ENDIF}
  private
    FIsInitialized: string;
    FFrequency: Int64;
    FStartTimeStamp: Int64;
    FIsHighResolution: Boolean;
    FTickFrequency: Double;
    procedure Init;
    function GetTimeStamp: Int64;
    function GetElapsedTicks: Int64;

    function GetElapsedMicroseconds: Extended;
    function GetElapsedMilliseconds: Int64;
    function GetElapsed: string;
  public
    procedure Start;
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

var
  TWFormatSettings: TFormatSettings;

{ TATTimeWatcher }

function TATTimeWatcher.GetElapsedMilliseconds: Int64;
begin
  Result := Round(ElapsedMicroseconds * 0.001);
end;

function TATTimeWatcher.GetElapsed: string;
begin
  Result := Format('%.3f', [ElapsedMicroseconds * 0.001], TWFormatSettings);
end;

function TATTimeWatcher.GetElapsedTicks: Int64;
begin
  Result := GetTimeStamp - FStartTimeStamp;
end; 

function TATTimeWatcher.GetElapsedMicroseconds: Extended;
begin
  Result := ElapsedTicks * FTickFrequency;
end;

function TATTimeWatcher.GetTimeStamp: Int64;
begin
{$IFDEF MSWINDOWS}
  if FIsHighResolution then
    QueryPerformanceCounter(Result)
  else
    { NOTE: Retrieves the number of milliseconds that have
            elapsed since the system was started, up to 49.7 days. }
    Result := GetTickCount;
{$ELSE}
  Result := TThread.GetTickCount;
{$ENDIF}
end;

procedure TATTimeWatcher.Init;
begin
  if FIsInitialized = '' then
  begin
  {$IFDEF MSWINDOWS}
    FIsHighResolution := QueryPerformanceFrequency(FFrequency);
  {$ELSE}
    FIsHighResolution := False;
  {$ENDIF}
    if FIsHighResolution then
      FTickFrequency := 1000000 / FFrequency
    else
      FTickFrequency := 1000.0;
      
    FIsInitialized := 'Yes';
  end;
end;

procedure TATTimeWatcher.Start;
begin
  Init;
  FStartTimeStamp := GetTimeStamp;
end;

initialization
{$IFDEF DXEAndUp}
  TWFormatSettings := TFormatSettings.Create;
{$ELSE}
  GetLocaleFormatSettings(SysLocale.DefaultLCID, TWFormatSettings);
{$ENDIF}

end.
