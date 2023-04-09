{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATCommon.pas                                              }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   Common types, classes and etc.                            }
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

unit ATCommon;

{$I AT.inc}

interface

uses
  SysUtils
{$IFDEF USE_DELPHI}
  {$IFDEF HAS_TINTERLOCKED}
  , SyncObjs
  {$ELSE}
  , Windows
  {$ENDIF HAS_TINTERLOCKED}
{$ENDIF USE_DELPHI}
  ;

type

  { Types }

{$IFDEF USE_DELPHI}
  {$IFDEF D2010AndUp}
  ATNativeInt  = NativeInt;
  ATNativeUInt = NativeUInt;
  {$ELSE}
  ATNativeInt  = Integer;
  ATNativeUInt = Cardinal;
  {$ENDIF}
{$ELSE}
  ATNativeInt  = NativeInt;
  ATNativeUInt = NativeUInt;
{$ENDIF USE_DELPHI}

  ATIntPtr     = ATNativeInt;
  ATUIntPtr    = ATNativeUInt;

  { Exceptions }

  EATException      = class(Exception);
  EATNotImplemented = class(EATException);

  { Classes }

  /// <summary> Atomic functions. </summary>
  TATAtomic = class
    class function Inc(var ATarget: Integer) : Integer; {$IFDEF HAS_CLASS_STATIC_METHODS}static;{$ENDIF}
    class function Dec(var ATarget: Integer) : Integer; {$IFDEF HAS_CLASS_STATIC_METHODS}static;{$ENDIF}
    class function Read(var ATarget: Integer): Integer; {$IFDEF HAS_CLASS_STATIC_METHODS}static;{$ENDIF}
    class function CompareExchange(var ATarget: Pointer; AValue, AComparand: Pointer): Pointer; {$IFDEF HAS_CLASS_STATIC_METHODS}static;{$ENDIF}
  end;

implementation

{ TRHAtomic }

class function TATAtomic.Inc(var ATarget: Integer): Integer;
begin
{$IFDEF USE_DELPHI}
  {$IFDEF DXEAndUp}
  Result := TInterlocked.Increment(ATarget);
  {$ELSE}
  Result := InterlockedIncrement(ATarget);
  {$ENDIF}
{$ELSE}
  Result := System.InterlockedIncrement(ATarget);
{$ENDIF}
end;

class function TATAtomic.CompareExchange(var ATarget: Pointer; AValue, AComparand: Pointer): Pointer;
begin
{$IFDEF USE_DELPHI}
  {$IFDEF DXEAndUp}
  Result := TInterlocked.CompareExchange(ATarget, AValue, AComparand);
  {$ELSE}
  Result := Pointer(InterlockedCompareExchange(Integer(ATarget), Integer(AValue), Integer(AComparand)));
  {$ENDIF}
{$ELSE}
  {$IFDEF CPU_IS_32BITS}
  Result := {%H-}Pointer(System.InterlockedCompareExchange({%H-}cardinal(ATarget), {%H-}cardinal(AValue), {%H-}cardinal(AComparand)));
  {$ELSE}
  Result := {%H-}Pointer(System.InterlockedCompareExchange64({%H-}qword(ATarget), {%H-}qword(AValue), {%H-}qword(AComparand)));
  {$ENDIF}
{$ENDIF}
end;

class function TATAtomic.Dec(var ATarget: Integer): Integer;
begin
{$IFDEF USE_DELPHI}
  {$IFDEF DXEAndUp}
  Result := TInterlocked.Decrement(ATarget);
  {$ELSE}
  Result := InterlockedDecrement(ATarget);
  {$ENDIF}
{$ELSE}
  Result := System.InterlockedDecrement(ATarget);
{$ENDIF}
end;

class function TATAtomic.Read(var ATarget: Integer): Integer;
begin
{$IFDEF USE_DELPHI}
  {$IFDEF DXEAndUp}
  Result := TInterlocked.CompareExchange(ATarget, 0, 0);
  {$ELSE}
  Result := InterlockedExchangeAdd(@ATarget, 0);
  {$ENDIF}
{$ELSE}
  Result := System.InterlockedExchangeAdd(ATarget, 0);
{$ENDIF}
end;

end.
