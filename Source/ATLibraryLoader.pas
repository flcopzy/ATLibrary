{ *************************************************************************** }
{                          Delphi Auxiliary Toolkit                           }
{                                                                             }
{   ModuleName  :   ATLibraryLoader                                           }
{   Author      :   ZY                                                        }
{   EMail       :   zylove619@hotmail.com                                     }
{   Description :   The ATLibraryLoader can conveniently load libraries,      }
{                   it avoid the tedious process of defining proc types       }
{                   and getting the proc address from the library.            }
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
 * The Original Code is ATLibraryLoader.
 * Unit owner : ZY (zylove619@hotmail.com) All rights reserved.
 *
 * ***** END LICENSE BLOCK ***** *)

(*
  Change log:

  Version 1.001 by ZY:
    (2014.08.10) + First version created.
    
*)

unit ATLibraryLoader;

{$I AT.inc}

interface

{$IFNDEF MSWINDOWS}
  {$MESSAGE ERROR 'ATLibraryLoader can only used in MS WINDOWS.'}
{$ENDIF ~MSWINDOWS}

uses
{$IFDEF MSWINDOWS}
  Windows
{$ENDIF}
  , Classes, SysUtils
{$IFDEF ENHANCEDRTTI}
  ,RTTI
{$ENDIF};

const
  ATLibraryLoaderVersion = '1.001';

type

{ Exceptions }

  ELibLoaderException = class(Exception);
  ELibNotFound = class(ELibLoaderException);
  ELibLoadFail = class(ELibLoaderException);
  ELibGetProcsByIndexFail = class(ELibLoaderException);
  ELibGetProcsByNameFail = class(ELibLoaderException);

{ Library item }

  PATLibItem = ^TATLibItem;
  TATLibItem = record
    // The name of the lib
    LibName: string;

    // The lib's handle
    Module: HMODULE;

    // The lib's container
    Holder: Pointer;

    // The lib container's size
    HolderSize: Integer;
  end;

{ Library Loader }

  TATLibsArray = array of string;

{$IFDEF HAS_ANONYMOUSMETHOD}
  TATLibsProc = reference to procedure(const ALib: TATLibItem);
{$ELSE}
  TATLibsProc = procedure(const ALib: TATLibItem) of object;
{$ENDIF}

  TATLibNotifyEvent = TATLibsProc;

  TATLibLoader = class(TObject)
  private
    FLibs: TList;
    FOnLibLoad: TATLibNotifyEvent;
    FOnLibUnLoad: TATLibNotifyEvent;
    procedure DoLoadLibrary(ALibItem: PATLibItem);
    function IsValidModule(AModule: HMODULE): Boolean;
  {$IFDEF ENHANCEDRTTI}
    procedure GetProcsByName<T>(const AHolder: T; ALibItem: PATLibItem);
  {$ELSE}
    procedure GetProcsByIndex(ALibItem: PATLibItem);
  {$ENDIF}
  
    function CreateLibItem(const ALibName: string; const AHolder; AHolderSize: Integer): PATLibItem;
    procedure DestroyLibItem(ALibItem: PATLibItem);
    procedure UnloadLibItem(ALibItemIndex: Integer);

    function GetLibsCount: Integer;
  protected
    constructor Create;
    procedure DoLibLoad(ALibItem: PATLibItem); virtual;
    procedure DoLibUnLoad(ALibItem: PATLibItem); virtual;
  public
    destructor Destroy; override;

    { Load: ALibName    : The name of the library, if the path is ignored,
                          then the current dir will be used.
            AHolder     : A record-type container which hold the library.
            AHolderSize : The size of the holder(ignored since D2010).

      NOTE: Make sure all items in the record are proc pointer types, in
            other words, the holder's size must be an integer multiple of
            the pointer size, otherwise the holder may hold the wrong addresses.
    }
  {$IFDEF ENHANCEDRTTI}
    { The AHolderSize is ignored, here it just used for compatibility. }
    procedure Load<T: record>(const ALibName: string; const AHolder: T; AHolderSize: Integer = -1);
  {$ELSE}
    procedure Load(const ALibName: string; const AHolder; AHolderSize: Integer);
  {$ENDIF}
  
    procedure UnLoadByHolder(const AHolder);
    procedure UnLoadByName(const ALibName: string);
    procedure UnloadAll;

    function IsLibLoaded(const ALibName: string): Boolean;
    function IsHolderUsed(const AHolder): Boolean;

    function GetLibs: TATLibsArray;
    procedure LibsIterator(ALibsIterator: TATLibsProc);
     
    property LibsCount: Integer read GetLibsCount;

    property OnLibLoad:   TATLibNotifyEvent read FOnLibLoad write FOnLibLoad;
    property OnLibUnLoad: TATLibNotifyEvent read FOnLibUnLoad write FOnLibUnLoad;
  end;

(* Useage:

  in Dll:

    function GetSum(A, B: Integer): Integer; stdcall;
    begin
      Result := A + B;
    end;

    procedure ShowHello; stdcall;
    begin
      ShowMessage('Hello');
    end;

    exports
      GetSum {$IFNDEF D2010AndUp} index 1 {$ENDIF},
      Show   {$IFNDEF D2010AndUp} index 2 {$ENDIF};
    end.

  ---------------

  in Exe:

    type
      TMyDll = record
        GetSum: function(A, B: Integer): Integer; stdcall;
        ShowHello: procedure; stdcall;
      end;

    var
      MyDll: TMyDll;

    {$IFDEF D2010AndUp}

      DllLoader.Load('MyDll.dll', MyDll{, SizeOf(MyDll)});
     
    {$ELSE}

      // The dll must exported by index start from 1(exports GetSum index 1,
      // ShowHello index 2... ), and the record MUST keep the same order.
      DllLoader.Load('MyDll.dll', MyDll, SizeOf(MyDll));

    {$ENDIF}

      MyDll.ShowHello;
      MyDll.GetSum(1, 2);

      // UnLoad the lib manually
      DllLoader.UnLoadByHolder(MyDll);

      // NOTE: The Loader will auto unload all the lib items when it
               is no longer used.
*)
function DllLoader: TATLibLoader;

{ Public resourcestring for:
  Method of parameterized type declared in interface
  section must not use local symbol. }
  
resourcestring

  sLibNotFound         = 'Library "%s" not found.';
  sInvalidHolder       = 'Invalid library holder, it cannot be nil.';
  sInvalidHolderSize   = 'The holder''s size must greater than zero and must be aligned with %d.';
  sLibLoadFail         = 'Library "%s" load fail, module: %d, msg: %s';
{$IFDEF ENHANCEDRTTI}
  sRecordRequired      = 'The holder "%s" must be a record type.';
  sGetProcsByNameFail  = 'Library "%s" gets proc by name "%s" fail.' + sLineBreak + 'msg: %s';
{$ELSE}
  sGetProcsByIndexFail = 'Library "%s" gets proc by index %d fail.' + sLineBreak + 'msg: %s';
{$ENDIF}
  sDuplicateLib        = 'Library "%s" has already been loaded.';
  sDuplicateHolder     = 'Library "%s", the holder has already been used, please unload it first.';

implementation

const
  POINTERSIZE = SizeOf(Pointer);

var
  InternalLibLoader: TATLibLoader;

function SameLibName(const ALibName, BLibName: string): Boolean;
begin
  Result := SameText(ALibName, BLibName);
end;

type
  TATLibList = class(TList{<PATLibItem>})
  private
    FOwner: TATLibLoader;
    function GetLibItem(AIndex: Integer): PATLibItem;
  protected
    procedure Notify(Ptr: Pointer; Action: TListNotification); override;  
  public
    constructor Create(AOwner: TATLibLoader);
    procedure LibsIterator(ALibsIterator: TATLibsProc);
    function GetLibs: TATLibsArray;
    function GetLibIndex(const ALibName: string): Integer; overload;
    function GetLibIndex(const AHolder): Integer; overload;
    property LibItems[AIndex: Integer]: PATLibItem read GetLibItem; default;
  end;

{ TATLibList }

function TATLibList.GetLibIndex(const ALibName: string): Integer;
begin
  for Result := 0 to Count - 1 do
    if SameLibName(LibItems[Result].LibName, ALibName) then
      Exit;
  Result := -1;
end;

constructor TATLibList.Create(AOwner: TATLibLoader);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TATLibList.GetLibIndex(const AHolder): Integer;
begin
  for Result := 0 to Count - 1 do
    if LibItems[Result].Holder = @AHolder then
      Exit;
  Result := -1;
end;

function TATLibList.GetLibItem(AIndex: Integer): PATLibItem;
begin
  Result := PATLibItem(Items[AIndex]);
end;

function TATLibList.GetLibs: TATLibsArray;
var
  I: Integer;
begin
  SetLength(Result, Count);
  for I := 0 to Count - 1 do
    Result[I] := LibItems[I].LibName;
end;

procedure TATLibList.LibsIterator(ALibsIterator: TATLibsProc);
var
  I: Integer;
begin
  if Assigned(ALibsIterator) then
    for I := 0 to Count - 1 do
      ALibsIterator(LibItems[I]^);
end;

procedure TATLibList.Notify(Ptr: Pointer; Action: TListNotification);
begin
  inherited;
  if Action = lnAdded then
    FOwner.DoLibLoad(Ptr) 
  else if Action = lnDeleted then
    FOwner.DoLibUnLoad(Ptr);
end;

{ TATLibLoader }

{$IFDEF ENHANCEDRTTI}

procedure TATLibLoader.GetProcsByName<T>(const AHolder: T; ALibItem: PATLibItem);
var
  LR: TRttiContext;
  LA: TArray<TRttiField>;
  LF: TRttiField;
  LRttiType: TRttiType;
  LAddress: PByte;
begin
  LRttiType := LR.GetType(TypeInfo(T));
  if not (LRttiType is TRttiRecordType) then
    raise ELibLoaderException.CreateResFmt(@sRecordRequired, [ALibItem.LibName]);

  LA := TRttiRecordType(LRttiType).GetDeclaredFields;
  for LF in LA do
  begin
    LAddress := PByte(@AHolder) + LF.Offset;
    PPointer(LAddress)^ := GetProcAddress(ALibItem.Module, PChar(LF.Name));
    if not Assigned(PPointer(LAddress)^) then
      raise ELibGetProcsByNameFail.CreateResFmt(@sGetProcsByNameFail,
        [ALibItem.LibName, LF.Name, SysErrorMessage(GetLastError)]);
  end;
end;

procedure TATLibLoader.Load<T>(const ALibName: string; const AHolder: T;
  AHolderSize: Integer = -1);
var
  LLibItem: PATLibItem;
  LRttiContext: TRttiContext;
begin
  LLibItem := CreateLibItem(ALibName, AHolder, LRttiContext.GetType(TypeInfo(T)).TypeSize);
  try
    DoLoadLibrary(LLibItem);
    GetProcsByName(AHolder, LLibItem);
    FLibs.Add(LLibItem);
  except
    DestroyLibItem(LLibItem);
    raise;
  end;
end;

{$ELSE ~ENHANCEDRTTI}

procedure TATLibLoader.GetProcsByIndex(ALibItem: PATLibItem);
var
  I: Integer;
  LAddress: PByte;
begin
  LAddress := ALibItem.Holder;
  for I := 1 to ALibItem.HolderSize div POINTERSIZE do
  begin
    PPointer(LAddress)^ := GetProcAddress(ALibItem.Module, PChar(MakeWord(I, 0)));
    if not Assigned(PPointer(LAddress)^) then
      raise ELibGetProcsByIndexFail.CreateResFmt(@sGetProcsByIndexFail,
        [ALibItem.LibName, I, SysErrorMessage(GetLastError)]);
    Inc(LAddress, POINTERSIZE);
  end;
end;

procedure TATLibLoader.Load(const ALibName: string; const AHolder; AHolderSize: Integer);
var
  LLibItem: PATLibItem;
begin
  LLibItem := CreateLibItem(ALibName, AHolder, AHolderSize);
  try
    DoLoadLibrary(LLibItem);
    GetProcsByIndex(LLibItem);
    FLibs.Add(LLibItem);
  except
    DestroyLibItem(LLibItem);
    raise;
  end;
end;

{$ENDIF ~ENHANCEDRTTI}

function TATLibLoader.GetLibs: TATLibsArray;
begin
  Result := TATLibList(FLibs).GetLibs;
end;

function TATLibLoader.GetLibsCount: Integer;
begin
  Result := FLibs.Count;
end;

procedure TATLibLoader.LibsIterator(ALibsIterator: TATLibsProc);
begin
  TATLibList(FLibs).LibsIterator(ALibsIterator);
end;

procedure TATLibLoader.UnloadLibItem(ALibItemIndex: Integer);
var
  LItem: PATLibItem;
begin
  if (ALibItemIndex > -1) and (ALibItemIndex < FLibs.Count) then
  begin
    LItem := TATLibList(FLibs)[ALibItemIndex];
    if Assigned(LItem) then
    begin
      FLibs.Delete(ALibItemIndex);
      DestroyLibItem(LItem);
    end;
  end;
end;

function TATLibLoader.IsHolderUsed(const AHolder): Boolean;
begin
  Result := TATLibList(FLibs).GetLibIndex(AHolder) > -1;
end;

function TATLibLoader.IsLibLoaded(const ALibName: string): Boolean;
begin
  Result := TATLibList(FLibs).GetLibIndex(ExpandFileName(ALibName)) > -1;
end;

function TATLibLoader.IsValidModule(AModule: HMODULE): Boolean;
begin
  Result := (AModule <> 0);
end; 

constructor TATLibLoader.Create;
begin
  inherited Create;
  FLibs := TATLibList.Create(Self);
end;

function TATLibLoader.CreateLibItem(const ALibName: string; const AHolder;
  AHolderSize: Integer): PATLibItem;

  procedure Check(const ALibName: string; const AHolder; AHolderSize: Integer);
  begin
    if not FileExists(ALibName) then
      raise ELibNotFound.CreateResFmt(@sLibNotFound, [ALibName]);

    if @AHolder = nil then
      raise ELibLoaderException.CreateRes(@sInvalidHolder);

    if (AHolderSize <= 0) or ((AHolderSize mod POINTERSIZE) <> 0) then
      raise ELibLoaderException.CreateResFmt(@sInvalidHolderSize, [POINTERSIZE]);

    if IsLibLoaded(ALibName)then
      raise ELibLoaderException.CreateResFmt(@sDuplicateLib, [ALibName]);

    { NOTE: if the used holder was covered, the previous one may have no
            chance to free it's handle. }
    if IsHolderUsed(AHolder) then
      raise ELibLoaderException.CreateResFmt(@sDuplicateHolder, [ALibName]);
  end;

var
  LLibName: string;  
begin
  LLibName := ExpandFileName(ALibName);

  Check(LLibName, AHolder, AHolderSize);

  New(Result);
  Result.LibName    := LLibName;
  Result.Module     := 0;
  Result.Holder     := @AHolder;
  Result.HolderSize := AHolderSize;
end;

destructor TATLibLoader.Destroy;
begin
  UnloadAll;
  FLibs.Free;  
  inherited;
end;

procedure TATLibLoader.DestroyLibItem(ALibItem: PATLibItem);
begin
  if Assigned(ALibItem) then
  begin
    if IsValidModule(ALibItem.Module) then
      FreeLibrary(ALibItem.Module);
    Dispose(ALibItem);
  end;
end;

procedure TATLibLoader.DoLibLoad(ALibItem: PATLibItem);
begin
  if Assigned(FOnLibLoad) then
    FOnLibLoad(ALibItem^);
end;

procedure TATLibLoader.DoLibUnLoad(ALibItem: PATLibItem);
begin
  if Assigned(FOnLibUnLoad) then
    FOnLibUnLoad(ALibItem^);
end;

procedure TATLibLoader.DoLoadLibrary(ALibItem: PATLibItem);
var
  LLastErrorCode: Cardinal;
begin
  ALibItem.Module := LoadLibrary(PChar(ALibItem.LibName));
  if not IsValidModule(ALibItem.Module) then
  begin
    LLastErrorCode := GetLastError;
    raise ELibLoadFail.CreateResFmt(@sLibLoadFail,
      [ALibItem.LibName, ALibItem.Module, SysErrorMessage(LLastErrorCode)]);
  end;
end;

procedure TATLibLoader.UnLoadByHolder(const AHolder);
begin
  UnloadLibItem(TATLibList(FLibs).GetLibIndex(AHolder));
end;

procedure TATLibLoader.UnLoadByName(const ALibName: string);
begin
  UnloadLibItem(TATLibList(FLibs).GetLibIndex(ExpandFileName(ALibName)));
end;

procedure TATLibLoader.UnloadAll;
begin
  while FLibs.Count <> 0 do
    UnloadLibItem(FLibs.Count - 1);
end;

function DllLoader: TATLibLoader;
begin
  if not Assigned(InternalLibLoader) then
    InternalLibLoader := TATLibLoader.Create;
  Result := InternalLibLoader;
end;

initialization
  InternalLibLoader := nil;
  
finalization
  FreeAndNil(InternalLibLoader);
  
end.
