unit UDllLoaderMain;

interface

{$IF CompilerVersion >= 25.0}
  {$LEGACYIFEND ON}
{$IFEND}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  ATLibraryLoader;

type

  TDll = record
    GetSum: function(A, B: Integer): Integer; stdcall;
    Show: procedure; stdcall;
    ShowModalForm: procedure; stdcall;
  end;

  TFrmDllLoaderMain = class(TForm)
    MInfo: TMemo;
    BLoadDll: TButton;
    BListDlls: TButton;
    BExecute: TButton;
    BUnLoadDll: TButton;
    procedure BLoadDllClick(Sender: TObject);
    procedure BListDllsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure BExecuteClick(Sender: TObject);
    procedure BUnLoadDllClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FDLL: TDll;
    procedure OnLibLoad(const ALib: TATLibItem);
    procedure OnLibUnLoad(const ALib: TATLibItem);
    procedure LogMsg(const AMsg: string);
  public
    { Public declarations }
    property DLL: TDll read FDLL write FDLL;
  end;

var
  FrmDllLoaderMain: TFrmDllLoaderMain;

implementation

{$IF CompilerVersion >= 21.0}
  {$DEFINE D2010AndUp}
{$IFEND}

{$R *.dfm}

const
  LIBNAME = 'Dll.dll';

procedure TFrmDllLoaderMain.BListDllsClick(Sender: TObject);
var
  I: Integer;
  LLibs: TATLibsArray;
begin
  LLibs := DllLoader.GetLibs;

  if Length(LLibs) = 0 then
  begin
    LogMsg('There is no dll info.');
    Exit;
  end;
  
  LogMsg(Format('dll info(%d): ', [Length(LLibs)]));
  for I := 0 to Length(LLibs) - 1 do
    LogMsg(Format('(%d): %s', [I, LLibs[I]]));
  LogMsg('--------------------------------');
end;

procedure TFrmDllLoaderMain.BUnLoadDllClick(Sender: TObject);
begin
  // or DllLoader.UnLoadByHolder(DLL)
  if DllLoader.IsLibLoaded(LIBNAME) then
    DllLoader.UnLoadByName(LIBNAME)
  else
    LogMsg('The dll has not yet loaded or it has already been unloaded.');
end;

procedure TFrmDllLoaderMain.BExecuteClick(Sender: TObject);
var
  LValue1, LValue2: Integer;
begin
  if not DllLoader.IsHolderUsed(DLL) then
  begin
    LogMsg('The dll has not yet loaded, please load first.');
    Exit;
  end;

  LValue1 := Random(1024);
  LValue2 := Random(10000);
  LogMsg(Format('GetSum(%d, %d) = %d', [LValue1, LValue2, DLL.GetSum(LValue1, LValue2)]));

  DLL.Show;
  DLL.ShowModalForm;
  
  LogMsg('All dll functions executed successful.');
end;

procedure TFrmDllLoaderMain.BLoadDllClick(Sender: TObject);
begin
  if not DllLoader.IsLibLoaded(LIBNAME) then
    DllLoader.Load(LIBNAME, DLL, SizeOf(DLL))
  else
    LogMsg('The dll has already been loaded.');
end;

procedure TFrmDllLoaderMain.FormCreate(Sender: TObject);
begin
  DllLoader.OnLibLoad   := OnLibLoad;
  DllLoader.OnLibUnLoad := OnLibUnLoad;

  Randomize;
end;

procedure TFrmDllLoaderMain.FormDestroy(Sender: TObject);
begin
  DllLoader.UnloadAll;  
end;

procedure TFrmDllLoaderMain.LogMsg(const AMsg: string);
begin
  MInfo.Lines.Add(Format('[%s]:   %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss', Now), AMsg]));
end;

procedure TFrmDllLoaderMain.OnLibLoad(const ALib: TATLibItem);
begin
  LogMsg(Format('%s loaded.', [ALib.LibName]));
end;

procedure TFrmDllLoaderMain.OnLibUnLoad(const ALib: TATLibItem);
begin
  LogMsg(Format('%s unloaded.', [ALib.LibName]));
end;

end.
