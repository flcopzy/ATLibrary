
{
  The demo was created on Delphi XE7 Update 1
}

unit UMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo,
  FMX.Controls.Presentation, FMX.Edit, FMX.TabControl;

type
  TForm1 = class(TForm)
    BSetConfig: TButton;
    MStdConfig: TMemo;
    Layout1: TLayout;
    RIni: TRadioButton;
    RXML: TRadioButton;
    RJSON: TRadioButton;
    RDB: TRadioButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EKey: TEdit;
    EGroup: TEdit;
    EValue: TEdit;
    Layout2: TLayout;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    Layout3: TLayout;
    BClear: TButton;
    BCopyToClipbrd: TButton;
    MConfig: TMemo;
    procedure BCopyToClipbrdClick(Sender: TObject);
    procedure BSetConfigClick(Sender: TObject);
    procedure BClearClick(Sender: TObject);
    procedure RIniClick(Sender: TObject);
  private
    { Private declarations }
    procedure Clean;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  IniFiles, System.IOUtils, ATConfigurator;

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}
{$R *.Windows.fmx MSWINDOWS}

var
  FConfigurator: IATConfigurator;

procedure TForm1.BCopyToClipbrdClick(Sender: TObject);
begin
  FConfigurator.CopyToClipbrd;
end;

procedure TForm1.BClearClick(Sender: TObject);
begin
  FConfigurator.ClearAllConfigs;
  Clean;
end;

procedure TForm1.BSetConfigClick(Sender: TObject);
var
  LKey, LValue, LGroup: string;
begin

  if FConfigurator = nil then
  begin
    ShowMessage('please select a type first.');
    Exit;
  end;

  LKey := Trim(EKey.Text);
  if LKey = '' then
  begin
    EKey.SetFocus;
    Exit;
  end;

  LValue := Trim(EValue.Text);
  if LValue = '' then
  begin
    EValue.SetFocus;
    Exit;
  end;

  LGroup := Trim(EGroup.Text);
  if LGroup = '' then
  begin
    EGroup.SetFocus;
    Exit;
  end;

  FConfigurator.SetConfig(LKey, LValue, LGroup);

  MStdConfig.Text := FConfigurator.StdConfigText;
  MConfig.Text    := FConfigurator.ConfigText;

end;

procedure TForm1.Clean;
begin
  EKey.Text   := '';
  EGroup.Text := '';
  EValue.Text := '';

  MStdConfig.Text := '';
  MConfig.Text    := '';
end;

procedure TForm1.RIniClick(Sender: TObject);
var
  LDBFileName: string;
begin
  case TComponent(Sender).Tag of
    1: FConfigurator := NCIni;
    2: FConfigurator := NCXML;
    3: FConfigurator := NCJSON;
    4: begin
         LDBFileName   := TPath.Combine(TPath.GetDocumentsPath, 'Config.s3db');
         FConfigurator := NCDB('DriverID=SQLite;Database=' + LDBFileName, 'Config');
       end;
  end;
  FConfigurator.ValueWrapper := NewDefaultCrypt;
  MStdConfig.Text := FConfigurator.StdConfigText;
  MConfig.Text    := FConfigurator.ConfigText;
end;

end.
