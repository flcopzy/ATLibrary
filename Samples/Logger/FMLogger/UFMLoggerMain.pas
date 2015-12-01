unit UFMLoggerMain;

{
   NOTE:  This demo was created on DelphiXE7 Update1.
}

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.ListBox;

type
  TForm1 = class(TForm)
    BDefLog: TButton;
    MInfo: TMemo;
    BThreadsWrite: TButton;
    BCleanLogFiles: TButton;
    Layout1: TLayout;
    Switch1: TSwitch;
    Label1: TLabel;
    ComboBox1: TComboBox;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure BDefLogClick(Sender: TObject);
    procedure Switch1Switch(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LogMsg(const AMsg: string);
    procedure ComboBox1Change(Sender: TObject);
    procedure BThreadsWriteClick(Sender: TObject);
    procedure BCleanLogFilesClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ATLogger,
  ULoggerDemo;

{$R *.fmx}

var
  LoggerDemo: TLoggerDemo;

procedure TForm1.BCleanLogFilesClick(Sender: TObject);
begin
  LoggerDemo.ShowCleanLogFiles;
end;

procedure TForm1.BDefLogClick(Sender: TObject);
begin
  Log.D('FM default logger executed...');
end;

procedure TForm1.BThreadsWriteClick(Sender: TObject);
begin
  LoggerDemo.ShowThreadsWrite;
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
  LoggerDemo.LogLevel := TATLogLevel(TComboBox(Sender).ItemIndex);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  LoggerDemo := TLoggerDemo.Create;
  LoggerDemo.OnStatus := LogMsg;
  ComboBox1.ItemIndex := Ord(LOG_DEFAULT_LEVEL);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  LoggerDemo.Free;
end;

procedure TForm1.LogMsg(const AMsg: string);
begin
  MInfo.Lines.Add(AMsg);
end;

procedure TForm1.Switch1Switch(Sender: TObject);
begin
  LoggerDemo.LogEnabled := TSwitch(Sender).IsChecked;
end;

end.
