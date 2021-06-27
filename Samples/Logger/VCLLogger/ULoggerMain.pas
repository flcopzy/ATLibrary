unit ULoggerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TFrmLoggerMain = class(TForm)
    RadioGroupLL: TRadioGroup;
    GroupBox1: TGroupBox;
    Button1: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    MInfo: TMemo;
    Button11: TButton;
    Button12: TButton;
    CheckBox1: TCheckBox;
    GroupBox2: TGroupBox;
    Button2: TButton;
    GroupBox4: TGroupBox;
    Button13: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure RadioGroupLLClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure Button13Click(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
    procedure LogMsg(const AMsg: string);
  public
    { Public declarations }
  end;

var
  FrmLoggerMain: TFrmLoggerMain;

implementation

uses
  ATLogger,
  ULoggerDemo;

{$R *.dfm}

var
  LoggerDemo: TLoggerDemo;
 
procedure TFrmLoggerMain.Button13Click(Sender: TObject);
begin
  LoggerDemo.ShowCleanLogFiles;
end;

procedure TFrmLoggerMain.Button1Click(Sender: TObject);
begin
  with LoggerDemo do
    case TATLogLevel(TComponent(Sender).Tag) of
      llTrace: DefaultLog.T('Log Trace Test');
      llDebug: DefaultLog.D('Log Debug Test %d', [GetCurrentThreadId]);
      llInfo:  DefaultLog.I('Log Info Test');
      llWarn:  DefaultLog.W('Log Warn Test');
      llError: DefaultLog.E('Log Error Test');
      llFatal: DefaultLog.F('Log Fatal Test');
    end;
end;

procedure TFrmLoggerMain.Button2Click(Sender: TObject);
begin
  LoggerDemo.ShowThreadsWrite;
end;

procedure TFrmLoggerMain.CheckBox1Click(Sender: TObject);
begin
  LoggerDemo.LogEnabled := TCheckBox(Sender).Checked;
end;

procedure TFrmLoggerMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not LoggerDemo.IsThreadsBusy;
  if not CanClose then
  begin
    ShowMessage('Demo threads are still running now, please wait...');
    Exit;
  end;
end;

procedure TFrmLoggerMain.FormCreate(Sender: TObject);
begin
  LoggerDemo := TLoggerDemo.Create;
  LoggerDemo.OnStatus := LogMsg;
  RadioGroupLL.ItemIndex := Ord(LOG_DEFAULT_LEVEL);     
end;

procedure TFrmLoggerMain.FormDestroy(Sender: TObject);
begin
  LoggerDemo.Free;
end;

procedure TFrmLoggerMain.LogMsg(const AMsg: string);
begin
  MInfo.Lines.Add(AMsg);
end;

procedure TFrmLoggerMain.RadioGroupLLClick(Sender: TObject);
begin
  LoggerDemo.LogLevel := TATLogLevel(RadioGroupLL.ItemIndex);
end;

end.
