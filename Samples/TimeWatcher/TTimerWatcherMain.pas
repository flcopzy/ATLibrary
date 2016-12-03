unit TTimerWatcherMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Spin;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
    SpinEditTime: TSpinEdit;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  ATTimeWatcher;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  LTW: TATTimeWatcher;
  LTime: Cardinal;
  LTimeStamp: Int64;
begin
  LTime := SpinEditTime.Value;

  Memo1.Lines.Add('test start...' + sLineBreak);

  LTW.Start;
  Sleep(LTime);
  Memo1.Lines.Add(Format('elapsed: %s ms', [LTW.Elapsed]));

  // Continue
  Sleep(LTime);
  Memo1.Lines.Add(Format('elapsed: %u ms', [LTW.ElapsedMilliseconds]));

  Memo1.Lines.Add('restart...');

  // Restart
  LTW.Start;
  Sleep(LTime);
  Memo1.Lines.Add(Format('elapsed: %.2f us', [LTW.ElapsedMicroseconds]));

  // Test timestamp
  Memo1.Lines.Add(sLineBreak + 'timestamp test start...' + sLineBreak);

  LTimeStamp := LTW.GetTimeStamp;
  Sleep(LTime);
  Memo1.Lines.Add(Format('elapsed: %u ms', [LTW.ElapsedMsFrom(LTimeStamp)]));

  Memo1.Lines.Add(sLineBreak + 'all tests finished.' + sLineBreak);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Text := '';
end;

end.
