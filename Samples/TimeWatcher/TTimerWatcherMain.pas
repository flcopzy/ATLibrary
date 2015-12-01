unit TTimerWatcherMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    ETime: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Button2: TButton;
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
  I: Integer;
  LTime: Cardinal;
begin
  LTime := StrToInt(ETime.Text);
  for I := 1 to 5 do
  begin
    LTW.Start;
    Sleep(LTime);
    Memo1.Lines.Add(FloatToStr(LTW.ElapsedMilliseconds));

    Sleep(LTime);
    Memo1.Lines.Add(FloatToStr(LTW.ElapsedMilliseconds));
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Memo1.Text := '';
end;

end.
