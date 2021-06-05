unit frmOOAIMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormOOAIMain = class(TForm)
    LabelMsg: TLabel;
    MemoLog: TMemo;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AddLog(const ALog: string);
  end;

var
  FormOOAIMain: TFormOOAIMain;

implementation

{$R *.dfm}

procedure TFormOOAIMain.AddLog(const ALog: string);
begin
  MemoLog.Lines.Add(DateTimeToStr(Now) + ' ==> ' + ALog);
end;

procedure TFormOOAIMain.FormCreate(Sender: TObject);
begin
  Caption := 'Created at ' + DateTimeToStr(Now);
end;

end.
