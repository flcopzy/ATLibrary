unit frmOOAIMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TFormOOAIMain = class(TForm)
    LabelMsg: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure LabelMsgClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormOOAIMain: TFormOOAIMain;

implementation

{$R *.dfm}

procedure TFormOOAIMain.FormCreate(Sender: TObject);
begin
  Caption := 'Created at ' + DateTimeToStr(Now);
end;

procedure TFormOOAIMain.LabelMsgClick(Sender: TObject);
begin
  Hide;
end;

end.
