program Timer;

uses
  Forms,
  frmTimerMain in 'frmTimerMain.pas' {FormTimerMain},
  ATTimer in '..\..\Source\ATTimer.pas',
  ATTimeWatcher in '..\..\Source\ATTimeWatcher.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormTimerMain, FormTimerMain);
  Application.Run;
end.
