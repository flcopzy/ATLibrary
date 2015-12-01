program FMLogger;

uses
  System.StartUpCopy,
  FMX.MobilePreview,
  FMX.Forms,
  UFMLoggerMain in 'UFMLoggerMain.pas' {Form1},
  ULoggerDemo in '..\ULoggerDemo.pas',
  ATLogger in '..\..\..\Source\ATLogger.pas',
  ATTimeWatcher in '..\..\..\Source\ATTimeWatcher.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
