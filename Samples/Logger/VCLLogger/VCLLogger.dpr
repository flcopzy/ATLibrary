program VCLLogger;

{$IF CompilerVersion >= 25.0}
  {$LEGACYIFEND ON}
{$IFEND}

uses
  Forms,
  ULoggerMain in 'ULoggerMain.pas' {FrmLoggerMain},
  ULoggerDemo in '..\ULoggerDemo.pas';

{$R *.res}

begin
  //d2006andup
{$IF CompilerVersion >= 18}
  ReportMemoryLeaksOnShutdown := True;
{$IFEND}
  Application.Initialize;
  Application.CreateForm(TFrmLoggerMain, FrmLoggerMain);
  Application.Run;
end.
