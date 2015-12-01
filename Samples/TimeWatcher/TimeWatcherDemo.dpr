program TimeWatcherDemo;

{$IF CompilerVersion >= 25.0}
  {$LEGACYIFEND ON}
{$IFEND}

uses
  Forms,
  TTimerWatcherMain in 'TTimerWatcherMain.pas' {Form1};

{$R *.res}

begin
  //d2006andup
{$IF CompilerVersion >= 18}
  ReportMemoryLeaksOnShutdown := True;
{$IFEND}
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
