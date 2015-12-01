program ConfigViewer;

{$IF CompilerVersion >= 25.0}
  {$LEGACYIFEND ON}
{$IFEND}

uses
  Forms,
  UMainConfigViewer in 'UMainConfigViewer.pas' {FrmConfigViewer};

{$R *.res}

begin
  //d2006andup
{$IF CompilerVersion >= 18}
  ReportMemoryLeaksOnShutdown := True;
{$IFEND}
  Application.Initialize;
  Application.CreateForm(TFrmConfigViewer, FrmConfigViewer);
  Application.Run;
end.
