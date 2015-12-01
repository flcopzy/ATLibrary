program DllLoader;

{$IF CompilerVersion >= 25.0}
  {$LEGACYIFEND ON}
{$IFEND}

uses
  Forms,
  UDllLoaderMain in 'UDllLoaderMain.pas' {FrmDllLoaderMain};

{$R *.res}

begin
  // d2006andup
{$IF CompilerVersion >= 18}
  ReportMemoryLeaksOnShutdown := True;
{$IFEND}
  Application.Initialize;
  Application.CreateForm(TFrmDllLoaderMain, FrmDllLoaderMain);
  Application.Run;
end.
