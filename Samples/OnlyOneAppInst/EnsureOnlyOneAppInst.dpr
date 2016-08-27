program EnsureOnlyOneAppInst;

uses
  ATOnlyOneAppInst,
  Forms,
  SysUtils,
  Windows,
  frmOOAIMain in 'frmOOAIMain.pas' {FormOOAIMain};

{$R *.res}

const
  CAppGlobalUniqueID = '{F1FB2123-DD15-44DF-B03B-9D724467AA34}_OnlyOneAppInst';

procedure MyOnPreviousAppCall;
begin
  DefaultActiveApp('App is already running.', 'Info', True, Application.MainForm);
end;

begin

  { 1. Use deault unique id and empty callback event. }
//  if OnlyOneAppInst.IsAppRunning then
//    Exit;

  { 2. Use your own unique id and the default callback event. }
//  if OnlyOneAppInst(CAppGlobalUniqueID, DefaultOnPreviousAppCall).IsAppRunning then
//    Exit;

  { 3. Use your own unique id and callback event. }
  if OnlyOneAppInst(CAppGlobalUniqueID, MyOnPreviousAppCall).IsAppRunning then
    Exit;

  { 4. Use anonymous method. }
//  if OnlyOneAppInst(CAppGlobalUniqueID,
//                    procedure
//                    begin
//                      { Do something useful.}
//                    end).IsAppRunning then
//    Exit;

  Application.Initialize;
  Application.CreateForm(TFormOOAIMain, FormOOAIMain);
  Application.Run;
end.

