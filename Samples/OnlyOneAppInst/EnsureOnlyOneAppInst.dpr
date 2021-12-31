program EnsureOnlyOneAppInst;

uses
  Forms,
  frmOOAIMain in 'frmOOAIMain.pas' {FormOOAIMain},
  ATOnlyOneAppInst in '..\..\Source\ATOnlyOneAppInst.pas';

{$R *.res}

const
  CAppGlobalUniqueID = '{F1FB2123-DD15-44DF-B03B-9D724467AA34}_OnlyOneAppInst_Demo';

procedure MyOnAppCall(ANextPID: UInt64; const ANextParam: string);
begin
  if FormOOAIMain = nil then
    Exit;

  if ANextPID = 0 then
    FormOOAIMain.AddLog('App is already running.')
  else
    FormOOAIMain.AddLog('App is already running, new param: ' + ANextParam);
end;

procedure MyOnAppCheck(IsAppRunning: Boolean; var ANeedNotify: Boolean);
begin
  if IsAppRunning then
  begin
    { Notify the first application or not. }
    ANeedNotify := True;

    { If possible, you can do more initializations here,
      e.g. write more information to your shared memory
      to communication with the first application.
    }
  end;
end;

begin
  ReportMemoryLeaksOnShutdown := True;

  Application.Initialize;

  { 1. Use default unique id and empty events. }
//  if OnlyOneAppInst.IsAppRunning then
//    Exit;

  { 2. Use your own unique id and callback event. }
  if OnlyOneAppInst(CAppGlobalUniqueID, MyOnAppCall, MyOnAppCheck).IsAppRunning then
    Exit;

  { 3. Use anonymous method. }
//  if OnlyOneAppInst(CAppGlobalUniqueID,
//                    procedure(ANextPID: UInt64; const ANextParam: string)
//                    begin
//                      MyOnAppCall(ANextPID, ANextParam);
//                    end).IsAppRunning then
//    Exit;

   { 4. NOTE: if use inline var in begin end scope, it should be declare a
              global var to save the intf.

     begin
       if SomeCondition then
       begin
         var LInst: IATOnlyOneAppInst := OnlyOneAppInst(CAppGlobalUniqueID, MyOnAppCall);
         if LInst.IsAppRunning then
           Exit;
       end;

       ...
     end.

     should be:

     var
       LInst: IATOnlyOneAppInst;
     begin
       LInst := OnlyOneAppInst(CAppGlobalUniqueID, MyOnAppCall);
       if SomeCondition then
       begin
         if LInst.IsAppRunning then
           Exit;
       end;

       ...
     end.
   }

  Application.CreateForm(TFormOOAIMain, FormOOAIMain);
  Application.Run;
end.
