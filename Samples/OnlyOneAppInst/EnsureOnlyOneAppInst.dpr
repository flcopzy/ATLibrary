program EnsureOnlyOneAppInst;

uses
  Forms,
  frmOOAIMain in 'frmOOAIMain.pas' {FormOOAIMain},
  ATOnlyOneAppInst in '..\..\Source\ATOnlyOneAppInst.pas';

{$R *.res}

const
  CAppGlobalUniqueID = '{F1FB2123-DD15-44DF-B03B-9D724467AA34}_OnlyOneAppInst_Demo';

procedure MyOnAppCall(ANextPID: UInt64; const ANextParam: ATOOAIParamString);
begin
  { NOTE: This proc executed only in the first app context. }

  if FormOOAIMain = nil then
    Exit;

  if ANextPID = 0 then
    FormOOAIMain.AddLog('App is already running.')
  else
  begin
    { If unicode not supported, the log may display garbled text,
      but in fact, the param has recved correctly, you can handle
      it manually. }
    FormOOAIMain.AddLog('App is already running, new param: ' + string(ANextParam));
  end;
end;

procedure MyOnAppCheck(IsAppRunning: Boolean; var ANeedNotify: Boolean);
begin
  { NOTE: This proc executed in each app context. }

  if IsAppRunning then
  begin
    { This executed in the next app context. }

    { Notify the first application or not. }
    ANeedNotify := True;

    { If necessary, you can do more initializations before the
      notification, e.g. write more information to other shared
      memory to communication with the first application.
    }
  end else
  begin
    { This executed in the first app context, so the "ANeedNotify"
      is ignored. }

    { You can do some work after you have known that there is no
      other app is running. }
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
//                    procedure(ANextPID: UInt64; const ANextParam: ATOOAIParamString)
//                    begin
//                      MyOnAppCall(ANextPID, ANextParam);
//                    end).IsAppRunning then
//    Exit;

   { 4. NOTE: Do not use inline var in begin end scope, it should be declare a
              global var to hold the intf.

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
