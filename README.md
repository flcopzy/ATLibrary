## Delphi Auxiliary Toolkit
 A set of units for delphi, the purposes is to simplify development, it should supports delphi7 and above(tested in D7, TD, XE2, XE5, XE7, D10, D10.1, D10.2, D10.3, D10.4) and the platforms Windows(XP or later) and Android(MacOS and iOS not tested).
## Install
 Clone the repository, and add units to your project. 
## Features  
- ATLibraryLoader: implements a common interface to easily access from delphi to different kinds of profiles(ini, registry, xml, json, and db profiles).
- ATLogger: A lightweight logging for delphi based applications.
- ATLibraryLoader: makes use dlls becomes simple.
- ATTimeWatcher:A very lightweight stopwatch.
- ATOnlyOneAppInst: Ensure only a single instance of the application runs.
- ATTimer: A high-precision timer in MSWindows.
## Useage
Note: These examples are for Windows and the used file paths are hard-coded, so you may make a little changes if the paths not exist or you want to compile to other platforms,  there are some complete examples in the samples folder.
#### ATConfigurator:
    var
      LC: IATConfigurator;
    begin
      // Create an ini file.
      LC := NCIni('D:\MyConfig.ini');
      
      // Set user name.
      LC.SetConfig('UserName', 'admin', 'User');

      // The password will be encrypted.
      LC.SetConfig('Userpwd', '123456', 'User', True);
    
      // Set array of key values.   
      LC.SetConfig(['IntValue', 'BoolValue', 'FloatValue'], [10, True, 3.14], 'Values');
    
      // Show Config Text.
      ShowMessage(LC.ConfigText);
      
      // All configs now copied to registry.
      LC.CopyTo(NCReg('SoftWare\MyConfig'));
    
      // All configs now copied to xml.
      LC.CopyTo(NCXML('D:\MyConfig.xml'));
    
      // All configs now copied to json.
      LC.CopyTo(NCJSON('D:\MyConfig.json'));
    
      // All configs now copied to MS Access.
      LC.CopyTo(NCDB(...ConnectionStr..., dtMSAccess));
    
      // Copy all configs to clipbrd.
      LC.CopyToClipbrd;
    
      // Save all configs to stream.
      LC.SaveToStream(YourStream);
    
      // Save all configs to file.
      LC.SaveToFile('D:\MyConfig.data');  
      ...  
    end;
 
#### ATLogger:
    var
      LLog: IATLogger;
    begin
      // Create a file log.
      LLog := NewFileLogger('D:\MyLog\MyLogs.log');
     
      // You can set a log level. 
      LLog.LogLevel := llTrace;

      // Log debug info. 
      LLog.D('Output debug info to a file.');
    
      // Delete all the log files from dir "D:\MyLog\".
      //CleanLogFiles(['D:\MyLog\']);
    end;

#### ATLibraryLoader:
    type
      TMyDll = record
        // exported functions list from dll.
        GetSum: function(A, B: Integer): Integer; stdcall;
        ...
      end;

    var
      LMyDll: TMyDll; 
    begin
      // NOTE: ATLibraryLoader can only used in Windows.

      // Load the "MyDll.dll" first.  
      DllLoader.Load('MyDll.dll', LMyDll, SizeOf(LMyDll));
     
      // then call the function.
      LMyDll.GetSum(10, 10);
    end;

#### ATTimeWatcher:
    var
      LTimeWatcher: TATTimeWatcher;
    begin
      // The ATTimeWatcher is very very simple to use.
      LTimeWatcher.Start;
      Sleep(500);
      OutputDebugString(PChar(LTimeWatcher.Elapsed));
    
      // Continue.
      Sleep(600);
      OutputDebugString(PChar(LTimeWatcher.Elapsed));
    
      // Restart.
      LTimeWatcher.Start;
      Sleep(700);
      OutputDebugString(PChar(LTimeWatcher.Elapsed));
    end;
    
#### ATOnlyOneAppInst:
    In dpr file:
    
    program EnsureOnlyOneAppInst;
    
    uses
      Forms,
	  ATOnlyOneAppInst,
      ...;
      
    const
      // You can define your own id.
      CAppGlobalUniqueID = '{F1FB2123-DD15-44DF-B03B-9D724467AA34}_OnlyOneAppInst_Demo';
      
    procedure MyOnAppCall(ANextPID: UInt64; const ANextParam: string);
    begin
      // You can receive the next app's param if it exists.
    end;  
    
    begin        
      // NOTE: ATOnlyOneAppInst currently only used in MSWindows.
      
	  Application.Initialize;
	  
      // Check whether the previous app is running.
      if OnlyOneAppInst(CAppGlobalUniqueID, MyOnAppCall).IsAppRunning then    
        Exit;
        
      
      Application.CreateForm(TFormOOAIMain, FormOOAIMain);
      Application.Run;
    end.   
