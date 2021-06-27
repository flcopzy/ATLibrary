unit ULoggerDemo;

interface

uses
  Classes, SysUtils,
{$IFNDEF MSWINDOWS}
  System.IOUtils,
{$ENDIF}
  ATTimeWatcher,
  ATLogger;

type

  TStatusNotify = procedure(const AMsg: string) of object;

  TLoggerDemo = class(TObject)
  private
    FDefaultLog,
    FDefaultFileLog,
    FEveryDayLog: IATLogger;
    FLogEnabled: Boolean;

    FThreadTimeWatcher : TATTimeWatcher;
    FThreadsCount: Integer;
    FTotalWriteCount: Integer;
    FOnStatus: TStatusNotify;
    FLogLevel: TATLogLevel;
    procedure OnLogThreadTerminate(Sender: TObject);
    procedure DoStatus(const AMsg: string);
    function GeIsThreadsBusy: Boolean;
    procedure SetLogEnabled(const Value: Boolean);
    procedure SetLogLevel(const Value: TATLogLevel);
  public
    constructor Create;
    destructor Destroy; override;

    procedure ShowThreadsWrite;
    procedure ShowCleanLogFiles;

    property LogEnabled: Boolean read FLogEnabled write SetLogEnabled;
    property LogLevel: TATLogLevel read FLogLevel write SetLogLevel;

    property OnStatus: TStatusNotify read FOnStatus write FOnStatus;
    property DefaultLog: IATLogger read FDefaultLog;
    property IsThreadsBusy: Boolean read GeIsThreadsBusy;
  end;

implementation

type
  TThreadLogTest = class(TThread)
  private
    FWriteCount: Integer;
    FID: string;
    FLogger: IATLogger;
    FOwner: TLoggerDemo;
  public
    procedure Execute; override;
    constructor Create(AOwner: TLoggerDemo; AWriteCount: Integer; AID: string; const ALogger: IATLogger = nil);
  end;

{ TThreadLogTest }

constructor TThreadLogTest.Create(AOwner: TLoggerDemo; AWriteCount: Integer; AID: string; const ALogger: IATLogger);
begin
  FOwner := AOwner;
  FWriteCount := AWriteCount;
  FID := AID;
  FLogger := ALogger;

  if FLogger = nil then
    FLogger := AOwner.FDefaultFileLog;

  FreeOnTerminate := True;
  OnTerminate := FOwner.OnLogThreadTerminate;

  Inc(FOwner.FTotalWriteCount, AWriteCount);
  Inc(FOwner.FThreadsCount);

  FOwner.DoStatus(Format('Thread %s create, write count: %d', [AID, AWriteCount]));

  inherited Create(False);
end;

procedure TThreadLogTest.Execute;
var
  I: Integer;
begin
  for I := 1 to FWriteCount do
  begin
    if Terminated then
      Exit;
    FLogger.D(FID + '  ' + IntToStr(I));
  end;
end;

{ TLoggerDemo }

function GetDefaultLogPath: string;
begin
{$IFDEF MSWINDOWS}
  Result := ExtractFilePath(ParamStr(0));
{$ELSE}
  Result := TPath.GetPublicPath;
{$ENDIF}
  Result := IncludeTrailingPathDelimiter(Result) + 'ATLogs' + PathDelim;
end;

function EveryDayLogFileName: string;
begin
  Result := GetDefaultLogPath + FormatDateTime('YYYY-MM-DD', Date()) + '.log';
end;

constructor TLoggerDemo.Create;
begin
  { get default logger }
  FDefaultLog := GetDefaultLogger;

  { Create s static file name logger }
  FDefaultFileLog := NewFileLogger(GetDefaultLogPath + 'Log.log');

  { Create a file name event logger. }
  FEveryDayLog := NewFileLogger(EveryDayLogFileName);
end;

destructor TLoggerDemo.Destroy;
begin
  FDefaultLog     := nil;
  FDefaultFileLog := nil;
  FEveryDayLog    := nil;
  inherited;
end;

procedure TLoggerDemo.DoStatus(const AMsg: string);
begin
  if Assigned(FOnStatus) then
    FOnStatus(AMsg);
end;

function TLoggerDemo.GeIsThreadsBusy: Boolean;
begin
  Result := FThreadsCount > 0;
end;

procedure TLoggerDemo.OnLogThreadTerminate(Sender: TObject);
begin
  Dec(FThreadsCount);
  DoStatus(Format('Thread %s terminated.', [TThreadLogTest(Sender).FID]));
  if FThreadsCount = 0 then
  begin
    DoStatus(Format('Threads write total count: %d.', [FTotalWriteCount]));
    DoStatus(Format('All threads terminated, waste time: %s ms.', [FThreadTimeWatcher.Elapsed]));
    DoStatus('------------------------------------------------------------------');
  end;
end;

procedure TLoggerDemo.SetLogEnabled(const Value: Boolean);
begin
  FLogEnabled := Value;
  FDefaultLog.Enabled     := FLogEnabled;
  FDefaultFileLog.Enabled := FLogEnabled;
  FEveryDayLog.Enabled    := FLogEnabled;
end;

procedure TLoggerDemo.SetLogLevel(const Value: TATLogLevel);
begin
  FLogLevel := Value;
  FDefaultLog.LogLevel     := FLogLevel;
  FDefaultFileLog.LogLevel := FLogLevel;
  FEveryDayLog.LogLevel    := FLogLevel;
end;

procedure TLoggerDemo.ShowCleanLogFiles;
begin
  CleanLogFiles([GetDefaultLogPath()]);
  DoStatus('Log files cleaned.');
end;

procedure TLoggerDemo.ShowThreadsWrite;
begin
  if FThreadsCount <> 0 then
  begin
    DoStatus('Threads are still running now, please wait...');
    Exit;
  end;

  FTotalWriteCount := 0;
  FThreadsCount    := 0;

  FThreadTimeWatcher.Start;

  TThreadLogTest.Create(Self, 10000, 'a');
  TThreadLogTest.Create(Self, 80000, 'b');
  TThreadLogTest.Create(Self, 20000, 'c');
  TThreadLogTest.Create(Self, 25000, 'd');
  TThreadLogTest.Create(Self, 34000, 'e');
  TThreadLogTest.Create(Self, 10000, 'f');
  TThreadLogTest.Create(Self, 80000, 'g');
  TThreadLogTest.Create(Self, 20000, 'h');
  TThreadLogTest.Create(Self, 25000, 'i');
  TThreadLogTest.Create(Self, 34000, 'j');
  TThreadLogTest.Create(Self, 10000, 'k');
  TThreadLogTest.Create(Self, 80000, 'l');
  TThreadLogTest.Create(Self, 20000, 'm');
  TThreadLogTest.Create(Self, 25000, 'n');
  TThreadLogTest.Create(Self, 34000, 'o');
  TThreadLogTest.Create(Self, 10000, 'p');
  TThreadLogTest.Create(Self, 80000, 'q');
  TThreadLogTest.Create(Self, 20000, 'r');
  TThreadLogTest.Create(Self, 25000, 's');
  TThreadLogTest.Create(Self, 34000, 't');
  TThreadLogTest.Create(Self, 34000, 'u');
  TThreadLogTest.Create(Self, 10000, 'v');
  TThreadLogTest.Create(Self, 80000, 'w');
  TThreadLogTest.Create(Self, 20000, 'x');
  TThreadLogTest.Create(Self, 25000, 'y');
  TThreadLogTest.Create(Self, 34000, 'z', FEveryDayLog);
end;

end.
