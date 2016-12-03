unit frmTimerMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ExtCtrls, StdCtrls, Spin, Gauges, Math,

  ATTimer, ATTimeWatcher;

type
  TFormTimerMain = class(TForm)
    SysTimer: TTimer;
    CheckBox1: TCheckBox;
    LabelSys: TLabel;
    GaugeSys: TGauge;
    Label1: TLabel;
    GaugeAT: TGauge;
    Label2: TLabel;
    Label3: TLabel;
    Shape1: TShape;
    Shape2: TShape;
    LSysMin: TLabel;
    LSysMax: TLabel;
    LATMin: TLabel;
    LATMax: TLabel;
    SpinEditInterval: TSpinEdit;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure SysTimerTimer(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    FATTimer: TATTimer;
    procedure ATTimerTimer(Sender: TObject);
  end;

var
  FormTimerMain: TFormTimerMain;

implementation

{$R *.dfm}

var
  GSysTimeStamp, GSysMax, GSysMin: Int64;
  GSysTW: TATTimeWatcher;
procedure TFormTimerMain.SysTimerTimer(Sender: TObject);
begin
  GaugeSys.Progress := GSysTW.ElapsedMsFrom(GSysTimeStamp);
  GSysMin := Min(GSysMin, GaugeSys.Progress);
  GSysMax := Max(GSysMax, GaugeSys.Progress);
  LSysMin.Caption := IntToStr(GSysMin);
  LSysMax.Caption := IntToStr(GSysMax);
  GSysTimeStamp := GSysTW.GetTimeStamp;
end;

var
  GATTimeStamp, GATMax, GATMin: Int64;
  GATTW: TATTimeWatcher;
procedure TFormTimerMain.ATTimerTimer(Sender: TObject);
begin
  GaugeAT.Progress := GATTW.ElapsedMsFrom(GATTimeStamp);
  GATMin := Min(GATMin, GaugeAT.Progress);
  GATMax := Max(GATMax, GaugeAT.Progress);
  LATMin.Caption := IntToStr(GATMin);
  LATMax.Caption := IntToStr(GATMax);
  GATTimeStamp := GATTW.GetTimeStamp;
end;

procedure TFormTimerMain.CheckBox1Click(Sender: TObject);
begin
  SpinEditInterval.Enabled := not TCheckBox(Sender).Checked;
 
  if TCheckBox(Sender).Checked then
  begin
    SysTimer.Interval := SpinEditInterval.Value;
    GSysMin := SysTimer.Interval;
    GSysMax := 0;
    GSysTimeStamp := GSysTW.GetTimeStamp;

    FATTimer.Interval := SpinEditInterval.Value;
    GATMin := FATTimer.Interval;
    GATMax := 0;
    GATTimeStamp := GATTW.GetTimeStamp;

    SysTimer.Enabled := True;
    FATTimer.Enabled := True;
  end else
  begin
    SysTimer.Enabled := False;
    FATTimer.Enabled := False;
  end;
end;

procedure TFormTimerMain.FormCreate(Sender: TObject);
begin
  FATTimer := TATTimer.Create(Self);
  FATTimer.Enabled := False;
  FATTimer.OnTimer := ATTimerTimer;
end;

end.
