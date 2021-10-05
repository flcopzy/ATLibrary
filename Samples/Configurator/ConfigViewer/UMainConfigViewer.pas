unit UMainConfigViewer;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 25.0}
    {$LEGACYIFEND ON}
  {$IFEND}
{$ENDIF}

{$DEFINE HAS_SYSTEM_ACTIONS}
{$IF CompilerVersion <= 23.0}
  {$UNDEF HAS_SYSTEM_ACTIONS}
{$IFEND}

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, ComCtrls, ShellAPI,

  ATConfigurator, DB, ADODB, Menus, ActnList

  {$IFDEF HAS_SYSTEM_ACTIONS}
  , System.Actions
  {$ENDIF}

{$IFNDEF UNICODE}
  {$IF Sizeof(CChar) = 2}
  {$DEFINE USE_WIDESTRING}
  , TntComCtrls, TntStdCtrls, TntForms, TntSysUtils
  {$IFEND}
{$ENDIF};

type

  TStringList = TCStringList;

{$IFDEF USE_WIDESTRING}
  TListView = class(TTntListView);
  TEdit     = class(TTntEdit);
  TMemo     = class(TTntMemo);
  TForm     = class(TTntForm);
{$ENDIF}

  TFrmConfigViewer = class(TForm)
    PanelConfig: TPanel;
    EIniFileName: TEdit;
    RBIniFile: TRadioButton;
    RBRegistry: TRadioButton;
    ERegistryKey: TEdit;
    RBDB: TRadioButton;
    PanelDB: TPanel;
    MDBConnectStr: TMemo;
    RGDBType: TRadioGroup;
    Label6: TLabel;
    EDBTableName: TEdit;
    Label7: TLabel;
    Button2: TButton;
    ADOConnection: TADOConnection;
    Splitter: TSplitter;
    PanelView: TPanel;
    PanelOpt: TPanel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditGroup: TEdit;
    EditKey: TEdit;
    EditValue: TEdit;
    RBAdd: TRadioButton;
    RBEdit: TRadioButton;
    Panel3: TPanel;
    EXMLFileName: TEdit;
    RBXMLFile: TRadioButton;
    RBJSONFile: TRadioButton;
    EJSONFileName: TEdit;
    PMConfig: TPopupMenu;
    ALConfig: TActionList;
    ActionDelete: TAction;
    MIDeleteConfig: TMenuItem;
    ActionSaveToFile: TAction;
    MISaveToFile: TMenuItem;
    ActionCopyToClipbrd: TAction;
    MICopyToClipbrd: TMenuItem;
    N1: TMenuItem;
    PCConfig: TPageControl;
    TSTable: TTabSheet;
    ListView: TListView;
    ActionReload: TAction;
    MIReload: TMenuItem;
    ActionClear: TAction;
    MIConfigClear: TMenuItem;
    N2: TMenuItem;
    Button1: TButton;
    ActionSubmit: TAction;
    ActionCreateConfigurator: TAction;
    PanelDetailViews: TPanel;
    Splitter1: TSplitter;
    GroupBoxStdConfigView: TGroupBox;
    GroupBoxConfigTextView: TGroupBox;
    MStd: TMemo;
    MConfig: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure RBAddClick(Sender: TObject);
    procedure ListViewKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure ListViewSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure RBIniFileClick(Sender: TObject);
    procedure EditKeyKeyPress(Sender: TObject; var Key: Char);
    procedure EditValueKeyPress(Sender: TObject; var Key: Char);
    procedure EditGroupKeyPress(Sender: TObject; var Key: Char);
    procedure ActionDeleteExecute(Sender: TObject);
    procedure ActionSaveToFileExecute(Sender: TObject);
    procedure ActionCopyToClipbrdExecute(Sender: TObject);
    procedure PMConfigPopup(Sender: TObject);
    procedure ActionReloadExecute(Sender: TObject);
    procedure ActionClearExecute(Sender: TObject);
    procedure ActionSubmitExecute(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ActionCreateConfiguratorExecute(Sender: TObject);
    procedure PanelDetailViewsResize(Sender: TObject);
    procedure PanelViewClick(Sender: TObject);
  private
    { Private declarations }
    FAppConfigurator: IATConfigurator;
    FConfigurator: IATConfigurator;
    procedure ReadIni;
    procedure WriteIni;
    procedure DoRefresh;
    procedure CreateItems(const AKey, AValue, AGroup: CString);
    procedure MsgDropFile(var message: TWMDROPFILES); message WM_DROPFILES;
  public  
    { Public declarations }
  end;

var
  FrmConfigViewer: TFrmConfigViewer;

implementation 

{$R *.dfm}

const
  // Application config section.
  App_SectionName    = 'App';

  // Only for win32.
  DBConnectionString = 'Provider=Microsoft.Jet.OLEDB.4.0;Data Source=%s;Persist Security Info=False';

procedure CShowMessage(const AStr: CString);
begin
{$IFDEF USE_WIDESTRING}
  MessageboxW(Application.Handle, PWideChar(AStr), PWideChar(WideString(Application.Title)), 0);
{$ELSE}
  ShowMessage(AStr);
{$ENDIF}
end;

type TCustomEditAccess = class(TCustomEdit);

procedure SetEditText(AEdit: TCustomEdit; const AText: CString);
begin
  if not TCustomEditAccess(AEdit).ReadOnly then
  {$IFDEF USE_WIDESTRING}
    SendMessageW(AEdit.Handle, WM_SETTEXT, 0, LPARAM(AText));
  {$ELSE}
    SendMessage(AEdit.Handle, WM_SETTEXT, 0, LPARAM(AText));
  {$ENDIF}
end;

function GetEditText(AEdit: TCustomEdit): CString;
var
  LBuffer: array[0..255] of CChar;
  LLen: Integer;
begin
  LLen :=
  {$IFDEF USE_WIDESTRING}
    GetWindowTextW(AEdit.Handle, LBuffer, Length(LBuffer));
  {$ELSE}
    GetWindowText(AEdit.Handle, LBuffer, Length(LBuffer));
  {$ENDIF}
  SetLength(Result, LLen);
  Move(LBuffer, Pointer(Result)^, SizeOf(CChar) * LLen);
end;

procedure AssertEditTextNotEmpty(AEdit: TCustomEdit; const APrompt: CString; AFocusEmptyEdit: Boolean = True);
var
  LText: CString;
begin
  LText := GetEditText(AEdit);
  if Trim(LText) = '' then
  begin
    CShowMessage(APrompt);
    if AFocusEmptyEdit and AEdit.CanFocus then
      AEdit.SetFocus;
    Abort;
  end;
end;

function GetAppPath: CString;
begin
{$IFDEF USE_WIDESTRING}
  Result := WideExtractFilePath(WideGetModuleFileName(HInstance));
{$ELSE}
  Result := ExtractFilePath(ParamStr(0));
{$ENDIF}
end;

{ ChangeWindowMessageFilter begin }
const
  WM_COPYGLOBALDATA = $49;
  MSGFLT_ADD        = 1;

type
  TChangeWindowMessageFilter = function(uMessage: UINT; dwFlag: DWORD): BOOL; stdcall;

procedure DoChangeWindowMessageFilter;
var
  LH: THandle;
  LChangeWindowMessageFilter: TChangeWindowMessageFilter;
begin
  LH := LoadLibrary(User32);
  if LH <> 0 then
    try
      LChangeWindowMessageFilter := GetProcAddress(LH, 'ChangeWindowMessageFilter');
      if Assigned(LChangeWindowMessageFilter) then
      begin
        LChangeWindowMessageFilter(WM_COPYGLOBALDATA, MSGFLT_ADD);
        LChangeWindowMessageFilter(WM_DROPFILES, MSGFLT_ADD);
      end;
    finally
      FreeLibrary(LH);
    end;
end;
{ ChangeWindowMessageFilter end }

procedure TFrmConfigViewer.ActionClearExecute(Sender: TObject);
begin
  if (ListView.Items.Count > 0) and
   (Application.MessageBox('Confirmed to clear all the configs?', 'ask',
     MB_YESNO or MB_ICONQUESTION) = IDYES) then
  begin
    FConfigurator.ClearAllConfigs;
    DoRefresh;
  end;
end;

procedure TFrmConfigViewer.ActionCopyToClipbrdExecute(Sender: TObject);
begin
  FConfigurator.CopyToClipbrd;
end;

procedure TFrmConfigViewer.ActionCreateConfiguratorExecute(Sender: TObject);
begin
  FConfigurator := nil;
  PanelOpt.Enabled := False;

  if RBIniFile.Checked then
  begin
    AssertEditTextNotEmpty(EIniFileName, 'The ini file name can not be empty.');
    FConfigurator := NCIni(EIniFileName.Text);
  end else if RBRegistry.Checked then
  begin
    AssertEditTextNotEmpty(ERegistryKey, 'The registry key can not be empty.');

    // NOTE: Administrator permission may be required.
    FConfigurator := NCReg(ERegistryKey.Text);
  end else if RBXMLFile.Checked then
  begin
    AssertEditTextNotEmpty(EXMLFileName, 'The XML file name can not be empty.');
    FConfigurator := NCXML(EXMLFileName.Text);
  end else if RBJSONFile.Checked then
  begin
    AssertEditTextNotEmpty(EJSONFileName, 'The JSON file name can not be empty.');
    FConfigurator := NCJSON(EJSONFileName.Text);
  end else if RBDB.Checked then
  begin
    AssertEditTextNotEmpty(MDBConnectStr, 'The ConnectStr can not be empty.');
    FConfigurator := NCDB(MDBConnectStr.Text, TATDBType(RGDBType.ItemIndex), EDBTableName.Text);
  end;

  PanelOpt.Enabled := Assigned(FConfigurator);
  DoRefresh;
end;

procedure TFrmConfigViewer.ActionDeleteExecute(Sender: TObject);
begin
  with ListView do
    if (ItemIndex > -1) and (Application.MessageBox(PChar(Format('delete %s\%s ?', [
   Items[ItemIndex].Caption, Items[ItemIndex].SubItems[0]])), 'ask', MB_YESNO + MB_ICONQUESTION) = IDYES) then
  begin
    FConfigurator.DeleteConfig(Items[ItemIndex].SubItems[0], Items[ItemIndex].Caption);
    DoRefresh;
  end;
end;

procedure TFrmConfigViewer.ActionReloadExecute(Sender: TObject);
begin
  DoRefresh;
end;

procedure TFrmConfigViewer.ActionSaveToFileExecute(Sender: TObject);
begin
  if ListView.Items.Count > 0 then
    with TSaveDialog.Create(nil) do
      try
        if Execute then
          FConfigurator.SaveToFile(FileName + '.data');
      finally
        Free;
      end;
end;

procedure TFrmConfigViewer.ActionSubmitExecute(Sender: TObject);
var
  LIsAdd: Boolean;
begin
  if FConfigurator = nil then
    Exit;

  LIsAdd := RBAdd.Checked;

  AssertEditTextNotEmpty(EditGroup, 'The group can not be empty.');
  AssertEditTextNotEmpty(EditKey,   'The key can not be empty.');
  AssertEditTextNotEmpty(EditValue, 'The value can not be empty.');
  
  FConfigurator.SetConfig(EditKey.Text, EditValue.Text, EditGroup.Text);
  DoRefresh;
                                     
  if LIsAdd then
  begin
    RBAdd.Checked := True;
    EditKey.SetFocus;
  end;
end;

procedure TFrmConfigViewer.CreateItems(const AKey, AValue, AGroup: CString);
begin
  with ListView.Items.Add do
  begin
    Caption := AGroup;
    SubItems.Add(AKey);
    SubItems.Add(AValue);
  end;
end;

procedure TFrmConfigViewer.DoRefresh;
var
  I: Integer;
begin

  MStd.Text    := '';
  MConfig.Text := '';

  if (FConfigurator = nil) then
    Exit;

  PanelOpt.Enabled := True;

  // Load config data.
  ListView.Items.BeginUpdate;
  try
    ListView.Clear;
    FConfigurator.ConfigsIterator(CreateItems);
  finally
    ListView.Items.EndUpdate;
  end;

  MStd.Text    := FConfigurator.StdConfigText;
  MConfig.Text := FConfigurator.ConfigText;
  
  TSTable.Caption := Format('Table View (%d)', [ListView.Items.Count]);

  // Locate the current data.
  if ListView.Items.Count > 0 then
  begin
    if ListView.CanFocus then
      ListView.SetFocus;

    if (Trim(EditKey.Text) <> '') and (Trim(EditGroup.Text) <> '') then
    begin
      for I := 0 to ListView.Items.Count - 1 do
        if (ListView.Items[I].Caption = EditGroup.Text) and
           (ListView.Items[I].SubItems[0] = EditKey.Text) then
        begin
          ListView.ItemIndex := I;
          Break;
        end;   
    end else
      ListView.ItemIndex := 0;

  end else
    RBAdd.Checked := True;
end;

procedure TFrmConfigViewer.EditGroupKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^M then
    EditKey.SetFocus;
end;

procedure TFrmConfigViewer.EditKeyKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^M then
    EditValue.SetFocus;
end;

procedure TFrmConfigViewer.EditValueKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = ^M then
    ActionSubmit.Execute;
end;

procedure TFrmConfigViewer.FormCreate(Sender: TObject);
begin
  DoChangeWindowMessageFilter;
  DragAcceptFiles(Handle, True);
  
  ListView.Columns[0].Width := ListView.Width div 3 - 10;
  ListView.Columns[1].Width := ListView.Columns[0].Width;
  ListView.Columns[2].Width := ListView.Columns[0].Width;

  EIniFileName.Text  := GetAppPath + 'Config.ini';
  EXMLFileName.Text  := GetAppPath + 'Config.xml';
  EJSONFileName.Text := GetAppPath + 'Config.json';
  ERegistryKey.Text  := 'SoftWare\ATConfigurator';
  MDBConnectStr.Text := Format(DBConnectionString, [GetAppPath + 'Config.mdb']);

  FAppConfigurator := NCIni;
  ReadIni;

  RBIniFileClick(nil);
end;

procedure TFrmConfigViewer.FormDestroy(Sender: TObject);
begin
  WriteIni;
end;

procedure TFrmConfigViewer.ListViewKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_DELETE) then
    ActionDelete.Execute;
end;

procedure TFrmConfigViewer.ListViewSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
begin
  if Selected then
  begin
    RBEdit.Checked := True;
    with ListView do
      if ItemIndex > -1 then
      begin
        EditGroup.Text := Items[ItemIndex].Caption;
        EditKey.Text   := Items[ItemIndex].SubItems[0];
        EditValue.Text := Items[ItemIndex].SubItems[1];
      end;
  end;
end;

procedure TFrmConfigViewer.MsgDropFile(var message: TWMDROPFILES);
var
  LCount, LhDrop: Integer;
  LPFileName: CPChar;
  LCopyedCount: Cardinal;
  LPoint: TPoint;
  LCursorControl: TControl;
begin
  LhDrop := message.Drop;
  GetMem(LPFileName, MAX_PATH * SizeOf(CPChar));
  try
  {$IFDEF USE_WIDESTRING}
    LCount := DragQueryFileW(LhDrop, MAXDWORD, LPFileName, MAX_PATH - 1);
  {$ELSE}
    LCount := DragQueryFile(LhDrop, MAXDWORD, LPFileName, MAX_PATH - 1);
  {$ENDIF}

    if LCount > 1 then
      Exit;

  {$IFDEF USE_WIDESTRING}
    LCopyedCount := DragQueryFileW(LhDrop, 0, LPFileName, MAX_PATH);
  {$ELSE}
    LCopyedCount := DragQueryFile(LhDrop, 0, LPFileName, MAX_PATH);
  {$ENDIF}

    if LCopyedCount = 0 then
      Exit;

    GetCursorPos(LPoint);
    LCursorControl := FindControl(WindowFromPoint(LPoint));
    if LCursorControl is TCustomEdit then
      SetEditText(TCustomEdit(LCursorControl), LPFileName);
  finally
    FreeMem(LPFileName);
    DragFinish(LhDrop);
  end;
end;

procedure TFrmConfigViewer.PanelDetailViewsResize(Sender: TObject);
begin
  GroupBoxStdConfigView.Width := GroupBoxStdConfigView.Parent.ClientWidth div 2;
end;

procedure TFrmConfigViewer.PanelViewClick(Sender: TObject);
begin
  if not Assigned(FConfigurator) then
    ShowMessage('Please create a configurator first.');
end;

procedure TFrmConfigViewer.PMConfigPopup(Sender: TObject);
begin
  MIDeleteConfig.Enabled  := Assigned(FConfigurator) and (ListView.ItemIndex > -1);
  MIConfigClear.Enabled   := Assigned(FConfigurator) and (ListView.Items.Count > 0);
  MIReload.Enabled        := Assigned(FConfigurator);
  MISaveToFile.Enabled    := MIConfigClear.Enabled;
  MICopyToClipbrd.Enabled := MIConfigClear.Enabled;
end;

procedure TFrmConfigViewer.RBAddClick(Sender: TObject);
begin
  if RBAdd.Checked then
  begin
    EditGroup.Enabled := True;
    EditKey.Enabled   := True;
    
    EditKey.Text   := '';
    EditValue.Text := '';
  end else
  begin
    EditGroup.Enabled := False;
    EditKey.Enabled   := False;
  end;
end;

procedure TFrmConfigViewer.RBIniFileClick(Sender: TObject);
begin
  EIniFileName.Enabled  := RBIniFile.Checked;
  ERegistryKey.Enabled  := RBRegistry.Checked;
  EXMLFileName.Enabled  := RBXMLFile.Checked;
  EJSONFileName.Enabled := RBJSONFile.Checked;
  PanelDB.Enabled       := RBDB.Checked;
end;

procedure TFrmConfigViewer.ReadIni;
begin
  if Assigned(FAppConfigurator) then
    with FAppConfigurator.AccessGroup(App_SectionName) do
    begin
      Left   := GetConfig('Left',   Left);
      Top    := GetConfig('Top',    Top);
      Width  := GetConfig('Width',  Width);
      Height := GetConfig('Height', Height);
    end;
end;

procedure TFrmConfigViewer.WriteIni;
begin
  if Assigned(FAppConfigurator) then
    FAppConfigurator.SetConfig(['Left', 'Top', 'Width', 'Height'],
                               [ Left ,  Top ,  Width ,  Height ],
                               App_SectionName);
end;

end.
