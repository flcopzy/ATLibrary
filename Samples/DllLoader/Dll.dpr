library Dll;

{$IF CompilerVersion >= 25.0}
  {$LEGACYIFEND ON}
{$IFEND}

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Controls,
  Forms,
  StdCtrls,
  Dialogs,
  Classes;

{$R *.res}

function GetSum(A, B: Integer): Integer; stdcall;
begin
  Result := A + B;
end;

procedure Show; stdcall;
begin
  ShowMessage('msg dialog from dll.');
end;

procedure ShowModalForm; stdcall;
var
  LForm: TForm;
begin
  LForm := TForm.CreateNew(nil);
  with LForm do
    try
      Caption := 'I am a dll form.';
      Position := poDesktopCenter;
      BorderStyle := bsDialog;

      with TButton.Create(LForm) do
      begin
        Parent := LForm;
        Left := (LForm.ClientWidth - Width) div 2;
        Top  := (LForm.ClientHeight - Height) div 2;
        Caption := 'Exit';
        ModalResult := mrOK;
      end;

      ShowModal;
    finally
      Free;
    end;
end;

{$IF CompilerVersion >= 21.0}
  {$DEFINE D2010AndUp}
{$IFEND}

{$WARN SYMBOL_PLATFORM OFF}
exports
  GetSum        {$IFNDEF D2010AndUp} index 1 {$ENDIF},
  Show          {$IFNDEF D2010AndUp} index 2 {$ENDIF},
  ShowModalForm {$IFNDEF D2010AndUp} index 3 {$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

end.
