unit _uLoadAsDelphiPlugin;

interface

uses
  ToolsAPI,
  Menus, Forms, Classes;

type
  TProfilerExperts = class(TNotifierObject, IOTANotifier, IOTAWizard)
  private
  protected
    // IOTAWizard
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
                      RegisterProc: TWizardRegisterProc;
                      var Terminate: TWizardTerminateProc): Boolean; stdcall;

exports
  InitWizard name 'INITEXPERT0016', { Delphi 2.0x & C++Builder }
  InitWizard name 'INITEXPERT0017', { Delphi 3 }
  InitWizard name 'INITWIZARD0001',
  InitWizard name 'INITEXPERT'; 

implementation

uses _uAsmProfiler, _uProfilerManager, _frmProfileMain;

procedure DoneExpert;
begin
  // cleanup
  DestroyProfileForm;
  RemoveAllProfileIntercepts;
end;

function InitWizard(const BorlandIDEServices: IBorlandIDEServices;
                    RegisterProc: TWizardRegisterProc;
                    var Terminate: TWizardTerminateProc): Boolean; stdcall;
begin
  Result := True;
  try
    Terminate := DoneExpert;
    (BorlandIDEServices as IOTAWizardServices).AddWizard(TProfilerExperts.Create as IOTAWizard);

    ShowProfileForm;
    if ProfilerManager.SelectedItemsCount <= 0 then
      frmProfileMain.btnSelectItems.Click
    else
      frmProfileMain.btnStart.Click;
      //ProfilerManager.StartProfiling;
  except
  end
end {InitExpert};

{ TGExperts }

constructor TProfilerExperts.Create;
begin
  inherited Create;
end;

destructor TProfilerExperts.Destroy;
begin
  //
  inherited;
end;

procedure TProfilerExperts.Execute;
begin
  //
end;

function TProfilerExperts.GetIDString: string;
begin
  Result := 'AsmProfiler';
end;

function TProfilerExperts.GetName: string;
begin
  Result := 'AsmProfiler';
end;

function TProfilerExperts.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.

{see c:\PROJECTS\Gexperts\Source\Framework\GX_LibrarySource.pas for example}
