unit mcTestThread;

interface

uses
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF};

type
  TTestThread = class(TThread)
  private
    procedure SetName;
  protected
    procedure Execute; override;

    procedure Function1000;
    procedure Function100;
    procedure Function10;
  end;

implementation

uses
  SysUtils;

{ Important: Methods and properties of objects in visual components can only be
  used in a method called using Synchronize, for example,

      Synchronize(UpdateCaption);

  and UpdateCaption could look like,

    procedure TTestThread.UpdateCaption;
    begin
      Form1.Caption := 'Updated in a thread';
    end; }

{$IFDEF MSWINDOWS}
type
  TThreadNameInfo = record
    FType: LongWord;     // must be 0x1000
    FName: PAnsiChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ TTestThread }

procedure TTestThread.Function10;
begin
  if Terminated then Abort;
  Sleep(10);
end;

procedure TTestThread.Function100;
var
  i: Integer;
begin
  for i := 0 to 10 do
    Function10;
end;

procedure TTestThread.Function1000;
var
  i: Integer;
begin
  for i := 0 to 10 do
    Function100;
end;

procedure TTestThread.SetName;
{$IFDEF MSWINDOWS}
var
  ThreadNameInfo: TThreadNameInfo;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  ThreadNameInfo.FType := $1000;
  ThreadNameInfo.FName := 'TTestThread';
  ThreadNameInfo.FThreadID := $FFFFFFFF;
  ThreadNameInfo.FFlags := 0;

  try
    RaiseException( $406D1388, 0, sizeof(ThreadNameInfo) div sizeof(LongWord), @ThreadNameInfo );
  except
  end;
{$ENDIF}
end;

procedure TTestThread.Execute;
begin
  SetName;

  while not Terminated do
  begin
    Function10;
    Function100;
    Function1000;
  end;
end;

initialization
  //TTestThread.Create(False);

end.
