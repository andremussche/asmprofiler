unit _uTestThread;

interface

uses
  Controls, Messages, Sysutils,
  Classes {$IFDEF MSWINDOWS} , Windows {$ENDIF};

type
  TTestThread = class(TThread)
  private
    procedure SetName;
    procedure WaitForMessages;
  protected
    procedure Execute; override;
  end;

implementation

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
    FName: PChar;        // pointer to name (in user address space)
    FThreadID: LongWord; // thread ID (-1 indicates caller thread)
    FFlags: LongWord;    // reserved for future use, must be zero
  end;
{$ENDIF}

{ TTestThread }

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

procedure TTestThread.WaitForMessages;
var
  msg: TMsg;
  Message: TMessage;
begin
  WaitMessage;
  //if GetMessage(msg,0,0,0) then
  while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
  begin
    TranslateMessage(msg);
    DispatchMessage(msg);

    Message.Msg := Msg.Message; // Yes, this looks weird
    Message.wParam := Msg.wParam;
    Message.lParam := Msg.lParam;
    // Message.Result can remain undefined;
    // it's not used with PostMessage anyway
    Dispatch(Message);
  end;
end;

procedure TTestThread.Execute;
begin
  SetName;
  { Place thread code here }

  while not Terminated do
  begin
    {nested execute with try..except to retry after error}
    try
      while not Terminated do
        Self.WaitForMessages;
    except
      on E: Exception do
      begin
        raise;
        //Globals.ExceptionHandler.HandleException( Self, E);
      end;
    end;
  end;
end;

end.
 