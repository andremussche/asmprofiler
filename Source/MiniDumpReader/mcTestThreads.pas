unit mcTestThreads;

interface

uses
  Classes;

type
  TTestThread = class(TThread)
  protected
    procedure Execute;override;
  end;

  TTestThread2 = class(TThread)
  protected
    procedure TestWait;
    procedure Execute;override;
  end;

implementation

uses
  Windows;

{ TTestThread }

procedure TTestThread.Execute;
begin
  while not Terminated do
    Sleep(1);
end;

{ TTestThread2 }

procedure TTestThread2.Execute;
begin
  while not Terminated do
    TestWait;
end;

procedure TTestThread2.TestWait;
begin
  Sleep(1);
end;

end.
