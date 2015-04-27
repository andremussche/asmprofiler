unit _uTestFunctions;

interface

  procedure Busy1msec;
  procedure Busy10msec;
  procedure Busy100msec;
  procedure Busy250msec;
  procedure Busy500msec;
  procedure Busy1000msec;

  procedure Sleep1msec;
  procedure Sleep10msec;
  procedure Sleep100msec;
  procedure Sleep250msec;
  procedure Sleep500msec;
  procedure Sleep1000msec;

implementation

uses
  SysUtils, Dateutils;

procedure LoopMsec(const aMsec: integer);
var
  tStart: TDatetime;
begin
  tStart := now;
  while millisecondsbetween(now,tStart) < aMsec do ;
end;

procedure Busy1msec;
begin
  LoopMsec(1);
end;

procedure Busy10msec;
begin
  LoopMsec(10);
end;

procedure Busy100msec;
begin
  LoopMsec(100);
end;

procedure Busy250msec;
begin
  LoopMsec(250);
end;

procedure Busy500msec;
begin
  LoopMsec(500);
end;

procedure Busy1000msec;
begin
  LoopMsec(1000);
end;

procedure Sleep1msec;
begin
  Sleep(1);
end;

procedure Sleep10msec;
begin
  Sleep(10);
end;

procedure Sleep100msec;
begin
  Sleep(100);
end;

procedure Sleep250msec;
begin
  Sleep(250);
end;

procedure Sleep500msec;
begin
  Sleep(500);
end;

procedure Sleep1000msec;
begin
  Sleep(1000);
end;

end.
