unit muExactTiming;

interface

  function NowExact: TDatetime;

implementation

uses
  Windows, SysUtils,
  KOLDetours;

type
  TNowFunction = function:TDatetime;

var
  OrgNow: TNowFunction;
  _StartTime: TDatetime;
  _Frequency, _StartCounter: Int64;

function NowExact: TDatetime;
var inowcount, idiff: Int64;
    fdiffsec: Double;
const
  cSecPerDay = 24 * 60 * 60;
begin
  if _StartTime > 0 then
  begin
    //current count
    QueryPerformanceCounter(inowcount);
    idiff  := inowcount - _StartCounter;
    //calc new time: starttime + exact diff
    if idiff > 0 then
    begin
      fdiffsec := (idiff/_Frequency);  //1 = 1s
      Result   := _StartTime + (fdiffsec/cSecPerDay);
    end
    else
    //counter has overrun/reset? reinit again
    begin
      QueryPerformanceCounter(_StartCounter);
      _StartTime := OrgNow;
      Result     := _StartTime;
    end
  end
  else
  //init
  begin
    QueryPerformanceFrequency(_Frequency);
    QueryPerformanceCounter(_StartCounter);
    _StartTime := OrgNow;
    Result     := _StartTime;
  end;
end;

//var t,t2: TDatetime;
initialization
  OrgNow := KOLDetours.InterceptCreate(@Now, @NowExact);
  if not Assigned(OrgNow) then   //failed? then always map OrgNow to Now
    OrgNow := @Now;

  {
  t  := Now;
  sleep(10);
  MessageDlg(Format('%dms',[MilliSecondsBetween(Now,t)]), Dialogs.mtWarning, [mbOK], 0);

  t := Now;
  sleep(1);
  MessageDlg(Format('%dms',[MilliSecondsBetween(Now,t)]), Dialogs.mtWarning, [mbOK], 0);

  t := Now;
  sleep(2);
  MessageDlg(Format('%dms',[MilliSecondsBetween(Now,t)]), Dialogs.mtWarning, [mbOK], 0);

  t := Now;
  sleep(3);
  MessageDlg(Format('%dms',[MilliSecondsBetween(Now,t)]), Dialogs.mtWarning, [mbOK], 0);
  }

end.
