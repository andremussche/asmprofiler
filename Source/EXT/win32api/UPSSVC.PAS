unit UPSSVC;
{
* upssvc.h
*
* UPS service interface
*
}

interface

uses
  Windows;

function UPSGetState(): DWORD; stdcall;
{$EXTERNALSYM UPSGetState}

function UPSInit(): DWORD; stdcall;
{$EXTERNALSYM UPSInit}

procedure UPSCancelWait();stdcall;
{$EXTERNALSYM UPSCancelWait}

procedure UPSTurnOff(aTurnOffDelay:DWORD);stdcall;
{$EXTERNALSYM UPSTurnOff}

procedure UPSWaitForStateChange(aCurrentState,anInterval:DWORD);stdcall;
{$EXTERNALSYM UPSWaitForStateChange}

function GetUPSStateText(const aUPS_State:integer):string;
function GetUPSSInitText(const aUPS_Init:integer):string;

const
  C_STATE_UPS_ONLINE              = 1;
  C_STATE_UUPS_ONBATTERY          = 2;
  C_STATE_UUPS_LOWBATTERY         = 4;
  C_STATE_UUPS_NOCOMM             = 8;
  C_STATE_UUPS_CRITICAL           = 16;

  C_INIT_UPS_INITUNKNOWNERROR    = 0;
  C_INIT_UPS_INITOK              = 1;
  C_INIT_UPS_INITNOSUCHDRIVER    = 2;
  C_INIT_UPS_INITBADINTERFACE    = 3;
  C_INIT_UPS_INITREGISTRYERROR   = 4;
  C_INIT_UPS_INITCOMMOPENERROR   = 5;
  C_INIT_UPS_INITCOMMSETUPERROR  = 6;

  C_STATE_UPS_ONLINE_TEXT       = 'UPS ONLINE';
  C_STATE_UUPS_ONBATTERY_TEXT   = 'UPS ONBATTERY';
  C_STATE_UUPS_LOWBATTERY_TEXT  = 'UPS LOWBATTERY';
  C_STATE_UUPS_NOCOMM_TEXT      = 'UPS NOCOMM';
  C_STATE_UUPS_CRITICAL_TEXT    = 'UPS CRITICAL';

  C_INIT_UPS_INITUNKNOWNERROR_TEXT    = 'INIT UNKNOWN ERROR';
  C_INIT_UPS_INITOK_TEXT              = 'INIT OK';
  C_INIT_UPS_INITNOSUCHDRIVER_TEXT    = 'INIT NOSUCH DRIVER';
  C_INIT_UPS_INITBADINTERFACE_TEXT    = 'INIT BAD INTERFACE';
  C_INIT_UPS_INITREGISTRYERROR_TEXT   = 'INIT REGISTRY ERROR';
  C_INIT_UPS_INITCOMMOPENERROR_TEXT   = 'INIT COMM OPEN ERROR';
  C_INIT_UPS_INITCOMMSETUPERROR_TEXT  = 'INIT COMM SETUP ERROR';

implementation

uses SysUtils;

const
  APCUPS = 'APCUPS.dll';

function GetUPSStateText(const aUPS_State:integer):string;
begin
  case aUPS_State of
    C_STATE_UPS_ONLINE              : Result := C_STATE_UPS_ONLINE_TEXT;
    C_STATE_UUPS_ONBATTERY          : Result := C_STATE_UUPS_ONBATTERY_TEXT;
    C_STATE_UUPS_LOWBATTERY         : Result := C_STATE_UUPS_LOWBATTERY_TEXT;
    C_STATE_UUPS_NOCOMM             : Result := C_STATE_UUPS_NOCOMM_TEXT;
    C_STATE_UUPS_CRITICAL           : Result := C_STATE_UUPS_CRITICAL_TEXT;
  else
    Result := Format('Unknown (%d)',[aUPS_State]);
  end;
end;

function GetUPSSInitText(const aUPS_Init:integer):string;
begin
  case aUPS_Init of
    C_INIT_UPS_INITUNKNOWNERROR    : Result := C_INIT_UPS_INITUNKNOWNERROR_TEXT;
    C_INIT_UPS_INITOK              : Result := C_INIT_UPS_INITOK_TEXT;
    C_INIT_UPS_INITNOSUCHDRIVER    : Result := C_INIT_UPS_INITNOSUCHDRIVER_TEXT;
    C_INIT_UPS_INITBADINTERFACE    : Result := C_INIT_UPS_INITBADINTERFACE_TEXT;
    C_INIT_UPS_INITREGISTRYERROR   : Result := C_INIT_UPS_INITREGISTRYERROR_TEXT;
    C_INIT_UPS_INITCOMMOPENERROR   : Result := C_INIT_UPS_INITCOMMOPENERROR_TEXT;
    C_INIT_UPS_INITCOMMSETUPERROR  : Result := C_INIT_UPS_INITCOMMSETUPERROR_TEXT;
  else
    Result := Format('Unknown (%d)',[aUPS_Init]);
  end;
end;

function UPSGetState; external APCUPS name 'UPSGetState';
function UPSInit; external APCUPS name 'UPSInit';
procedure UPSCancelWait; external APCUPS name 'UPSCancelWait';
procedure UPSTurnOff; external APCUPS name 'UPSTurnOff';
procedure UPSWaitForStateChange; external APCUPS name 'UPSWaitForStateChange';

end.
