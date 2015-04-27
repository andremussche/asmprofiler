{:Component wrappers for the GpSync unit.
  @author Primoz Gabrijelcic
  @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2007, Primoz Gabrijelcic
All rights reserved.

Redistribution and use in source and binary forms, with or without modification,
are permitted provided that the following conditions are met:
- Redistributions of source code must retain the above copyright notice, this
  list of conditions and the following disclaimer.
- Redistributions in binary form must reproduce the above copyright notice,
  this list of conditions and the following disclaimer in the documentation
  and/or other materials provided with the distribution.
- The name of the Primoz Gabrijelcic may not be used to endorse or promote
  products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON
ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

  Tested with Delphi 5.

  Author           : Primoz Gabrijelcic
  Creation date    : 2002-06-27
  Last modification: 2007-05-30
  Version          : 1.06a

  </pre>}{

  History:
    1.06a: 2007-05-30
      - AllocateHwnd and DeallocateHwnd replaced with thread-safe versions.
    1.06: 2004-02-10
      - Added TGpCounterComp component.
    1.05a: 2003-01-12
      - Bug fixed in TGpMessageQueueReaderComp. It was prevending the
        application to close on system logoff/restart.
    1.05: 2002-12-13
      - Added TGpFlagComp component.
      - Modified all components so that they are inactive in design mode.
    1.04: 2002-12-01
      - Added method TGpMessageQueueWriterComp.IsReaderAlive.
      - Added property TGpMessageQueueWriterComp.Active.
    1.03: 2002-11-09
      - Moved posting functionality from TGpMessageQueueWriterComp to
        TGpMessageQueueComp so that Reader can post to itself.
    1.02: 2002-10-24
      - Added TGpMessageQueueReaderComp and TGpMessageQueueWriterComp components.
    1.01: 2002-07-16
      - Function TGpTokenComp.IsValidToken renamed to IsTokenPublished.
}
unit GpSyncComp;

interface

uses
  Windows,
  SysUtils,
  Messages,
  Forms,     //keep Forms before Classes so that correct AllocateHwnd is used
  Classes,
  GpSync;

type
  {:TGpFlag wrapper.
    @since   2002-12-13
  }
  TGpFlagComp = class(TComponent)
  private
    FOpFailed: boolean;
    FFlag    : TGpFlag;
    FFlagName: string;
  protected
    function  GetIsSet: boolean; virtual;
    procedure IgnoreOpFailed(const Value: boolean); virtual;
    procedure SetFlagName(const Value: string); virtual;
    procedure SetIsSet(const Value: boolean); virtual;
  public
    destructor  Destroy; override;
    function  IsFlagSet(const flagName: string): boolean;
    property OperationFailed: boolean read FOpFailed write IgnoreOpFailed;
  published
    property FlagName: string read FFlagName write SetFlagName;
    property IsSet: boolean read GetIsSet write SetIsSet;
  end; { TGpFlagComp }

  {:TGpSync wrapper.
    @since   2002-06-27
  }
  TGpTokenComp = class(TComponent)
  private
    FToken: TGpToken;
  protected
    function  GetIsPublished: boolean; virtual;
    function  GetTokenName: string; virtual;
    procedure IgnoreSetTokenName(const Value: string); virtual;
    procedure SetIsPublished(const Value: boolean); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  IsTokenPublished(token: string): boolean;
    procedure Publish;
    procedure Revoke;
  published
    property IsPublished: boolean read GetIsPublished write SetIsPublished;
    property Token: string read GetTokenName write IgnoreSetTokenName;
  end; { TGpTokenComp }

  {:TGpCounter wrapper.
    @since   2004-02-10
  }
  TGpCounterComp = class(TComponent)
  private
    FCounter: TGpCounter;
  protected
    function  GetCounterName: string;
    function  GetValue: integer;
    procedure RecreateCounter(const name: string; initialValue: integer);
    procedure SetCounterName(const Value: string);
    procedure SetValue(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Dec(decrement: integer = 1);
    procedure Inc(increment: integer = 1);
  published
    property CounterName: string read GetCounterName write SetCounterName;
    property Value: integer read GetValue write SetValue;
  end; { TGpCounterComp }

  {:TGpGroup wrapper.
    @since   2002-06-27
  }
  TGpGroupComp = class(TComponent)
  private
    FGroup: TGpGroup;
  protected
    procedure CheckNameSet; virtual;
    function  GetIsEmpty: boolean; virtual;
    function  GetIsMember: boolean; virtual;
    function  GetGroupName: string; virtual;
    procedure IgnoreSetIsEmpty(const Value: boolean); virtual;
    procedure SetIsMember(const Value: boolean); virtual;
    procedure SetGroupName(const Value: string); virtual;
  public
    destructor  Destroy; override;
    procedure Join(var isFirstMember: boolean); overload;
    procedure Join; overload;
    procedure Leave(var wasLastMember: boolean); overload;
    procedure Leave; overload;
  published {order is important!}
    property GroupName: string read GetGroupName write SetGroupName;
    property IsEmpty: boolean read GetIsEmpty write IgnoreSetIsEmpty;
    property IsMember: boolean read GetIsMember write SetIsMember;
  end; { TGpGroupComp }

  {:TGpCountedGroup wrapper.
    @since   2002-06-27
  }
  TGpCountedGroupComp = class(TComponent)
  private
    FCountedGroup: TGpCountedGroup;
    FMemberLimit : integer;
  protected
    procedure CheckNameSet; virtual;
    function  GetIsEmpty: boolean; virtual;
    function  GetIsMember: boolean; virtual;
    function  GetMemberLimit: integer; virtual;
    function  GetCountedGroupName: string; virtual;
    function  GetNumMembers: integer; virtual;
    procedure IgnoreSetIsEmpty(const Value: boolean); virtual;
    procedure IgnoreSetNumMembers(const Value: integer); virtual;
    procedure SetIsMember(const Value: boolean); virtual;
    procedure SetMemberLimit(const Value: integer); virtual;
    procedure SetCountedGroupName(const Value: string); virtual;
  public
    destructor  Destroy; override;
    function  Join(timeout: DWORD; var isFirstMember: boolean): boolean; overload;
    function  Join(timeout: DWORD): boolean; overload;
    procedure Leave(var wasLastMember: boolean); overload;
    procedure Leave; overload;
  published {order is important!}
    property MemberLimit: integer read GetMemberLimit write SetMemberLimit;
    property CountedGroupName: string read GetCountedGroupName write SetCountedGroupName;
    property IsEmpty: boolean read GetIsEmpty write IgnoreSetIsEmpty;
    property IsMember: boolean read GetIsMember write SetIsMember;
    property NumMembers: integer read GetNumMembers write IgnoreSetNumMembers;
  end; { TGpCountedGroupComp }

  {:TGpSWMR wrapper.
    @since   2002-06-27
  }
  TGpSWMRComp = class(TComponent)
  private
    FSWMR: TGpSWMR;
  protected
    procedure CheckNameSet; virtual;
    function  GetAccess: TGpSWMRAccess; virtual;
    function  GetSWMRName: string; virtual;
    procedure SetAccess(const Value: TGpSWMRAccess); virtual;
    procedure SetSWMRName(const Value: string); virtual;
  public
    destructor  Destroy; override;
    procedure Done;
    function  WaitToRead(timeout: DWORD): boolean;
    function  WaitToWrite(timeout: DWORD): boolean;
  published {order is important!}
    property SWMRName: string read GetSWMRName write SetSWMRName;
    property Access: TGpSWMRAccess read GetAccess write SetAccess;
  end; { TGpSWMRComp }

  {:'New messages are waiting' notification event.
    @since   2002-10-24
  }        
  TGpMQReaderNewMessageNotifyEvent = TNotifyEvent;           

  {:'New message' event.
    @since   2002-10-24
  }
  TGpMWReaderNewMessageEvent = procedure (Sender: TObject;
    flags: TGpMQMessageFlags; msg: UINT; wParam: WPARAM;
    lParam: LPARAM; const msgData: string) of object;

  {:Common parent for message queue reader and writer.
    @since   2002-11-09
  }
  TGpMessageQueueComp = class(TComponent)
  protected
    procedure CheckMQExists; virtual; abstract;
    function  MessageQueueObject: TGpMessageQueue; virtual; abstract;
  public
    function PostMessage(timeout: DWORD;
      const msgData: string): TGpMQPostStatus; overload;
    function PostMessage(timeout: DWORD; flags: TGpMQMessageFlags; msg: UINT;
      wParam: WPARAM; lParam: LPARAM; const msgData: string): TGpMQPostStatus; overload;
    function PostMessage(timeout: DWORD; msg: UINT;
      const msgData: string): TGpMQPostStatus; overload;
    function PostMessage(timeout: DWORD; msg: UINT; wParam: WPARAM;
      lParam: LPARAM): TGpMQPostStatus; overload;
  end; { TGpMessageQueueComp }

  {:TGpMessageQueueReader wrapper.
    @since   2002-10-23
  }
  TGpMessageQueueReaderComp = class(TGpMessageQueueComp)
  private
    FActive            : boolean;
    FMessageQueueName  : string;
    FMessageQueueReader: TGpMessageQueueReader;
    FOnNewMessage      : TGpMWReaderNewMessageEvent;
    FOnNewMessageNotify: TGpMQReaderNewMessageNotifyEvent;
    FReaderWindowHandle: HWND;
    FSize              : cardinal;
    FTimeout           : cardinal;
  protected
    procedure CheckMQExists; override;
    procedure CreateReader; virtual;
    function  MessageQueueObject: TGpMessageQueue; override;
    procedure ProcessNewMessages(doNotify: boolean); virtual;
    procedure ReaderWindowProc(var msg: TMessage); virtual;
    procedure SetActive(const Value: boolean); virtual;
    procedure SetMessageQueueName(const Value: string); virtual;
    procedure SetSize(const Value: cardinal); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure DefaultHandler(var Message); override;
    function  GetMessage(timeout: DWORD;
      var msgData: string): TGpMQGetStatus; overload;
    function  GetMessage(timeout: DWORD; var flags: TGpMQMessageFlags;
      var msg: UINT; var wParam: WPARAM; var lParam: LPARAM;
      var msgData: string): TGpMQGetStatus; overload;
    function  GetMessage(timeout: DWORD; var msg: UINT;
      var msgData: string): TGpMQGetStatus; overload;
    function  GetMessage(timeout: DWORD; var msg: UINT; var wParam: WPARAM;
      var lParam: LPARAM): TGpMQGetStatus; overload;
    function  PeekMessage(timeout: DWORD; var flags: TGpMQMessageFlags;
      var msg: UINT; var wParam: WPARAM; var lParam: LPARAM;
      var msgData: string): TGpMQGetStatus; overload;
    function  PeekMessage(timeout: DWORD; var msg: UINT;
      var wParam: WPARAM; var lParam: LPARAM): TGpMQGetStatus; overload;
    function  PeekMessage(timeout: DWORD; var msg: UINT;
      var msgData: string): TGpMQGetStatus; overload;
    function  PeekMessage(timeout: DWORD; var msgData: string): TGpMQGetStatus; overload;
    property Active: boolean read FActive write SetActive;
  published
    property MessageQueueName: string read FMessageQueueName write SetMessageQueueName;
    property Size: cardinal read FSize write SetSize;
    property Timeout: cardinal read FTimeout write FTimeout default 1000;
    property OnNewMessageNotify: TGpMQReaderNewMessageNotifyEvent
      read FOnNewMessageNotify write FOnNewMessageNotify;
    property OnNewMessage: TGpMWReaderNewMessageEvent
      read FOnNewMessage write FOnNewMessage;
  end; { TGpMessageQueueReaderComp }

  {:TGpMessageQueueWriter wrapper.
    @since   2002-10-23
  }
  TGpMessageQueueWriterComp = class(TGpMessageQueueComp)
  private
    FActive            : boolean;
    FMessageQueueName  : string;
    FMessageQueueWriter: TGpMessageQueueWriter;
    FSize              : cardinal;
    FTimeout           : cardinal;
  protected
    procedure CheckMQExists; override;
    procedure CreateWriter; virtual;
    function  MessageQueueObject: TGpMessageQueue; override;
    procedure SetActive(const Value: boolean); virtual;
    procedure SetMessageQueueName(const Value: string); virtual;
    procedure SetSize(const Value: cardinal); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  IsReaderAlive: boolean;
    property Active: boolean read FActive write SetActive;
  published
    property MessageQueueName: string read FMessageQueueName write SetMessageQueueName;
    property Size: cardinal read FSize write SetSize;
    property Timeout: cardinal read FTimeout write FTimeout default 1000;
  end; { TGpMessageQueueWriterComp }

procedure Register;

implementation

uses
  DSiWin32;

resourcestring
  sCannotAllocHWND                     = '%s: Failed to allocate window.';
  sCountedGroupNameIsNotSet            = '%s: Counted group name is not set.';
  sGroupNameIsNotSet                   = '%s: Group name is not set.';
  sInternalErrorCountedGroupObjectIs   = '%s: Internal error. Counted group object %d is destroyed.';
  sInternalErrorGroupObjectIsDestroyed = '%s: Internal error. Group object %s is destroyed.';
  sInternalErrorMQObjectIsDestroyed    = '%s: Internal error. Message queue object %s is destroyed.';
  sInternalErrorSWMRObjectIsDestroyed  = '%s: Internal error. SWMR object %s is destroyed.';
  sMQNameIsNotSet                      = '%s: Message queue name is not set.';
  sMQSizeIsNotSet                      = '%s: Message queue size is not set.';
  sSWMRnameIsNotSet                    = '%s: SWMR name is not set.';

procedure Register;
begin
  RegisterComponents('Gp', [TGpFlagComp, TGpTokenComp, TGpCounterComp, TGpGroupComp,
    TGpCountedGroupComp, TGpSWMRComp, TGpMessageQueueReaderComp,
    TGpMessageQueueWriterComp]);
end; { Register }

{ TGpFlagComp }

destructor TGpFlagComp.Destroy;
begin
  FreeAndNil(FFlag);
  inherited;
end; { TGpFlagComp.Destroy }

function TGpFlagComp.GetIsSet: boolean;
begin
  if assigned(FFlag) then
    Result := FFlag.IsSet
  else
    Result := false;
end; { TGpFlagComp.GetIsSet }

procedure TGpFlagComp.IgnoreOpFailed(const Value: boolean);
begin
  // read-only property
end; { TGpFlagComp.IgnoreOpFailed }

function TGpFlagComp.IsFlagSet(const flagName: string): boolean;
begin
  Result := TGpFlag.IsFlagSet(flagName);
end; { TGpFlagComp.IsFlagSet }

procedure TGpFlagComp.SetFlagName(const Value: string);
var
  oldIsSet: boolean;
begin
  if Value <> FlagName then begin
    FFlagName := Value;
    if not (csDesigning in ComponentState) then begin
      oldIsSet := IsSet;
      if assigned(FFlag) then
        FreeAndNil(FFlag);
      if Value <> '' then begin
        FFlag := TGpFlag.Create(Value);
        FOpFailed := FFlag.CreateFailed;
        if FOpFailed then 
          FreeAndNil(FFlag)
        else if oldIsSet then
          IsSet := oldIsSet;
      end;
    end;
  end;
end; { TGpFlagComp.SetFlagName }

procedure TGpFlagComp.SetIsSet(const Value: boolean);
begin
  if assigned(FFlag) then
    if Value then
      FOpFailed := not FFlag.SetFlag
    else
      FFlag.ClearFlag;
end; { TGpFlagComp.SetIsSet }

{ TGpTokenComp }

constructor TGpTokenComp.Create(AOwner: TComponent);
begin
  inherited;
  FToken := TGpToken.Create;
end; { TGpTokenComp.Create }

destructor TGpTokenComp.Destroy;
begin
  FreeAndNil(FToken);
  inherited;
end; { TGpTokenComp.Destroy }

function TGpTokenComp.GetIsPublished: boolean;
begin
  Result := FToken.IsPublished;
end; { TGpTokenComp.GetIsPublished }

function TGpTokenComp.GetTokenName: string;
begin
  Result := FToken.Token;
end; { TGpTokenComp.GetTokenName }

procedure TGpTokenComp.IgnoreSetTokenName(const Value: string);
begin
  // read-only property
end; { TGpTokenComp.IgnoreSetTokenName }

function TGpTokenComp.IsTokenPublished(token: string): boolean;
begin
  Result := TGpToken.IsTokenPublished(token);
end; { TGpTokenComp.IsTokenPublished }

procedure TGpTokenComp.Publish;
begin
  FToken.Publish;
end; { TGpTokenComp.Publish }

procedure TGpTokenComp.Revoke;
begin
  FToken.Revoke;
end; { TGpTokenComp.Revoke }

procedure TGpTokenComp.SetIsPublished(const Value: boolean);
begin
  if Value <> IsPublished then begin
    if Value then
      Publish
    else
      Revoke;
  end;
end; { TGpTokenComp.SetIsPublished }

{ TGpCounterComp }

constructor TGpCounterComp.Create(AOwner: TComponent);
begin
  inherited;
  RecreateCounter(CounterName, Value);
end; { TGpCounterComp.Create }

procedure TGpCounterComp.Dec(decrement: integer);
begin
  FCounter.Dec(decrement);
end; { TGpCounterComp.Dec }

destructor TGpCounterComp.Destroy;
begin
  FreeAndNil(FCounter);
  inherited;
end; { TGpCounterComp.Destroy }

function TGpCounterComp.GetCounterName: string;
begin
  if not assigned(FCounter) then
    Result := ''
  else
    Result := FCounter.Name;
end; { TGpCounterComp.GetCounterName }

function TGpCounterComp.GetValue: integer;
begin
  if not assigned(FCounter) then
    Result := 0
  else
    Result := FCounter.Value;
end; { TGpCounterComp.GetValue }

procedure TGpCounterComp.Inc(increment: integer);
begin
  FCounter.Inc(increment);
end; { TGpCounterComp.Inc }      

procedure TGpCounterComp.RecreateCounter(const name: string; initialValue: integer);
begin
  FCounter.Free;
  FCounter := TGpCounter.Create(name, initialValue);
end; { TGpCounterComp.RecreateCounter }

procedure TGpCounterComp.SetCounterName(const Value: string);
begin
  if (not assigned(FCounter)) or (Value <> FCounter.Name) then
    RecreateCounter(Value, self.Value);
end; { TGpCounterComp.SetCounterName }

procedure TGpCounterComp.SetValue(const Value: integer);
begin
  RecreateCounter(CounterName, Value);
end; { TGpCounterComp.SetValue }

{ TGpGroupComp }

procedure TGpGroupComp.CheckNameSet;
begin
  if GroupName = '' then
    raise Exception.CreateFmt(sGroupNameIsNotSet,[Name])
  else if not assigned(FGroup) then
    raise Exception.CreateFmt(sInternalErrorGroupObjectIsDestroyed,[Name, GroupName]);
end; { TGpGroupComp.CheckNameSet }

destructor TGpGroupComp.Destroy;
begin
  FreeAndNil(FGroup);
  inherited;
end; { TGpGroupComp.Destroy }

function TGpGroupComp.GetGroupName: string;
begin
  if assigned(FGroup) then
    Result := FGroup.Name
  else
    Result := '';
end; { TGpGroupComp.GetGroupName }

function TGpGroupComp.GetIsEmpty: boolean;
begin
  if assigned(FGroup) then
    Result := FGroup.IsEmpty
  else
    Result := true;
end; { TGpGroupComp.GetIsEmpty }

function TGpGroupComp.GetIsMember: boolean;
begin
  if assigned(FGroup) then
    Result := FGroup.IsMember
  else
    Result := false;
end; { TGpGroupComp.GetIsMember }

procedure TGpGroupComp.IgnoreSetIsEmpty(const Value: boolean);
begin
  // read-only property
end; { TGpGroupComp.IgnoreSetIsEmpty }

procedure TGpGroupComp.Join;
begin
  CheckNameSet;
  FGroup.Join;
end; { TGpGroupComp.Join }

procedure TGpGroupComp.Join(var isFirstMember: boolean);
begin
  CheckNameSet;
  FGroup.Join(isFirstMember);
end; { TGpGroupComp.Join }

procedure TGpGroupComp.Leave;
begin
  CheckNameSet;
  FGroup.Leave;
end; { TGpGroupComp.Leave }

procedure TGpGroupComp.Leave(var wasLastMember: boolean);
begin
  CheckNameSet;
  FGroup.Leave(wasLastMember);
end; { TGpGroupComp.Leave }

procedure TGpGroupComp.SetGroupName(const Value: string);
begin
  if Value <> GroupName then begin
    if assigned(FGroup) then
      FreeAndNil(FGroup);
    if Value <> '' then
      FGroup := TGpGroup.Create(Value);
  end;
end; { TGpGroupComp.SetGroupName }

procedure TGpGroupComp.SetIsMember(const Value: boolean);
begin
  if Value <> IsMember then begin
    if Value then
      Join
    else
      Leave;
  end;
end; { TGpGroupComp.SetIsMember }

{ TGpCountedGroupComp }

procedure TGpCountedGroupComp.CheckNameSet;
begin
  if CountedGroupName = '' then
    raise Exception.CreateFmt(sCountedGroupNameIsNotSet,[Name])
  else if not assigned(FCountedGroup) then
    raise Exception.CreateFmt(sInternalErrorCountedGroupObjectIs,[Name, CountedGroupName]);
end; { TGpCountedGroupComp.CheckNameSet }

destructor TGpCountedGroupComp.Destroy;
begin
  FreeAndNil(FCountedGroup);
  inherited;
end; { TGpCountedGroupComp.Destroy }

function TGpCountedGroupComp.GetCountedGroupName: string;
begin
  if assigned(FCountedGroup) then
    Result := FCountedGroup.Name
  else
    Result := '';
end; { TGpCountedGroupComp.GetCountedGroupName }

function TGpCountedGroupComp.GetIsEmpty: boolean;
begin
  if assigned(FCountedGroup) then
    Result := FCountedGroup.IsEmpty
  else
    Result := true;
end; { TGpCountedGroupComp.GetIsEmpty }

function TGpCountedGroupComp.GetIsMember: boolean;
begin
  if assigned(FCountedGroup) then
    Result := FCountedGroup.IsMember
  else
    Result := false;
end; { TGpCountedGroupComp.GetIsMember }

function TGpCountedGroupComp.GetMemberLimit: integer;
begin
  if assigned(FCountedGroup) then
    Result := FCountedGroup.MemberLimit
  else
    Result := FMemberLimit;
end; { TGpCountedGroupComp.GetMemberLimit }

function TGpCountedGroupComp.GetNumMembers: integer;
begin
  if assigned(FCountedGroup) then
    Result := FCountedGroup.NumMembers
  else
    Result := 0;
end; { TGpCountedGroupComp.GetNumMembers }

procedure TGpCountedGroupComp.IgnoreSetIsEmpty(const Value: boolean);
begin
  // read-only property
end; { TGpCountedGroupComp.IgnoreSetIsEmpty }

procedure TGpCountedGroupComp.IgnoreSetNumMembers(const Value: integer);
begin
  // read-only property
end; { TGpCountedGroupComp.IgnoreSetNumMembers }

function TGpCountedGroupComp.Join(timeout: DWORD): boolean;
begin
  CheckNameSet;
  Result := FCountedGroup.Join(timeout);
end; { TGpCountedGroupComp.Join }

function TGpCountedGroupComp.Join(timeout: DWORD;
  var isFirstMember: boolean): boolean;
begin
  CheckNameSet;
  Result := FCountedGroup.Join(timeout, isFirstMember);
end; { TGpCountedGroupComp.Join }

procedure TGpCountedGroupComp.Leave;
begin
  CheckNameSet;
  FCountedGroup.Leave;
end; { TGpCountedGroupComp.Leave }

procedure TGpCountedGroupComp.Leave(var wasLastMember: boolean);
begin
  CheckNameSet;
  FCountedGroup.Leave(wasLastMember);
end; { TGpCountedGroupComp.Leave }

procedure TGpCountedGroupComp.SetCountedGroupName(const Value: string);
begin
  if Value <> CountedGroupName then begin
    FreeAndNil(FCountedGroup);
    if Value <> '' then
      FCountedGroup := TGpCountedGroup.Create(Value, FMemberLimit);
  end;
end; { TGpCountedGroupComp.SetCountedGroupName }

procedure TGpCountedGroupComp.SetIsMember(const Value: boolean);
begin
  if Value <> IsMember then begin
    if Value then
      Join(0)
    else
      while IsMember do
        Leave;
  end;
end; { TGpCountedGroupComp.SetIsMember }

procedure TGpCountedGroupComp.SetMemberLimit(const Value: integer);
var
  oldName: string;
begin
  if Value <> MemberLimit then begin
    oldName := CountedGroupName;
    CountedGroupName := '';
    FMemberLimit := Value;
    CountedGroupName := oldName;
  end;
end; { TGpCountedGroupComp.SetMemberLimit }

{ TGpSWMRComp }

procedure TGpSWMRComp.CheckNameSet;
begin
  if SWMRName = '' then
    raise Exception.CreateFmt(sSWMRnameIsNotSet,[Name])
  else if not assigned(FSWMR) then
    raise Exception.CreateFmt(sInternalErrorSWMRobjectIsDestroyed,[Name, SWMRName]);
end; { TGpSWMRComp.CheckNameSet }

destructor TGpSWMRComp.Destroy;
begin
  FreeAndNil(FSWMR);
  inherited;
end; { TGpSWMRComp.Destroy }

procedure TGpSWMRComp.Done;
begin
  CheckNameSet;
  FSWMR.Done;
end; { TGpSWMRComp.Done }

function TGpSWMRComp.GetAccess: TGpSWMRAccess;
begin
  if assigned(FSWMR) then
    Result := FSWMR.Access
  else
    Result := swmrAccNone;
end; { TGpSWMRComp.GetAccess }

function TGpSWMRComp.GetSWMRName: string;
begin
  if assigned(FSWMR) then
    Result := FSWMR.Name
  else
    Result := '';
end; { TGpSWMRComp.GetSWMRName }

procedure TGpSWMRComp.SetAccess(const Value: TGpSWMRAccess);
begin
  if Value <> Access then begin
    while Access <> swmrAccNone do
      Done;
    if Value = swmrAccRead then
      WaitToRead(0)
    else if Value = swmrAccWrite then
      WaitToWrite(0);
  end;
end; { TGpSWMRComp.SetAccess }

procedure TGpSWMRComp.SetSWMRName(const Value: string);
begin
  if Value <> SWMRName then begin
    FreeAndNil(FSWMR);
    if Value <> '' then
      FSWMR := TGpSWMR.Create(Value);
  end;
end; { TGpSWMRComp.SetSWMRName }

function TGpSWMRComp.WaitToRead(timeout: DWORD): boolean;
begin
  CheckNameSet;
  Result := FSWMR.WaitToRead(timeout);
end; { TGpSWMRComp.WaitToRead }

function TGpSWMRComp.WaitToWrite(timeout: DWORD): boolean;
begin
  CheckNameSet;
  Result := FSWMR.WaitToWrite(timeout);
end; { TGpSWMRComp.WaitToWrite }

{ TGpMessageQueueComp }

function TGpMessageQueueComp.PostMessage(timeout: DWORD;
  flags: TGpMQMessageFlags; msg: UINT; wParam: WPARAM; lParam: LPARAM;
  const msgData: string): TGpMQPostStatus;
begin
  CheckMQExists;
  Result := MessageQueueObject.PostMessage(timeout, flags, msg, wParam, lParam, msgData);
end; { TGpMessageQueueComp.PostMessage }

function TGpMessageQueueComp.PostMessage(timeout: DWORD;
  const msgData: string): TGpMQPostStatus;
begin
  CheckMQExists;
  Result := MessageQueueObject.PostMessage(timeout, msgData);
end; { TGpMessageQueueComp.PostMessage }

function TGpMessageQueueComp.PostMessage(timeout: DWORD; msg: UINT;
  wParam: WPARAM; lParam: LPARAM): TGpMQPostStatus;
begin
  CheckMQExists;
  Result := MessageQueueObject.PostMessage(timeout, msg, wParam, lParam);
end; { TGpMessageQueueComp.PostMessage }

function TGpMessageQueueComp.PostMessage(timeout: DWORD; msg: UINT;
  const msgData: string): TGpMQPostStatus;
begin
  CheckMQExists;
  Result := MessageQueueObject.PostMessage(timeout, msg, msgData);
end; { TGpMessageQueueComp.PostMessage }

{ TGpMessageQueueReaderComp }

const
  WM_MQ_NEWMESSAGE = WM_USER;
  WM_MQ_RETRYGET = WM_USER+1;

procedure TGpMessageQueueReaderComp.CheckMQExists;
begin
  if FMessageQueueName = '' then
    raise Exception.CreateFmt(sMQNameIsNotSet, [Name])
  else if FSize <= 0 then
    raise Exception.CreateFmt(sMQSizeIsNotSet, [Name])
  else if not assigned(FMessageQueueReader) then
    raise Exception.CreateFmt(sInternalErrorMQObjectIsDestroyed, [Name, MessageQueueName]);
end; { TGpMessageQueueReaderComp.CheckMQExists }

constructor TGpMessageQueueReaderComp.Create(AOwner: TComponent);
begin
  inherited;
  FTimeout := 1000;
  FReaderWindowHandle := DSiAllocateHWND(ReaderWindowProc);
  if FReaderWindowHandle = 0 then
    raise Exception.CreateFmt(sCannotAllocHWND, [Name]);
end; { TGpMessageQueueReaderComp.Create }

procedure TGpMessageQueueReaderComp.CreateReader;
begin
  if not (csDesigning in ComponentState) then begin
    FreeAndNil(FMessageQueueReader);
    if FActive and (FMessageQueueName <> '') and (FSize > 0) then
      try
        FMessageQueueReader := TGpMessageQueueReader.Create(FMessageQueueName, FSize,
          FReaderWindowHandle, WM_MQ_NEWMESSAGE);
      except
        FActive := false;
      end;
  end;
end; { TGpMessageQueueReaderComp.CreateReader }

destructor TGpMessageQueueReaderComp.Destroy;
begin
  if FReaderWindowHandle <> 0 then begin
    DSiDeallocateHWND(FReaderWindowHandle);
    FReaderWindowHandle := 0;
  end;
  FreeAndNil(FMessageQueueReader);
  inherited;
end; { TGpMessageQueueReaderComp.Destroy }

function TGpMessageQueueReaderComp.GetMessage(timeout: DWORD;
  var msg: UINT; var msgData: string): TGpMQGetStatus;
begin
  CheckMQExists;
  Result := FMessageQueueReader.GetMessage(timeout, msg, msgData);
end; { TGpMessageQueueReaderComp.GetMessage }

procedure TGpMessageQueueReaderComp.DefaultHandler(var Message);
begin
  inherited;
end; { TGpMessageQueueReaderComp.DefaultHandler }

function TGpMessageQueueReaderComp.GetMessage(timeout: DWORD;
  var msg: UINT; var wParam: WPARAM; var lParam: LPARAM): TGpMQGetStatus;
begin
  CheckMQExists;
  Result := FMessageQueueReader.GetMessage(timeout, msg, wParam, lParam);
end; { TGpMessageQueueReaderComp.GetMessage }

function TGpMessageQueueReaderComp.GetMessage(timeout: DWORD;
  var msgData: string): TGpMQGetStatus;
begin
  CheckMQExists;
  Result := FMessageQueueReader.GetMessage(timeout, msgData);
end; { TGpMessageQueueReaderComp.GetMessage }

function TGpMessageQueueReaderComp.GetMessage(timeout: DWORD;
  var flags: TGpMQMessageFlags; var msg: UINT; var wParam: WPARAM;
  var lParam: LPARAM; var msgData: string): TGpMQGetStatus;
begin
  CheckMQExists;
  Result := FMessageQueueReader.GetMessage(timeout, flags, msg, wParam, lParam, msgData);
end; { TGpMessageQueueReaderComp.GetMessage }

function TGpMessageQueueReaderComp.PeekMessage(timeout: DWORD;
  var msg: UINT; var msgData: string): TGpMQGetStatus;
begin
  CheckMQExists;
  Result := FMessageQueueReader.PeekMessage(timeout, msg, msgData);
end; { TGpMessageQueueReaderComp.PeekMessage }

function TGpMessageQueueReaderComp.PeekMessage(timeout: DWORD;
  var flags: TGpMQMessageFlags; var msg: UINT; var wParam: WPARAM;
  var lParam: LPARAM; var msgData: string): TGpMQGetStatus;
begin
  CheckMQExists;
  Result := FMessageQueueReader.PeekMessage(timeout, flags, msg, wParam, lParam, msgData);
end; { TGpMessageQueueReaderComp.PeekMessage }

function TGpMessageQueueReaderComp.PeekMessage(timeout: DWORD;
  var msgData: string): TGpMQGetStatus;
begin
  CheckMQExists;
  Result := FMessageQueueReader.PeekMessage(timeout, msgData);
end; { TGpMessageQueueReaderComp.PeekMessage }

function TGpMessageQueueReaderComp.PeekMessage(timeout: DWORD;
  var msg: UINT; var wParam: WPARAM; var lParam: LPARAM): TGpMQGetStatus;
begin
  CheckMQExists;
  Result := FMessageQueueReader.PeekMessage(timeout, msg, wParam, lParam);
end; { TGpMessageQueueReaderComp.PeekMessage }

procedure TGpMessageQueueReaderComp.ProcessNewMessages(doNotify: boolean);
var
  flags    : TGpMQMessageFlags;
  getStatus: TGpMQGetStatus;
  lParam   : Windows.LPARAM;
  msg      : UINT;
  msgData  : string;
  wParam   : Windows.WPARAM;
begin
  if doNotify and assigned(FOnNewMessageNotify) then
    FOnNewMessageNotify(self);
  if assigned(FOnNewMessage) then begin
    repeat
      getStatus := FMessageQueueReader.GetMessage(FTimeout, flags, msg, wParam, lParam, msgData);
      if getStatus = mqgOK then 
        FOnNewMessage(self, flags, msg, wParam, lParam, msgData)
      else if getStatus = mqgTimeout then
        PostMessage(FReaderWindowHandle, WM_MQ_RETRYGET, 0, 0);
    until (getStatus <> mqgOK) or (csDestroying in ComponentState);
  end;
end; { TGpMessageQueueReaderComp.ProcessNewMessages }

procedure TGpMessageQueueReaderComp.ReaderWindowProc(var msg: TMessage);
begin
  if msg.msg = WM_MQ_NEWMESSAGE then
    ProcessNewMessages(true)
  else if msg.msg = WM_MQ_RETRYGET then
    ProcessNewMessages(false)
  else
    msg.Result := DefWindowProc(FReaderWindowHandle, msg.msg, msg.wParam, msg.lParam);
end; { TGpMessageQueueReaderComp.ReaderWindowProc }

procedure TGpMessageQueueReaderComp.SetActive(const Value: boolean);
begin
  if Value <> FActive then begin
    FActive := Value;
    CreateReader;
  end;
end; { TGpMessageQueueReaderComp.SetActive }

procedure TGpMessageQueueReaderComp.SetMessageQueueName(
  const Value: string);
begin
  if Value <> FMessageQueueName then begin
    FMessageQueueName := Value;
    CreateReader;
  end;
end; { TGpMessageQueueReaderComp.SetMessageQueueName }

procedure TGpMessageQueueReaderComp.SetSize(const Value: cardinal);
begin
  if Value <> FSize then begin
    FSize := Value;
    CreateReader;
  end;
end; { TGpMessageQueueReaderComp.SetSize }

function TGpMessageQueueReaderComp.MessageQueueObject: TGpMessageQueue;
begin
  Result := FMessageQueueReader;
end; { TGpMessageQueueReaderComp.MessageQueueObject }

{ TGpMessageQueueWriterComp }

procedure TGpMessageQueueWriterComp.CheckMQExists;
begin
  if FMessageQueueName = '' then
    raise Exception.CreateFmt(sMQNameIsNotSet, [Name])
  else if FSize <= 0 then
    raise Exception.CreateFmt(sMQSizeIsNotSet, [Name])
  else if not assigned(FMessageQueueWriter) then
    raise Exception.CreateFmt(sInternalErrorMQObjectIsDestroyed, [Name, MessageQueueName]);
end; { TGpMessageQueueWriterComp.CreateWriter }

constructor TGpMessageQueueWriterComp.Create(AOwner: TComponent);
begin
  inherited;
  FTimeout := 1000;
end; { TGpMessageQueueWriterComp.Create }

procedure TGpMessageQueueWriterComp.CreateWriter;
begin
  if not (csDesigning in ComponentState) then begin
    FreeAndNil(FMessageQueueWriter);
    if FActive and (FMessageQueueName <> '') and (FSize > 0) then
      FMessageQueueWriter := TGpMessageQueueWriter.Create(FMessageQueueName, FSize);
  end;
end; { TGpMessageQueueWriterComp.CreateWriter }

destructor TGpMessageQueueWriterComp.Destroy;
begin
  FreeAndNil(FMessageQueueWriter);
  inherited;
end; { TGpMessageQueueWriterComp.Destroy }

function TGpMessageQueueWriterComp.IsReaderAlive: boolean;
begin
  CheckMQExists;
  Result := FMessageQueueWriter.IsReaderAlive;
end; { TGpMessageQueueWriterComp.IsReaderAlive }

function TGpMessageQueueWriterComp.MessageQueueObject: TGpMessageQueue;
begin
  Result := FMessageQueueWriter;
end; { TGpMessageQueueWriterComp.MessageQueueObject }

procedure TGpMessageQueueWriterComp.SetActive(const Value: boolean);
begin
  if Value <> FActive then begin
    FActive := Value;
    CreateWriter;
  end;
end; { TGpMessageQueueWriterComp.SetActive }

procedure TGpMessageQueueWriterComp.SetMessageQueueName(
  const Value: string);
begin
  if Value <> FMessageQueueName then begin
    FMessageQueueName := Value;
    CreateWriter;
  end;
end; { TGpMessageQueueWriterComp.SetMessageQueueName }

procedure TGpMessageQueueWriterComp.SetSize(const Value: cardinal);
begin
  if Value <> FSize then begin
    FSize := Value;
    CreateWriter;
  end;
end; { TGpMessageQueueWriterComp.SetSize }

end.
