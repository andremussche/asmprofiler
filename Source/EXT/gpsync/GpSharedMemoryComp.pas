{:Component wrappers for the GpSharedMemory unit.
  (c) 2002 Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2003, Primoz Gabrijelcic
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

  Author           : Primoz Gabrijelcic
  Creation date    : 2002-10-02
  Last modification: 2002-10-02
  Version          : 1.0

  </pre>}{

  History:
    1.0:
      - Released.
}
unit GpSharedMemoryComp;

interface

uses
  Windows,
  Classes,
  GpSharedMemory;

type
  {:Access to the shared memory object.
    @since   2002-10-02
  }
  TGpSharedMemoryAccess = (accNone, accRead, accWrite);

  {:TGpSharedMemory wrapper.
    @since   2002-10-02
  }
  TGpSharedMemoryComp = class(TComponent)
  private
    FAccess          : TGpSharedMemoryAccess;
    FActive          : boolean;
    FInitialSize     : cardinal;
    FMaxSize         : cardinal;
    FSharedMemory    : TGpSharedMemory;
    FSharedMemoryName: string;
    FTimeout         : DWORD;
  protected
    procedure CheckAccessibility(caller: string); virtual;
    procedure CheckNotActive(caller: string); virtual;
    function  GetAcquired: boolean; virtual;
    function  GetAsStream: TGpSharedStream; virtual;
    function  GetAsString: string; virtual;
    function  GetByte(byteOffset: integer): byte; virtual;
    function  GetByteIdx(idx: integer): byte; virtual;
    function  GetDataPointer: pointer; virtual;
    function  GetHuge(byteOffset: integer): int64; virtual;
    function  GetHugeIdx(idx: integer): int64; virtual;
    function  GetIsResizable: boolean; virtual;
    function  GetLong(byteOffset: integer): longword; virtual;
    function  GetLongIdx(idx: integer): longword; virtual;
    function  GetModified: boolean; virtual;
    function  GetSize: cardinal; virtual;
    function  GetWasCreated: boolean; virtual;
    function  GetWord(byteOffset: integer): word; virtual;
    function  GetWordIdx(idx: integer): word; virtual;
    procedure IgnoreSetAcquired(const Value: boolean); virtual;
    procedure IgnoreSetIsResizable(const Value: boolean); virtual;
    procedure IgnoreSetModified(const Value: boolean); virtual;
    procedure IgnoreSetWasCreated(const Value: boolean);
    procedure SetAccess(Value: TGpSharedMemoryAccess); virtual;
    procedure SetActive(const Value: boolean); virtual;
    procedure SetAsString(const Value: string); virtual;
    procedure SetByte(byteOffset: integer; const Value: byte); virtual;
    procedure SetByteIdx(idx: integer; const Value: byte); virtual;
    procedure SetHuge(byteOffset: integer; const Value: int64); virtual;
    procedure SetHugeIdx(idx: integer; const Value: int64); virtual;
    procedure SetInitialSize(const Value: cardinal); virtual;
    procedure SetLong(byteOffset: integer; const Value: longword); virtual;
    procedure SetLongIdx(idx: integer; const Value: longword); virtual;
    procedure SetMaxSize(const Value: cardinal); virtual;
    procedure SetSharedMemoryName(const Value: string); virtual;
    procedure SetSize(const Value: cardinal); virtual;
    procedure SetWord(byteOffset: integer; const Value: word); virtual;
    procedure SetWordIdx(idx: integer; const Value: word); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    function  AcquireMemory(forWriting: boolean; timeout: DWORD): pointer;
    procedure ReleaseMemory;
    property AsStream: TGpSharedStream read GetAsStream;
    property Byte[byteOffset: integer]: byte read GetByte write SetByte;
    property ByteIdx[idx: integer]: byte read GetByteIdx write SetByteIdx;
    property DataPointer: pointer read GetDataPointer;
    property Huge[byteOffset: integer]: int64 read GetHuge write SetHuge;
    property HugeIdx[idx: integer]: int64 read GetHugeIdx write SetHugeIdx;
    property Long[byteOffset: integer]: longword read GetLong write SetLong;
    property LongIdx[idx: integer]: longword read GetLongIdx write SetLongIdx;
    property Word[byteOffset: integer]: word read GetWord write SetWord;
    property WordIdx[idx: integer]: word read GetWordIdx write SetWordIdx;
  published 
    property Access: TGpSharedMemoryAccess read FAccess write SetAccess;
    property Acquired: boolean read GetAcquired write IgnoreSetAcquired;
    property AsString: string read GetAsString write SetAsString;
    property InitialSize: cardinal read FInitialSize write SetInitialSize;
    property IsResizable: boolean read GetIsResizable write IgnoreSetIsResizable;
    property MaxSize: cardinal read FMaxSize write SetMaxSize;
    property Modified: boolean read GetModified write IgnoreSetModified;
    property SharedMemoryName: string read FSharedMemoryName write SetSharedMemoryName;
    property Size: cardinal read GetSize write SetSize;
    property Timeout: DWORD read FTimeout write FTimeout;
    property WasCreated: boolean read GetWasCreated write IgnoreSetWasCreated;
    property Active: boolean read FActive write SetActive; // must be last!
  end; { TGpSharedMemoryComp }

  {:TGpSharedPool reader wrapper.
    @since   2002-11-11
  }
  TGpSharedPoolReaderComp = class(TComponent)
  private
    FActive           : boolean;
    FInitialBufferSize: cardinal;
    FMaxBufferSize    : cardinal;
    FMaxNumBuffers    : cardinal;
    FMinNumBuffers    : cardinal;
    FOnDataReceived   : TGpSharedPoolDataReceivedNotify;
    FOnResized        : TGpSharedPoolResizedNotify;
    FResizeIncrement  : cardinal;
    FResizeThreshold  : cardinal;
    FSharedPoolName   : string;
    FSharedPoolReader : TGpSharedPoolReader;
    FStartNumBuffers  : cardinal;
    FSweepTimeoutSec  : cardinal;
  protected
    procedure CheckAccessibility(caller: string); virtual;
    procedure CheckNotActive(caller: string); virtual;
    function  GetLastError: TGpSharedPoolError; virtual;
    procedure SetActive(Value: boolean); virtual;
    procedure SetInitialBufferSize(const Value: cardinal); virtual;
    procedure SetMaxBufferSize(const Value: cardinal); virtual;
    procedure SetMaxNumBuffers(const Value: cardinal); virtual;
    procedure SetMinNumBuffers(const Value: cardinal); virtual;
    procedure SetResizeIncrement(const Value: cardinal); virtual;
    procedure SetSharedPoolName(const Value: string); virtual;
    procedure SetStartNumBuffers(const Value: cardinal); virtual;
    procedure SetSweepTimeoutSec(const Value: cardinal); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    function  AcquireBuffer(timeout: DWORD): TGpSharedMemory;
    function  AcquireCopy(shm: TGpSharedMemory; timeout: DWORD): TGpSharedMemory;
    function  GetNextReceived: TGpSharedMemory;
    procedure ReleaseBuffer(var shm: TGpSharedMemory);
    function  SendBuffer(var shm: TGpSharedMemory): boolean;
    property LastError: TGpSharedPoolError read GetLastError;
  published
    property InitialBufferSize: cardinal
      read FInitialBufferSize write SetInitialBufferSize;
    property MaxBufferSize: cardinal
      read FMaxBufferSize write SetMaxBufferSize;
    property MaxNumBuffers: cardinal
      read FMaxNumBuffers write SetMaxNumBuffers;
    property MinNumBuffers: cardinal
      read FMinNumBuffers write SetMinNumBuffers
      default CDefMinNumBuffers;
    property ResizeIncrement: cardinal
      read FResizeIncrement write SetResizeIncrement
      default CDefResizeIncrement;
    property ResizeThreshold: cardinal
      read FResizeThreshold
      default CDefResizeThreshold;
    property SharedPoolName: string
      read FSharedPoolName write SetSharedPoolName;
    property StartNumBuffers: cardinal
      read FStartNumBuffers write SetStartNumBuffers;
    property SweepTimeoutSec: cardinal
      read FSweepTimeoutSec write SetSweepTimeoutSec
      default CDefSweepTimeoutSec;
    property Active: boolean read FActive write SetActive ; // must be last!
  //events
    property OnDataReceived: TGpSharedPoolDataReceivedNotify
      read FOnDataReceived write FOnDataReceived;
    property OnResized: TGpSharedPoolResizedNotify
      read FOnResized write FOnResized;
  end; { TGpSharedPoolReaderComp }

  {:TGpSharedPoolWriter wrapper.
    @since   2002-11-11
  }
  TGpSharedPoolWriterComp = class(TComponent)
  private
    FActive          : boolean;
    FSharedPoolName  : string;
    FSharedPoolWriter: TGpSharedPoolWriter;
  protected
    procedure CheckAccessibility(caller: string); virtual;
    procedure CheckNotActive(caller: string); virtual;
    function  GetLastError: TGpSharedPoolError; virtual;
    procedure SetActive(const Value: boolean); virtual;
    procedure SetSharedPoolName(const Value: string); virtual;
  public
    function  AcquireBuffer(timeout: DWORD): TGpSharedMemory;
    function  AcquireCopy(shm: TGpSharedMemory; timeout: DWORD): TGpSharedMemory;
    procedure ReleaseBuffer(var shm: TGpSharedMemory);
    function  SendBuffer(var shm: TGpSharedMemory): boolean;
    property LastError: TGpSharedPoolError read GetLastError;
  published
    property SharedPoolName: string
      read FSharedPoolName write SetSharedPoolName;
    property Active: boolean read FActive write SetActive; // must be last!
  end; { TGpSharedPoolWriterComp }

procedure Register;

implementation

uses
  SysUtils;

resourcestring
  sSharedMemoryIsActive    = '%s: Shared memory is active: %s';
  sSharedMemoryIsNotActive = '%s: Shared memory is not active: %s';
  sSharedPoolIsActive      = '%s: Shared pool is active: %s';
  sSharedPoolIsNotActive   = '%s: Shared pool is not active: %s';

procedure Register;
begin
  RegisterComponents('Gp', [TGpSharedMemoryComp, TGpSharedPoolReaderComp,
    TGpSharedPoolWriterComp]);
end; { Register }

{ TGpSharedMemoryComp }

function TGpSharedMemoryComp.AcquireMemory(forWriting: boolean; timeout: DWORD): pointer;
begin
  CheckAccessibility('TGpSharedMemoryComp.AcquireMemory');
  Result := FSharedMemory.AcquireMemory(forWriting, timeout);
  if assigned(Result) then
    if forWriting then
      FAccess := accWrite
    else
      FAccess := accRead;
end; { TGpSharedMemoryComp.AcquireMemory }

procedure TGpSharedMemoryComp.CheckAccessibility(caller: string);
begin
  if not assigned(FSharedMemory) then
    raise Exception.CreateFmt(sSharedMemoryIsNotActive, [caller, SharedMemoryName]);
end; { TGpSharedMemoryComp.CheckAccessibility }

procedure TGpSharedMemoryComp.CheckNotActive(caller: string);
begin
  if assigned(FSharedMemory) then
    raise Exception.CreateFmt(sSharedMemoryIsActive, [caller, SharedMemoryName]);
end; { TGpSharedMemoryComp.CheckNotActive }

constructor TGpSharedMemoryComp.Create(AOwner: TComponent);
begin
  inherited;
end; { TGpSharedMemoryComp.Create }

destructor TGpSharedMemoryComp.Destroy;
begin
  FreeAndNil(FSharedMemory);
  inherited;
end; { TGpSharedMemoryComp.Destroy }

function TGpSharedMemoryComp.GetAcquired: boolean;
begin
  if not assigned(FSharedMemory) then
    Result := false
  else
    Result := FSharedMemory.Acquired;
end; { TGpSharedMemoryComp.GetAcquired }

function TGpSharedMemoryComp.GetAsStream: TGpSharedStream;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetAsStream');
  Result := FSharedMemory.AsStream;  
end; { TGpSharedMemoryComp.GetAsStream }

function TGpSharedMemoryComp.GetAsString: string;
begin
  if (not assigned(FSharedMemory)) or (Access = accNone) then
    Result := ''
  else 
    Result := FSharedMemory.AsString;
end; { TGpSharedMemoryComp.GetAsString }

function TGpSharedMemoryComp.GetByte(byteOffset: integer): byte;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetByte');
  Result := FSharedMemory.Byte[byteOffset];
end; { TGpSharedMemoryComp.GetByte }

function TGpSharedMemoryComp.GetByteIdx(idx: integer): byte;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetByteIdx');
  Result := FSharedMemory.ByteIdx[idx];
end; { TGpSharedMemoryComp.GetByteIdx }

function TGpSharedMemoryComp.GetDataPointer: pointer;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetDataPointer');
  Result := FSharedMemory.DataPointer;
end; { TGpSharedMemoryComp.GetDataPointer }

function TGpSharedMemoryComp.GetHuge(byteOffset: integer): int64;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetHuge');
  Result := FSharedMemory.Huge[byteOffset];
end; { TGpSharedMemoryComp.GetHuge }

function TGpSharedMemoryComp.GetHugeIdx(idx: integer): int64;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetHugeIdx');
  Result := FSharedMemory.HugeIdx[idx];
end; { TGpSharedMemoryComp.GetHugeIdx }

function TGpSharedMemoryComp.GetIsResizable: boolean;
begin
  Result := (FMaxSize <> 0);
end; { TGpSharedMemoryComp.GetIsResizable }

function TGpSharedMemoryComp.GetLong(byteOffset: integer): longword;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetLong');
  Result := FSharedMemory.Long[byteOffset];
end; { TGpSharedMemoryComp.GetLong }

function TGpSharedMemoryComp.GetLongIdx(idx: integer): longword;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetLongIdx');
  Result := FSharedMemory.LongIdx[idx];
end; { TGpSharedMemoryComp.GetLongIdx }

function TGpSharedMemoryComp.GetModified: boolean;
begin
  if (not assigned(FSharedMemory)) or (Access = accNone) then
    Result := false
  else
    Result := FSharedMemory.Modified;
end; { TGpSharedMemoryComp.GetModified }

function TGpSharedMemoryComp.GetSize: cardinal;
begin
  if not assigned(FSharedMemory) then
    Result := FInitialSize
  else
    Result := FSharedMemory.Size;
end; { TGpSharedMemoryComp.GetSize }

function TGpSharedMemoryComp.GetWasCreated: boolean;
begin
  if not assigned(FSharedMemory) then
    Result := false
  else
    Result := FSharedMemory.WasCreated;
end; { TGpSharedMemoryComp.GetWasCreated }

function TGpSharedMemoryComp.GetWord(byteOffset: integer): word;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetWord');
  Result := FSharedMemory.Word[byteOffset];
end; { TGpSharedMemoryComp.GetWord }

function TGpSharedMemoryComp.GetWordIdx(idx: integer): word;
begin
  CheckAccessibility('TGpSharedMemoryComp.GetWordIdx');
  Result := FSharedMemory.WordIdx[idx];
end; { TGpSharedMemoryComp.GetWordIdx }

procedure TGpSharedMemoryComp.IgnoreSetAcquired(const Value: boolean);
begin
  // read-only property
end; { TGpSharedMemoryComp.IgnoreSetAcquired }

procedure TGpSharedMemoryComp.IgnoreSetIsResizable(const Value: boolean);
begin
  // read-only property
end; { TGpSharedMemoryComp.IgnoreSetIsResizable }

procedure TGpSharedMemoryComp.IgnoreSetModified(const Value: boolean);
begin
  // read-only property
end; { TGpSharedMemoryComp.IgnoreSetModified }

procedure TGpSharedMemoryComp.IgnoreSetWasCreated(const Value: boolean);
begin
  // read-only property
end; { TGpSharedMemoryComp.IgnoreSetWasCreated }

procedure TGpSharedMemoryComp.ReleaseMemory;
begin
  CheckAccessibility('TGpSharedMemoryComp.ReleaseMemory');
  FSharedMemory.ReleaseMemory;
end; { TGpSharedMemoryComp.ReleaseMemory }

procedure TGpSharedMemoryComp.SetAccess(Value: TGpSharedMemoryAccess);
begin
  if FAccess <> Value then begin
    if assigned(FSharedMemory) then begin
      if FAccess <> accNone then
        FSharedMemory.ReleaseMemory;
      if Value = accRead then begin
        if FSharedMemory.AcquireMemory(false, Timeout) = nil then
          Value := accNone;
      end
      else if Value = accWrite then begin
        if FSharedMemory.AcquireMemory(true, Timeout) = nil then
          Value := accNone;
      end;
    end;
    FAccess := Value;
  end;
end; { TGpSharedMemoryComp.SetAccess }

procedure TGpSharedMemoryComp.SetActive(const Value: boolean);
var
  oldAccess: TGpSharedMemoryAccess;
begin
  if FActive <> Value then begin
    if Value then begin
      oldAccess := FAccess; FAccess := accNone;
      FSharedMemory := TGpSharedMemory.Create(SharedMemoryName, FInitialSize,
        FMaxSize);
      SetAccess(oldAccess);
    end
    else
      FreeAndNil(FSharedMemory);
    FActive := Value;
  end;
end; { TGpSharedMemoryComp.SetActive }

procedure TGpSharedMemoryComp.SetAsString(const Value: string);
begin
  if Value <> '' then
    CheckAccessibility('TGpSharedMemoryComp.SetAsString');
  if assigned(FSharedMemory) then
    FSharedMemory.AsString := Value;
end; { TGpSharedMemoryComp.SetAsString }

procedure TGpSharedMemoryComp.SetByte(byteOffset: integer;
  const Value: byte);
begin
  CheckAccessibility('TGpSharedMemoryComp.SetByte');
  FSharedMemory.Byte[byteOffset] := Value;
end; { TGpSharedMemoryComp.SetByte }

procedure TGpSharedMemoryComp.SetByteIdx(idx: integer; const Value: byte);
begin
  CheckAccessibility('TGpSharedMemoryComp.SetByteIdx');
  FSharedMemory.ByteIdx[idx] := Value;
end; { TGpSharedMemoryComp.SetByteIdx }

procedure TGpSharedMemoryComp.SetHuge(byteOffset: integer;
  const Value: int64);
begin
  CheckAccessibility('TGpSharedMemoryComp.SetHuge');
  FSharedMemory.Huge[byteOffset] := Value;
end; { TGpSharedMemoryComp.SetHuge }

procedure TGpSharedMemoryComp.SetHugeIdx(idx: integer; const Value: int64);
begin
  CheckAccessibility('TGpSharedMemoryComp.SetHugeIdx');
  FSharedMemory.HugeIdx[idx] := Value;
end; { TGpSharedMemoryComp.SetHugeIdx }

procedure TGpSharedMemoryComp.SetInitialSize(const Value: cardinal);
begin
  CheckNotActive('TGpSharedMemoryComp.SetInitialSize');
  FInitialSize := Value;
end; { TGpSharedMemoryComp.SetInitialSize }

procedure TGpSharedMemoryComp.SetLong(byteOffset: integer;
  const Value: longword);
begin
  CheckAccessibility('TGpSharedMemoryComp.SetLong');
  FSharedMemory.Long[byteOffset] := Value;
end; { TGpSharedMemoryComp.SetLong }

procedure TGpSharedMemoryComp.SetLongIdx(idx: integer;
  const Value: longword);
begin
  CheckAccessibility('TGpSharedMemoryComp.SetLongIdx');
  FSharedMemory.LongIdx[idx] := Value;
end; { TGpSharedMemoryComp.SetLongIdx }

procedure TGpSharedMemoryComp.SetMaxSize(const Value: cardinal);
begin
  CheckNotActive('TGpSharedMemoryComp.SetMaxSize');
  FMaxSize := Value;
end; { TGpSharedMemoryComp.SetMaxSize }

procedure TGpSharedMemoryComp.SetSharedMemoryName(const Value: string);
begin
  CheckNotActive('TGpSharedMemoryComp.SetSharedMemoryName');
  FSharedMemoryName := Value;
end; { TGpSharedMemoryComp.SetSharedMemoryName }

procedure TGpSharedMemoryComp.SetSize(const Value: cardinal);
begin
  if assigned(FSharedMemory) then
    FSharedMemory.Size := Value;
end; { TGpSharedMemoryComp.SetSize }

procedure TGpSharedMemoryComp.SetWord(byteOffset: integer;
  const Value: word);
begin
  CheckAccessibility('TGpSharedMemoryComp.SetWord');
  FSharedMemory.Word[byteOffset] := Value;
end; { TGpSharedMemoryComp.SetWord }

procedure TGpSharedMemoryComp.SetWordIdx(idx: integer; const Value: word);
begin
  CheckAccessibility('TGpSharedMemoryComp.SetWordIdx');
  FSharedMemory.WordIdx[idx] := Value;
end; { TGpSharedMemoryComp.SetWordIdx }

{ TGpSharedPoolReaderComp }

function TGpSharedPoolReaderComp.AcquireBuffer(
  timeout: DWORD): TGpSharedMemory;
begin
  CheckAccessibility('TGpSharedPoolReaderComp.AcquireBuffer');
  Result := FSharedPoolReader.AcquireBuffer(timeout);
end; { TGpSharedPoolReaderComp.AcquireBuffer }

function TGpSharedPoolReaderComp.AcquireCopy(shm: TGpSharedMemory;
  timeout: DWORD): TGpSharedMemory;
begin
  CheckAccessibility('TGpSharedPoolReaderComp.AcquireCopy');
  Result := FSharedPoolReader.AcquireCopy(shm, timeout);
end; { TGpSharedPoolReaderComp.AcquireCopy }

procedure TGpSharedPoolReaderComp.CheckAccessibility(caller: string);
begin
  if not assigned(FSharedPoolReader) then
    raise Exception.CreateFmt(sSharedPoolIsNotActive, [caller, SharedPoolName]);
end; { TGpSharedPoolReaderComp.CheckAccessibility }

procedure TGpSharedPoolReaderComp.CheckNotActive(caller: string);
begin
  if assigned(FSharedPoolReader) then
    raise Exception.CreateFmt(sSharedPoolIsActive, [caller, SharedPoolName]);
end; { TGpSharedPoolReaderComp.CheckNotActive }

constructor TGpSharedPoolReaderComp.Create(AOwner: TComponent);
begin
  inherited;
  FMinNumBuffers := CDefMinNumBuffers;
  FResizeIncrement := CDefResizeIncrement;
  FResizeThreshold := CDefResizeThreshold;
  FSweepTimeoutSec := CDefSweepTimeoutSec;
end; { TGpSharedPoolReaderComp.Create }

function TGpSharedPoolReaderComp.GetLastError: TGpSharedPoolError;
begin
  if Active then
    Result := FSharedPoolReader.LastError
  else
    Result := speOK;
end; { TGpSharedPoolReaderComp.GetLastError }

function TGpSharedPoolReaderComp.GetNextReceived: TGpSharedMemory;
begin
  CheckAccessibility('TGpSharedPoolReaderComp.GetNextReceived');
  Result := FSharedPoolReader.GetNextReceived;
end; { TGpSharedPoolReaderComp.GetNextReceived }

procedure TGpSharedPoolReaderComp.ReleaseBuffer(var shm: TGpSharedMemory);
begin
  CheckAccessibility('TGpSharedPoolReaderComp.ReleaseBuffer');
  FSharedPoolReader.ReleaseBuffer(shm);
end; { TGpSharedPoolReaderComp.ReleaseBuffer }

function TGpSharedPoolReaderComp.SendBuffer(
  var shm: TGpSharedMemory): boolean;
begin
  CheckAccessibility('TGpSharedPoolReaderComp.SendBuffer');
  Result := FSharedPoolReader.SendBuffer(shm);
end; { TGpSharedPoolReaderComp.SendBuffer }

procedure TGpSharedPoolReaderComp.SetActive(Value: boolean);
begin
  if FActive <> Value then begin
    if Value then begin
      FSharedPoolReader := TGpSharedPoolReader.Create(SharedPoolName);
      if not FSharedPoolReader.Initialize(FInitialBufferSize, FMaxBufferSize,
               FStartNumBuffers, FMaxNumBuffers, FResizeIncrement,
               FResizeThreshold, FMinNumBuffers, FSweepTimeoutSec) then
      begin
        Value := false;
        FreeAndNil(FSharedPoolReader);
      end;
    end
    else
      FreeAndNil(FSharedPoolReader);
    FActive := Value;
  end;
end; { TGpSharedPoolReaderComp.SetActive }

procedure TGpSharedPoolReaderComp.SetInitialBufferSize(
  const Value: cardinal);
begin
  CheckNotActive('TGpSharedPoolReaderComp.SetInitialBufferSize');
  FInitialBufferSize := Value;
end; { TGpSharedPoolReaderComp.SetInitialBufferSize }

procedure TGpSharedPoolReaderComp.SetMaxBufferSize(const Value: cardinal);
begin
  CheckNotActive('TGpSharedPoolReaderComp.SetMaxBufferSize');
  FMaxBufferSize := Value;
end; { TGpSharedPoolReaderComp.SetMaxBufferSize }

procedure TGpSharedPoolReaderComp.SetMaxNumBuffers(const Value: cardinal);
begin
  CheckNotActive('TGpSharedPoolReaderComp.SetMaxNumBuffers');
  FMaxNumBuffers := Value;
end; { TGpSharedPoolReaderComp.SetMaxNumBuffers }

procedure TGpSharedPoolReaderComp.SetMinNumBuffers(const Value: cardinal);
begin
  CheckNotActive('TGpSharedPoolReaderComp.SetMinNumBuffers');
  FMinNumBuffers := Value;
end; { TGpSharedPoolReaderComp.SetMinNumBuffers }

procedure TGpSharedPoolReaderComp.SetResizeIncrement(
  const Value: cardinal);
begin
  CheckNotActive('TGpSharedPoolReaderComp.SetResizeIncrement');
  FResizeIncrement := Value;
end; { TGpSharedPoolReaderComp.SetResizeIncrement }

procedure TGpSharedPoolReaderComp.SetSharedPoolName(const Value: string);
begin
  CheckNotActive('TGpSharedPoolReaderComp.SetSharedPoolName');
  FSharedPoolName := Value;
end; { TGpSharedPoolReaderComp.SetSharedPoolName }

procedure TGpSharedPoolReaderComp.SetStartNumBuffers(
  const Value: cardinal);
begin
  CheckNotActive('TGpSharedPoolReaderComp.SetStartNumBuffers');
  FStartNumBuffers := Value;
end; { TGpSharedPoolReaderComp.SetStartNumBuffers }

procedure TGpSharedPoolReaderComp.SetSweepTimeoutSec(
  const Value: cardinal);
begin
  CheckNotActive('TGpSharedPoolReaderComp.SetSweepTimeoutSec');
  FSweepTimeoutSec := Value;
end; { TGpSharedPoolReaderComp.SetSweepTimeoutSec }

{ TGpSharedPoolWriterComp }

function TGpSharedPoolWriterComp.AcquireBuffer(
  timeout: DWORD): TGpSharedMemory;
begin
  CheckAccessibility('TGpSharedPoolWriterComp.AcquireBuffer');
  Result := FSharedPoolWriter.AcquireBuffer(timeout);
end; { TGpSharedPoolWriterComp.AcquireBuffer }

function TGpSharedPoolWriterComp.AcquireCopy(shm: TGpSharedMemory;
  timeout: DWORD): TGpSharedMemory;
begin
  CheckAccessibility('TGpSharedPoolWriterComp.AcquireCopy');
  Result := FSharedPoolWriter.AcquireCopy(shm, timeout);
end; { TGpSharedPoolWriterComp.AcquireCopy }

procedure TGpSharedPoolWriterComp.CheckAccessibility(caller: string);
begin
  if not assigned(FSharedPoolWriter) then
    raise Exception.CreateFmt(sSharedPoolIsNotActive, [caller, SharedPoolName]);
end; { TGpSharedPoolWriterComp.CheckAccessibility }

procedure TGpSharedPoolWriterComp.CheckNotActive(caller: string);
begin
  if assigned(FSharedPoolWriter) then
    raise Exception.CreateFmt(sSharedPoolIsActive, [caller, SharedPoolName]);
end; { TGpSharedPoolWriterComp.CheckNotActive }

function TGpSharedPoolWriterComp.GetLastError: TGpSharedPoolError;
begin
  if Active then
    Result := FSharedPoolWriter.LastError
  else
    Result := speOK;
end; { TGpSharedPoolWriterComp.GetLastError }

procedure TGpSharedPoolWriterComp.ReleaseBuffer(var shm: TGpSharedMemory);
begin
  CheckAccessibility('TGpSharedPoolWriterComp.ReleaseBuffer');
  FSharedPoolWriter.ReleaseBuffer(shm);
end; { TGpSharedPoolWriterComp.ReleaseBuffer }

function TGpSharedPoolWriterComp.SendBuffer(
  var shm: TGpSharedMemory): boolean;
begin
  CheckAccessibility('TGpSharedPoolWriterComp.SendBuffer');
  Result := FSharedPoolWriter.SendBuffer(shm);
end; { TGpSharedPoolWriterComp.SendBuffer }

procedure TGpSharedPoolWriterComp.SetActive(const Value: boolean);
begin
  if FActive <> Value then begin
    if Value then 
      FSharedPoolWriter := TGpSharedPoolWriter.Create(SharedPoolName)
    else
      FreeAndNil(FSharedPoolWriter);
    FActive := Value;
  end;
end; { TGpSharedPoolWriterComp.SetActive }

procedure TGpSharedPoolWriterComp.SetSharedPoolName(const Value: string);
begin
  CheckNotActive('TGpSharedPoolWriterComp.SetSharedPoolName');
  FSharedPoolName := Value;
end; { TGpSharedPoolWriterComp.SetSharedPoolName }

end.
