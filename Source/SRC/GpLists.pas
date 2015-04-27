(*:Various TList descendants, TList-compatible, and TList-similar classes.
   @author Primoz Gabrijelcic
   @desc <pre>

This software is distributed under the BSD license.

Copyright (c) 2006, Primoz Gabrijelcic
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

   Author            : Primoz Gabrijelcic
   Creation date     : 2002-07-04
   Last modification : 2007-01-19
   Version           : 1.23a
</pre>*)(*
   History:
     1.23a: 2007-01-19
       - Compiles with Delphi 6 again. Big thanks to Tao Lin and 
         Erik Berry for reporting, diagnosing and fixing the problem.
     1.23: 2006-12-06
       - Added ValuesIdx to the TGpObjectMap class.
     1.22a: 2006-09-29
       - Fixed nasty bug in TGpIntegerObjectList.AddObject and
         TGpInt64ObjectList.AddObject.
       - Fixed range errors in TGpInt64[Object]List.
     1.22: 2006-09-20
       - Implemented TGpInt64List and TGpInt64ObjectList.
     1.21: 2006-05-15
       - Implemented list of TMethod records - TGpTMethodList.
     1.20: 2006-04-24
       - Added method TGpIntegerObjectList.ExtractObject.
     1.19: 2005-11-18
       - Added D2005-style TGpIntegerList enumerator.
     1.18: 2005-10-27
       - Added TGpString class.
     1.17: 2005-06-02
       - Added methods FreeAll and UnlinkAll to the TGpDoublyLinkedList class.
     1.16: 2004-11-22
       - Added Dump/Restore mechanism to the TGpInteger[Object]List classes.
     1.15: 2004-09-09
       - Added method Remove to the TGpIntegerList class.
     1.14: 2004-02-17
       - Added 'delimiter' parameter to the TGpIntegerList.AsHexText.
     1.13: 2004-02-12
       - Added iterator access (Count, Items) to the TGpObjectMap class.
     1.12: 2003-12-18
       - Published helper function IntegerCompare.
     1.11: 2003-11-05
       - TGpDoublyLinkedList got new constructor parameter - multithreaded. When
         set to True (default is False), all list-related operations are wrapped
         into a critical section.
     1.10: 2003-10-28
       - Added doubly-linked list class - TGpDoublyLinkedList.
     1.09a: 2003-10-16
       - TGpObjectRingBuffer.Head was off-by-one, causing all sorts of problems.
     1.09: 2003-09-27
       - Added function TGpIntegerList.AsDelimitedText.
     1.08: 2003-09-15
       - Added function TGpIntegerList.Ensure.
       - Added function TGpIntegerObjectList.EnsureObject.
       - Added function TGpCountedStringList.Ensure.
       - Added methods  TGpIntegerObjectList.LoadFromStream, .SaveToStream.
     1.07: 2003-08-02
       - Added class TGpObjectMap.
       - Added class TGpObjectObjectMap.
       - Added class TGpInt64.
     1.06: 2003-07-27
       - Prefixed all classes with 'Gp'.
       - Added class TGpObjectRingBuffer.
     1.05: 2003-07-15
       - Added overloaded constructor TIntegerList.Create(array of integer).
       - Added overloaded method Assign(array of integer).
     1.04: 2003-06-11
       - Added methods TIntegerList.SaveToStream and
         TIntegerList.LoadFromStream.
     1.03: 2003-06-09
       - Added TIntegerObjectList class.
     1.02a: 2003-03-21
       - Fixed TIntegerList.Find, which was completely broken.
     1.02: 2002-10-30
       - Added property TIntegerList.Text;
     1.01: 2002-09-23
       - Added method TIntegerList.IndexOf.
     1.0: 2002-07-04
       - Created.
*)

unit GpLists;

interface

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF (RTLVersion >= 15)} // Delphi 7.0 or newer
    {$DEFINE D7PLUS}
  {$IFEND}
{$ENDIF}

uses
  Classes,
  Contnrs,
  SyncObjs, 
  Windows;

type
  {:Class holding one int64 field. Usable for inserting int64 numbers into the
    TObjectList or similar classes.
    @since   2003-08-02
  }
  TGpInt64 = class
  private
    i64Value: int64;
  public
    constructor Create(aValue: int64 = 0);
    property Value: int64 read i64Value write i64Value;
  end; { TGpInt64 }

  {:Class holding one TDateTime field. Usable for inserting TDateTime numbers
    into the TObjectList or similar classes.
    @since   2003-08-23
  }
  TGpDateTime = class
  private
    dtValue: TDateTime;
  public
    constructor Create(aValue: TDateTime = 0);
    property Value: TDateTime read dtValue write dtValue;
  end; { TGpDateTime }

  {:Class holding one string field.
    @since   2005-10-27
  }
  TGpString = class
  private
    sValue: string;
  public
    constructor Create(aValue: string = '');
    property Value: string read sValue write sValue;
  end; { TGpString }

  TGpIntegerList = class;

  {:TGpIntegerList enumerator.
    @since   2005-11-18
  }
  TGpIntegerListEnumerator = class
  private
    ileIndex: integer;
    ileList : TGpIntegerList;
  public
    constructor Create(aList: TGpIntegerList);
    function  GetCurrent: integer;
    function  MoveNext: boolean;
    property Current: integer read GetCurrent;
  end; { TGpIntegerListEnumerator }

  TGpIntegerListSortCompare = function(List: TGpIntegerList;
    Index1, Index2: integer): integer;

  {:List of integers.
    @since   2002-07-04.
  }
  TGpIntegerList = class
  private
    ilDuplicates: TDuplicates;
    ilList      : TList;
    ilSorted    : Boolean;
  protected
    function  GetAsDelimitedText(const delimiter: string;
      appendLastDelimiter: boolean): string;
    function  GetCapacity: integer; virtual;
    function  GetCount: integer; virtual;
    function  GetItems(idx: integer): integer; virtual;
    function  GetText: string; virtual;
    procedure QuickSort(L, R: integer; SCompare: TGpIntegerListSortCompare);
    procedure SetCapacity(const value: integer); virtual;
    procedure SetCount(const value: integer); virtual;
    procedure SetItems(idx: integer; const value: integer); virtual;
    procedure SetSorted(const value: Boolean); virtual;
    procedure SetText(const Value: string); virtual;
    procedure Sort; virtual;
  public
    constructor Create; overload;
    constructor Create(elements: array of integer); overload;
    destructor  Destroy; override;
    function  Add(item: integer): integer; virtual;
    procedure Append(elements: array of integer); overload;
    procedure Append(list: TGpIntegerList); overload; virtual;
    function  AsDelimitedText(const delimiter: string): string;
    function  AsHexText(const delimiter: string = ''): string;
    procedure Assign(elements: array of integer); overload;
    procedure Assign(list: TGpIntegerList); overload; virtual;
    procedure Clear; virtual;
    procedure Delete(idx: integer); virtual;
    function  Dump(baseAddr: pointer): pointer; virtual;
    function  Ensure(item: integer): integer; virtual;
    procedure Exchange(idx1, idx2: integer); virtual;
    function  Find(avalue: integer; var idx: integer): boolean; virtual;
    function  First: integer; virtual;
    function  GetEnumerator: TGpIntegerListEnumerator;
    function  IndexOf(item: integer): integer;
    procedure Insert(idx, item: integer); virtual;
    function  Last: integer; virtual;
    function  LoadFromStream(stream: TStream): boolean; virtual;
    procedure Move(curIdx, newIdx: integer); virtual;
    procedure Remove(item: integer); virtual;
    function  Restore(baseAddr: pointer): pointer; virtual;
    procedure SaveToStream(stream: TStream); virtual;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read ilDuplicates write ilDuplicates;
    property Items[idx: integer]: integer read GetItems write SetItems; default;
    property Sorted: boolean read ilSorted write SetSorted;
    property Text: string read GetText write SetText;
  end; { TGpIntegerList }

  TGpInt64List = class;

  {:TGpInt64List enumerator.
    @since   2005-11-18
  }
  TGpInt64ListEnumerator = class
  private
    ileIndex: integer;
    ileList : TGpInt64List;
  public
    constructor Create(aList: TGpInt64List);
    function  GetCurrent: int64;
    function  MoveNext: boolean;
    property Current: int64 read GetCurrent;
  end; { TGpInt64ListEnumerator }

  TGpInt64ListSortCompare = function(List: TGpInt64List; Index1, Index2: integer):
    integer;

  {:List of 64-bit integers.
    @since   2006-09-20
  }
  TGpInt64List = class
  private
    ilDuplicates: TDuplicates;
    ilList      : TList;
    ilSorted    : Boolean;
  protected
    function  GetAsDelimitedText(const delimiter: string;
      appendLastDelimiter: boolean): string;
    function  GetCapacity: integer; virtual;
    function  GetCount: integer; virtual;
    function  GetItems(idx: integer): int64; virtual;
    function  GetText: string; virtual;
    procedure QuickSort(L, R: integer; SCompare: TGpInt64ListSortCompare);
    procedure SetCapacity(const value: integer); virtual;
    procedure SetCount(const value: integer); virtual;
    procedure SetItems(idx: integer; value: int64); virtual;
    procedure SetSorted(const value: Boolean); virtual;
    procedure SetText(const value: string); virtual;
    procedure Sort; virtual;
  public
    constructor Create; overload;
    constructor Create(elements: array of int64); overload;
    constructor Create(elements: array of integer); overload;
    destructor  Destroy; override;
    function  Add(item: int64): integer; virtual;
    procedure Append(elements: array of int64); overload;
    procedure Append(elements: array of integer); overload;
    procedure Append(list: TGpInt64List); overload; virtual;
    procedure Append(list: TGpIntegerList); overload; virtual;
    function  AsDelimitedText(const delimiter: string): string;
    function  AsHexText(const delimiter: string = ''): string;
    procedure Assign(elements: array of int64); overload;
    procedure Assign(elements: array of integer); overload;
    procedure Assign(list: TGpInt64List); overload; virtual;
    procedure Assign(list: TGpIntegerList); overload; virtual;
    procedure Clear; virtual;
    procedure Delete(idx: integer); virtual;
    function  Dump(baseAddr: pointer): pointer; virtual;
    function  Ensure(item: int64): integer; virtual;
    procedure Exchange(idx1, idx2: integer); virtual;
    function  Find(avalue: int64; var idx: integer): boolean; virtual;
    function  First: int64; virtual;
    function  GetEnumerator: TGpInt64ListEnumerator;
    function  IndexOf(item: int64): integer;
    procedure Insert(idx: integer; item: int64); virtual;
    function  Last: int64; virtual;
    function  LoadFromStream(stream: TStream): boolean; virtual;
    procedure Move(curIdx, newIdx: integer); virtual;
    procedure Remove(item: int64); virtual;
    function  Restore(baseAddr: pointer): pointer; virtual;
    procedure SaveToStream(stream: TStream); virtual;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Duplicates: TDuplicates read ilDuplicates write ilDuplicates;
    property Items[idx: integer]: int64 read GetItems write SetItems; default;
    property Sorted: boolean read ilSorted write SetSorted;
    property Text: string read GetText write SetText;
  end; { TGpInt64List }

  {:Integer list where each integer is accompanied with an object.
    @since   2003-06-09
  }
  TGpIntegerObjectList = class(TGpIntegerList)
  private
    iolObjects: TObjectList;
  protected
    function  GetObject(idxObject: integer): TObject; virtual;
    procedure SetObject(idxObject: integer; const value: TObject);
  public
    constructor Create(ownsObjects: boolean = true);
    destructor  Destroy; override;
    function  Add(item: integer): integer; override;
    function  AddObject(item: integer; obj: TObject): integer; virtual;
    procedure Clear; override;
    procedure Delete(idx: integer); override;
    function  Dump(baseAddr: pointer): pointer; override; 
    function  EnsureObject(item: integer; obj: TObject): integer; virtual;
    procedure Exchange(idx1, idx2: integer); override;
    function  ExtractObject(idxObject: integer): TObject;
    procedure Insert(idx: integer; item: integer); override;
    procedure InsertObject(idx: integer; item: integer; obj: TObject); virtual;
    function  LoadFromStream(stream: TStream): boolean; override;
    procedure Move(curIdx, newIdx: integer); override;
    function  Restore(baseAddr: pointer): pointer; override;
    procedure SaveToStream(stream: TStream); override;
    property Objects[idxObject: integer]: TObject read GetObject write SetObject;
  end; { TGpIntegerObjectList }

  {:Int64 list where each integer is accompanied with an object.
    @since   2006-09-20
  }
  TGpInt64ObjectList = class(TGpInt64List)
  private
    iolObjects: TObjectList;
  protected
    function  GetObject(idxObject: integer): TObject; virtual;
    procedure SetObject(idxObject: integer; const value: TObject);
  public
    constructor Create(ownsObjects: boolean = true);
    destructor  Destroy; override;
    function  Add(item: int64): integer; override;
    function  AddObject(item: int64; obj: TObject): integer; virtual;
    procedure Clear; override;
    procedure Delete(idx: integer); override;
    function  Dump(baseAddr: pointer): pointer; override;
    function  EnsureObject(item: int64; obj: TObject): integer; virtual;
    procedure Exchange(idx1, idx2: integer); override;
    function  ExtractObject(idxObject: integer): TObject;
    procedure Insert(idx: integer; item: int64); override;
    procedure InsertObject(idx: integer; item: int64; obj: TObject); virtual;
    function  LoadFromStream(stream: TStream): boolean; override;
    procedure Move(curIdx, newIdx: integer); override;
    function  Restore(baseAddr: pointer): pointer; override;
    procedure SaveToStream(stream: TStream); override;
    property Objects[idxObject: integer]: TObject read GetObject write SetObject;
  end; { TGpInt64ObjectList }

  {:String list where each item has associated counter (stored in the Objects)
    property.
  }
  TGpCountedStringList = class(TStringList)
  protected
    function  GetItemCount(idx: integer): integer; virtual;
    procedure SetItemCount(idx: integer; const Value: integer); virtual;
  public
    function  Add(const s: string; count: integer): integer; reintroduce;
    function  Ensure(const s: string; count: integer): integer; 
    procedure SortByCounter(descending: boolean = true);
    property  Counter[idx: integer]: integer read GetItemCount write SetItemCount;
  end; { TGpCountedStringList }

  TGpTMethodList = class;

  {:TGpTMethodList enumerator.
    @since   2006-05-15
  }
  TGpTMethodListEnumerator = class
  private
    mleIndex: integer;
    mleList : TGpTMethodList;
  public
    constructor Create(aList: TGpTMethodList);
    function  GetCurrent: TMethod;
    function  MoveNext: boolean;
    property Current: TMethod read GetCurrent;
  end; { TGpTMethodListEnumerator }

  {:List of TMethod records.
    @since   2006-05-15
  }
  TGpTMethodList = class
  private
    mlCode: TList;
    mlData: TList;
  protected
    function  GetCapacity: integer;
    function  GetCount: integer;
    function  GetItems(idx: integer): TMethod;
    procedure SetCapacity(const value: integer);
    procedure SetCount(const value: integer);
    procedure SetItems(idx: integer; const value: TMethod);
  public
    constructor Create;
    destructor  Destroy; override;
    function  Add(item: TMethod): integer; virtual;
    procedure Assign(list: TGpTMethodList); virtual;
    procedure Clear; virtual;
    procedure Delete(idx: integer); virtual;
    function  Ensure(item: TMethod): integer;
    function  GetEnumerator: TGpTMethodListEnumerator;
    function  IndexOf(item: TMethod): integer;
    procedure Insert(idx: integer; item: TMethod); virtual;
    procedure Remove(item: TMethod); virtual;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount write SetCount;
    property Items[idx: integer]: TMethod read GetItems write SetItems; default;
  end; { TGpTMethodList }

  {:Fixed-size ring buffer of TObject references.
    @since   2003-07-25
  }
  TGpObjectRingBuffer = class
  private
    orbBuffer     : array of TObject;
    orbBufferSize : integer;
    orbHead       : integer;
    orbOwnsObjects: boolean;
    orbTail       : integer;
  protected
    function  GetItem(iObject: integer): TObject; virtual;
    function  IncPointer(const ptr: integer; increment: integer = 1): integer;
    procedure SetItem(iObject: integer; const Value: TObject); virtual;
  public
    constructor Create(bufferSize: integer; ownsObjects: boolean = true);
    destructor  Destroy; override;
    procedure Clear;
    function  Count: integer;
    function  Dequeue: TObject;
    function  Enqueue(obj: TObject): boolean;
    function  Head: TObject;
    function  IsEmpty: boolean;
    function  IsFull: boolean;
    function  Tail: TObject;
    property Items[iObject: integer]: TObject read GetItem write SetItem; default;
    property OwnsObjects: boolean read orbOwnsObjects;
  end; { TGpObjectRingBuffer }

  {:Object map comparision function.
    @since   2003-08-02
  }        
  TGpObjectMapCompare = function(userValue, mapValue: TObject): boolean;

  {:List, indexed by objects and containing objects.
    @since   2003-08-02
  }
  TGpObjectMap = class
  private
    omList: TGpIntegerObjectList;
  protected
    function  GetIndexedItem(idxItem: integer): TObject;
    function  GetIndexedValue(idxValue: integer): TObject;
    function  GetItems(item: TObject): TObject; virtual;
    procedure SetItems(item: TObject; const value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true); overload;
    destructor  Destroy; override;
    procedure Clear; virtual;
    function  Count: integer;
    function  Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
    procedure Find(value: TObject; compareFunc: TGpObjectMapCompare;
      var item: TObject); virtual;
    property Items[idxItem: integer]: TObject read GetIndexedItem;
    property ValuesIdx[idxValue: integer]: TObject read GetIndexedValue;
    property Values[item: TObject]: TObject read GetItems write SetItems; default;
  end; { TGpObjectMap }

  {:Matrix, indexed by two objects and containing objects.
    @since   2003-08-02
  }        
  TGpObjectObjectMap = class
  private
    oomCompareFunc: TGpObjectMapCompare;
    oomFindValue  : TObject;
    oomItem2      : TObject;
    oomMap        : TGpObjectMap;
    oomOwnsObjects: boolean;
  protected
    function  GetItems(item1, item2: TObject): TObject; virtual;
    function  Map(item: TObject): TGpObjectMap; virtual;
    procedure SetItems(item1, item2: TObject; const Value: TObject); virtual;
  public
    constructor Create(ownsObjects: boolean = true); overload;
    destructor  Destroy; override;
    procedure Clear; virtual;
    function  Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
    procedure Find(value: TObject; compareFunc: TGpObjectMapCompare; var item1,
      item2: TObject);
    property Values[item1, item2: TObject]: TObject read GetItems write SetItems; default;
  end; { TGpObjectObjectMap }

  TGpDoublyLinkedList = class;

  {:Ancestor for objects that can be insterted in a doubly-linked list.
    @since   2003-10-27
  }
  TGpDoublyLinkedListObject = class
  private
    dlloList    : TGpDoublyLinkedList;
    dlloNext    : TGpDoublyLinkedListObject;
    dlloPrevious: TGpDoublyLinkedListObject;
  protected
    procedure LinkAfter(list: TGpDoublyLinkedList; obj: TGpDoublyLinkedListObject);
    procedure Unlink;
  public
    destructor Destroy; override;
    function  Next: TGpDoublyLinkedListObject;
    function  Previous: TGpDoublyLinkedListObject;
    property List: TGpDoublyLinkedList read dlloList;
  end; { TGpDoublyLinkedListObject }

  {:A list of doubly-linked TGpDoublyLinkedListObject objects. NOT an owner of linked
    objects.
    @since   2003-10-27
  }
  TGpDoublyLinkedList = class
  private
    dllCount: integer;
    dllHead : TGpDoublyLinkedListObject;
    dllLock : TCriticalSection;
    dllTail : TGpDoublyLinkedListObject;
  protected
    procedure Linking(obj: TGpDoublyLinkedListObject);
    procedure Unlinking(obj: TGpDoublyLinkedListObject);
  public
    constructor Create(multithreaded: boolean = false); overload;
    destructor  Destroy; override;
    function  Count: integer;
    procedure FreeAll;
    function  Head: TGpDoublyLinkedListObject;
    procedure InsertAfter(existingObject: TGpDoublyLinkedListObject;
      obj: TGpDoublyLinkedListObject);
    procedure InsertAtHead(obj: TGpDoublyLinkedListObject);
    procedure InsertAtTail(obj: TGpDoublyLinkedListObject);
    procedure InsertBefore(existingObject: TGpDoublyLinkedListObject;
      obj: TGpDoublyLinkedListObject);
    function  IsEmpty: boolean;
    procedure Lock;
    function  Next(obj: TGpDoublyLinkedListObject): TGpDoublyLinkedListObject;
    function  Previous(obj: TGpDoublyLinkedListObject): TGpDoublyLinkedListObject;
    function  RemoveFromHead: TGpDoublyLinkedListObject;
    function  RemoveFromTail: TGpDoublyLinkedListObject;
    function  Tail: TGpDoublyLinkedListObject;
    procedure Unlink(obj: TGpDoublyLinkedListObject);
    procedure UnlinkAll;
    procedure Unlock;
  end; { TGpDoublyLinkedList }

  {:Compares two TGpInt64objects for equality. Ready for use in
    TGpObject(Object)Map.
  }
  function GpCompareInt64(userValue, mapValue: TObject): boolean;

  {:Useful helper function.
    @returns <0, 0, >0; depending on the result of the comparison
    @since   2003-12-18
  }        
  function IntegerCompare(avalue1, avalue2: integer): integer;

  {:Useful helper function.
    @returns <0, 0, >0; depending on the result of the comparison
    @since   2006-09-20
  }
  function Int64Compare(avalue1, avalue2: int64): integer;

implementation

uses
  Types,
  SysUtils,
  Consts,
  Math;

{ publics }

function IntegerCompare(avalue1, avalue2: integer): integer;
begin
  if avalue1 < avalue2 then
    Result := -1
  else if avalue1 > avalue2 then
    Result := 1
  else
    Result := 0;
end; { IntegerCompare }

function Int64Compare(avalue1, avalue2: int64): integer;
begin
  if avalue1 < avalue2 then
    Result := -1
  else if avalue1 > avalue2 then
    Result := 1
  else
    Result := 0;
end; { Int64Compare }

function GpCompareInt64(userValue, mapValue: TObject): boolean;
begin
  Result := (TGpInt64(userValue).Value = TGpInt64(mapValue).Value);
end; { GpCompareInt64 }

{ globals }

function IntegerListCompare(List: TGpIntegerList; idx1, idx2: integer): integer;
begin
  Result := IntegerCompare(List[idx1], List[idx2]);
end; { IntegerListCompare }

function Int64ListCompare(List: TGpInt64List; idx1, idx2: integer): integer;
begin
  Result := Int64Compare(List[idx1], List[idx2]);
end; { Int64ListCompare }

{ TGpInt64 }

constructor TGpInt64.Create(aValue: int64);
begin
  inherited Create;
  i64Value := aValue;
end; { TGpInt64.Create }

{ TGpDateTime }

constructor TGpDateTime.Create(aValue: TDateTime);
begin
  inherited Create;
  dtValue := aValue;
end; { TGpDateTime.Create }

{ TGpString }

constructor TGpString.Create(aValue: string);
begin
  inherited Create;
  sValue := aValue;
end; { TGpString.Create }

{ TGpIntegerListEnumerator }

constructor TGpIntegerListEnumerator.Create(aList: TGpIntegerList);
begin
  inherited Create;
  ileIndex := -1;
  ileList := aList;
end; { TGpIntegerListEnumerator.Create }

function TGpIntegerListEnumerator.GetCurrent: integer;
begin
  Result := ileList[ileIndex];
end; { TGpIntegerListEnumerator.GetCurrent }

function TGpIntegerListEnumerator.MoveNext: boolean;
begin
  Result := ileIndex < (ileList.Count - 1);
  if Result then
    Inc(ileIndex);
end; { TGpIntegerListEnumerator.MoveNext }

constructor TGpIntegerList.Create;
begin
  inherited;
  ilList := TList.Create;
end; { TGpIntegerList.Create }

constructor TGpIntegerList.Create(elements: array of integer);
begin
  Create;
  Assign(elements);
end; { TGpIntegerList.Create }

destructor TGpIntegerList.Destroy;
begin
  FreeAndNil(ilList);
  inherited;
end; { TGpIntegerList.Destroy }

{ TGpIntegerList }

function TGpIntegerList.Add(item: integer): integer;
begin
  if not Sorted then
    Result := ilList.Add(pointer(item))
  else begin
    if Find(item, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError : ilList.Error(SDuplicateItem, item);
      end;
    Insert(Result, item);
  end;
end; { TGpIntegerList.Add }

procedure TGpIntegerList.Append(elements: array of integer);
var
  iElement: integer;
begin
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpIntegerList.Append }

procedure TGpIntegerList.Append(list: TGpIntegerList);
var
  iItem: integer;
begin
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpIntegerList.Append }

function TGpIntegerList.AsDelimitedText(const delimiter: string): string;
begin
  Result := GetAsDelimitedText(delimiter, false);
end; { TGpIntegerList.AsDelimitedText }

function TGpIntegerList.AsHexText(const delimiter: string): string;
var
  hsl  : TStringList;
  iItem: integer;
begin
  hsl := TStringList.Create;
  try
    for iItem := 0 to Count-2 do
      hsl.Add(Format('%x', [Items[iItem]]));
    Result := hsl.Text;
    if delimiter <> '' then
      Result := StringReplace(Result, #13#10, delimiter, [rfReplaceAll]);
    if Count > 0 then
      Result := Result + Format('%x', [Items[Count-1]]);
  finally FreeAndNil(hsl); end;
end; { TGpIntegerList.AsHexText }

procedure TGpIntegerList.Assign(elements: array of integer);
begin
  Clear;
  Append(elements);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Assign(list: TGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpIntegerList.Assign }

procedure TGpIntegerList.Clear;
begin
  ilList.Clear;
end; { TGpIntegerList.Clear }

procedure TGpIntegerList.Delete(idx: integer);
begin
  ilList.Delete(idx);
end; { TGpIntegerList.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpIntegerList.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PDWORD;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := DWORD(Items[iList]);
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerList.Dump }

function TGpIntegerList.Ensure(item: integer): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpIntegerList.Ensure }

procedure TGpIntegerList.Exchange(idx1, idx2: integer);
begin
  ilList.Exchange(idx1, idx2);
end; { TGpIntegerList.Exchange }

function TGpIntegerList.Find(avalue: integer; var idx: integer): boolean;
var
  L, H, I, C: integer;
begin
  Result := false;
  L := 0;
  H := Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := IntegerCompare(Items[I], avalue);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := true;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  idx := L;
end; { TGpIntegerList.Find }

function TGpIntegerList.First: integer;
begin
  Result := Items[0];
end; { TGpIntegerList.First }

function TGpIntegerList.GetAsDelimitedText(const delimiter: string;
  appendLastDelimiter: boolean): string;
var
  iItem   : integer;
  item    : integer;
  lenDelim: integer;
  lenItem : integer;
  p       : PChar;
  q       : PChar;
  sItem   : string;
  size    : integer;
begin
  size := 0;
  lenDelim := Length(delimiter);
  for iItem := 0 to Count-1 do begin
    item := GetItems(iItem);
    if item = 0 then
      lenItem := 1
    else if item < 0 then
      lenItem := Trunc(Log10(-item))+2
    else
      lenItem := Trunc(Log10(item))+1;
    Inc(size, lenItem);
    Inc(size, lenDelim);
  end;
  if not appendLastDelimiter then
    Dec(size, lenDelim);
  SetString(Result, nil, size);
  p := Pointer(Result);
  for iItem := 0 to Count-1 do begin
    sItem := IntToStr(GetItems(iItem));
    lenItem := Length(sItem);
    if lenItem <> 0 then begin
      System.Move(pointer(sItem)^, p^, lenItem);
      Inc(p, lenItem);
    end;
    if appendLastDelimiter or (iItem < (Count-1)) then begin
      q := Pointer(delimiter);
      while q^ <> #0 do begin
        p^ := q^;
        Inc(p);
        Inc(q);
      end; //while
    end;
  end;
end; { TGpIntegerList.GetAsDelimitedText }

function TGpIntegerList.GetCapacity: integer;
begin
  Result := ilList.Capacity;
end; { TGpIntegerList.GetCapacity }

function TGpIntegerList.GetCount: integer;
begin
  Result := ilList.Count;
end; { TGpIntegerList.GetCount }

function TGpIntegerList.GetEnumerator: TGpIntegerListEnumerator;
begin
  Result := TGpIntegerListEnumerator.Create(Self);
end; { TGpIntegerList.GetEnumerator }

function TGpIntegerList.GetItems(idx: integer): integer;
begin
  Result := integer(ilList.Items[idx]);
end; { TGpIntegerList.GetItems }

function TGpIntegerList.GetText: string;
begin
  Result := GetAsDelimitedText(#13#10, true);
end; { TGpIntegerList.GetText }

function TGpIntegerList.IndexOf(item: integer): integer;
begin
  if Sorted then begin         
    if not Find(item, Result) then
      Result := -1
  end
  else
    Result := ilList.IndexOf(pointer(item));
end; { TGpIntegerList.IndexOf }

procedure TGpIntegerList.Insert(idx, item: integer);
begin
  ilList.Insert(idx, pointer(item));
end; { TGpIntegerList.Insert }

function TGpIntegerList.Last: integer;
begin
  Result := Items[Count-1];
end; { TGpIntegerList.Last }

function TGpIntegerList.LoadFromStream(stream: TStream): boolean;
var
  item: integer;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 4);
    if read = 4 then
      Add(item)
    else if read > 0 then
      Exit;
  until read = 0;
  Result := true;
end; { TGpIntegerList.LoadFromStream }

procedure TGpIntegerList.Move(curIdx, newIdx: integer);
begin
  ilList.Move(curIdx, newIdx);
end; { TGpIntegerList.Move }

procedure TGpIntegerList.QuickSort(L, R: integer; SCompare: TGpIntegerListSortCompare);
var
  I, J, P: integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while (SCompare(Self, I, P) < 0) do
        Inc(I);
      while (SCompare(Self, J, P) > 0) do
        Dec(J);
      if (I <= J) then begin
        Exchange(I, J);
        if (P = I) then
          P := J
        else if (P = J) then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until (I > J);
    if (L < J) then
      QuickSort(L, J, SCompare);
    L := I;
  until (I >= R);
end; { TGpIntegerList.QuickSort }

procedure TGpIntegerList.Remove(item: integer);
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpIntegerList.Remove }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpIntegerList.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  numItems: integer;
  pList   : PDWORD;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    Add(integer(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerList.Restore }

procedure TGpIntegerList.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : integer;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 4);
  end;
end; { TGpIntegerList.SaveToStream }

procedure TGpIntegerList.SetCapacity(const value: integer);
begin
  ilList.Capacity := value;
end; { TGpIntegerList.SetCapacity }

procedure TGpIntegerList.SetCount(const value: integer);
begin
  ilList.Count := value;
end; { TGpIntegerList.SetCount }

procedure TGpIntegerList.SetItems(idx: integer; const value: integer);
begin
  ilList.Items[idx] := pointer(value);
end; { TGpIntegerList.SetItems }

procedure TGpIntegerList.SetSorted(const value: boolean);
begin
  if (ilSorted <> value) then begin
    if value then
      Sort;
    ilSorted := value;
  end;
end; { TGpIntegerList.SetSorted }

procedure TGpIntegerList.SetText(const Value: string);
var
  p    : PChar;
  s    : string;
  start: PChar;
begin
  Clear;
  p := pointer(Value);
  if P <> nil then
    while p^ <> #0 do begin
      start := p;
      while not CharInSet(p^,[#0, #10, #13]) do
        Inc(p);
      SetString(s, start, p - start);
      Add(StrToInt(s));
      if p^ = #13 then Inc(p);
      if p^ = #10 then Inc(p);
    end;
end; { TGpIntegerList.SetText }

procedure TGpIntegerList.Sort;
begin
  if not Sorted and (Count > 1) then
    QuickSort(0, Count - 1, IntegerListCompare);
end; { TGpIntegerList.Sort }

{ TGpInt64ListEnumerator }

constructor TGpInt64ListEnumerator.Create(aList: TGpInt64List);
begin
  inherited Create;
  ileIndex := -1;
  ileList := aList;
end; { TGpInt64ListEnumerator.Create }

function TGpInt64ListEnumerator.GetCurrent: int64;
begin
  Result := ileList[ileIndex];
end; { TGpInt64ListEnumerator.GetCurrent }

function TGpInt64ListEnumerator.MoveNext: boolean;
begin
  Result := ileIndex < (ileList.Count - 1);
  if Result then
    Inc(ileIndex);
end; { TGpInt64ListEnumerator.MoveNext }

{ TGpInt64List }

constructor TGpInt64List.Create;
begin
  inherited;
  ilList := TList.Create;
end; { TGpInt64List.Create }

constructor TGpInt64List.Create(elements: array of int64);
begin
  Create;
  Assign(elements);
end; { TGpInt64List.Create }

constructor TGpInt64List.Create(elements: array of integer);
begin
  Create;
  Assign(elements);
end; { TGpInt64List.Create }

destructor TGpInt64List.Destroy;
begin
  FreeAndNil(ilList);
  inherited;
end; { TGpInt64List.Destroy }

function TGpInt64List.Add(item: int64): integer;
begin
  if not Sorted then begin
    ilList.Add(pointer(Int64Rec(item).Lo));
    Result := ilList.Add(pointer(Int64Rec(item).Hi)) div 2;
  end
  else begin
    if Find(item, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError : ilList.Error(SDuplicateItem, item);
      end;
    Insert(Result, item);
  end;
end; { TGpInt64List.Add }

procedure TGpInt64List.Append(elements: array of int64);
var
  iElement: integer;
begin
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(elements: array of integer);
var
  iElement: integer;
begin
  for iElement := Low(elements) to High(elements) do
    Add(elements[iElement]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(list: TGpInt64List);
var
  iItem: integer;
begin
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpInt64List.Append }

procedure TGpInt64List.Append(list: TGpIntegerList);
var
  iItem: integer;
begin
  for iItem := 0 to list.Count-1 do
    Add(list[iItem]);
end; { TGpInt64List.Append }

function TGpInt64List.AsDelimitedText(const delimiter: string): string;
begin
  Result := GetAsDelimitedText(delimiter, false);
end; { TGpInt64List.AsDelimitedText }

function TGpInt64List.AsHexText(const delimiter: string): string;
var
  hsl  : TStringList;
  iItem: integer;
begin
  hsl := TStringList.Create;
  try
    for iItem := 0 to Count-2 do
      hsl.Add(Format('%x', [Items[iItem]]));
    Result := hsl.Text;
    if delimiter <> '' then
      Result := StringReplace(Result, #13#10, delimiter, [rfReplaceAll]);
    if Count > 0 then
      Result := Result + Format('%x', [Items[Count-1]]);
  finally FreeAndNil(hsl); end;
end; { TGpInt64List.AsHexText }

procedure TGpInt64List.Assign(elements: array of int64);
begin
  Clear;
  Append(elements);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(elements: array of integer);
begin
  Clear;
  Append(elements);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(list: TGpInt64List);
begin
  Clear;
  Append(list);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Assign(list: TGpIntegerList);
begin
  Clear;
  Append(list);
end; { TGpInt64List.Assign }

procedure TGpInt64List.Clear;
begin
  ilList.Clear;
end; { TGpInt64List.Clear }

procedure TGpInt64List.Delete(idx: integer);
begin
  ilList.Delete(2*idx);
  ilList.Delete(2*idx);
end; { TGpInt64List.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2006-09-20
}        
function TGpInt64List.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PLargeInteger;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := Items[iList];
    Inc(pList);
  end;
  Result := pList;
end; { TGpInt64List.Dump }

function TGpInt64List.Ensure(item: int64): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpInt64List.Ensure }

procedure TGpInt64List.Exchange(idx1, idx2: integer);
begin
  ilList.Exchange(2*idx1, 2*idx2);
  ilList.Exchange(2*idx1+1, 2*idx2+1);
end; { TGpInt64List.Exchange }

function TGpInt64List.Find(avalue: int64; var idx: integer): boolean;
var
  L, H, I, C: integer;
begin
  Result := false;
  L := 0;
  H := Count - 1;
  while L <= H do begin
    I := (L + H) shr 1;
    C := Int64Compare(Items[I], avalue);
    if C < 0 then
      L := I + 1
    else begin
      H := I - 1;
      if C = 0 then begin
        Result := true;
        if Duplicates <> dupAccept then
          L := I;
      end;
    end;
  end;
  idx := L;
end; { TGpInt64List.Find }

function TGpInt64List.First: int64;
begin
  Result := Items[0];
end; { TGpInt64List.First }

function TGpInt64List.GetAsDelimitedText(const delimiter: string;
  appendLastDelimiter: boolean): string;
var
  iItem   : integer;
  item    : int64;
  lenDelim: integer;
  lenItem : integer;
  p       : PChar;
  q       : PChar;
  sItem   : string;
  size    : integer;
begin
  size := 0;
  lenDelim := Length(delimiter);
  for iItem := 0 to Count-1 do begin
    item := GetItems(iItem);
    if item = 0 then
      lenItem := 1
    else if item < 0 then
      lenItem := Trunc(Log10(-item))+2
    else
      lenItem := Trunc(Log10(item))+1;
    Inc(size, lenItem);
    Inc(size, lenDelim);
  end;
  if not appendLastDelimiter then
    Dec(size, lenDelim);
  SetString(Result, nil, size);
  p := Pointer(Result);
  for iItem := 0 to Count-1 do begin
    sItem := IntToStr(GetItems(iItem));
    lenItem := Length(sItem);
    if lenItem <> 0 then begin
      System.Move(pointer(sItem)^, p^, lenItem);
      Inc(p, lenItem);
    end;
    if appendLastDelimiter or (iItem < (Count-1)) then begin
      q := Pointer(delimiter);
      while q^ <> #0 do begin
        p^ := q^;
        Inc(p);
        Inc(q);
      end; //while
    end;
  end;
end; { TGpInt64List.GetAsDelimitedText }

function TGpInt64List.GetCapacity: integer;
begin
  Result := ilList.Capacity div 2;
end; { TGpInt64List.GetCapacity }

function TGpInt64List.GetCount: integer;
begin
  Result := ilList.Count div 2;
end; { TGpInt64List.GetCount }

function TGpInt64List.GetEnumerator: TGpInt64ListEnumerator;
begin
  Result := TGpInt64ListEnumerator.Create(Self);
end; { TGpInt64List.GetEnumerator }

function TGpInt64List.GetItems(idx: integer): int64;
begin
  Int64Rec(Result).Lo := cardinal(ilList.Items[2*idx]);
  Int64Rec(Result).Hi := cardinal(ilList.Items[2*idx+1]);
end; { TGpInt64List.GetItems }

function TGpInt64List.GetText: string;
begin
  Result := GetAsDelimitedText(#13#10, true);
end; { TGpInt64List.GetText }

function TGpInt64List.IndexOf(item: int64): integer;
begin
  if Sorted then begin         
    if not Find(item, Result) then
      Result := -1
  end
  else begin
    Result := 0;
    while Result < ilList.Count do begin
      if (pointer(Int64Rec(item).Lo) = ilList[Result]) and
         (pointer(Int64Rec(item).Hi) = ilList[Result+1]) then
      begin
        Result := Result div 2;
        Exit;  
      end;
      Inc(Result, 2);
    end;
    Result := -1;
  end;
end; { TGpInt64List.IndexOf }

procedure TGpInt64List.Insert(idx: integer; item: int64);
begin
  ilList.Insert(2*idx, pointer(Int64Rec(item).Hi));
  ilList.Insert(2*idx, pointer(Int64Rec(item).Lo));
end; { TGpInt64List.Insert }

function TGpInt64List.Last: int64;
begin
  Result := Items[Count-1];
end; { TGpInt64List.Last }

function TGpInt64List.LoadFromStream(stream: TStream): boolean;
var
  item: int64;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 8);
    if read = 8 then
      Add(item)
    else if read > 0 then
      Exit;
  until read = 0;
  Result := true;
end; { TGpInt64List.LoadFromStream }

procedure TGpInt64List.Move(curIdx, newIdx: integer);
begin
  ilList.Move(2*curIdx, 2*newIdx);
  ilList.Move(2*curIdx+1, newIdx+1);
end; { TGpInt64List.Move }

procedure TGpInt64List.QuickSort(L, R: integer; SCompare: TGpInt64ListSortCompare);
var
  I, J, P: integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while (SCompare(Self, I, P) < 0) do
        Inc(I);
      while (SCompare(Self, J, P) > 0) do
        Dec(J);
      if (I <= J) then begin
        Exchange(I, J);
        if (P = I) then
          P := J
        else if (P = J) then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until (I > J);
    if (L < J) then
      QuickSort(L, J, SCompare);
    L := I;
  until (I >= R);
end; { TGpInt64List.QuickSort }

procedure TGpInt64List.Remove(item: int64);
var
  idxItem: integer;
begin
  idxItem := IndexOf(item);
  if idxItem >= 0 then
    Delete(idxItem);
end; { TGpInt64List.Remove }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2006-09-20
}

type
  PInteger64 = ^Int64; // Workaround for Delphi 6 "Internal error: URW699" below

function TGpInt64List.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  numItems: integer;
  pList   : {$IFNDEF D7PLUS} PInteger64 {$ELSE} PLargeInteger {$ENDIF};
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    Add(int64(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpInt64List.Restore }

procedure TGpInt64List.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : int64;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 8);
  end;
end; { TGpInt64List.SaveToStream }

procedure TGpInt64List.SetCapacity(const value: integer);
begin
  ilList.Capacity := 2*value;
end; { TGpInt64List.SetCapacity }

procedure TGpInt64List.SetCount(const value: integer);
begin
  ilList.Count := 2*value;
end; { TGpInt64List.SetCount }

procedure TGpInt64List.SetItems(idx: integer; value: int64);
begin
  ilList.Items[2*idx] := pointer(Int64Rec(value).Lo);
  ilList.Items[2*idx+1] := pointer(Int64Rec(value).Hi);
end; { TGpInt64List.SetItems }

procedure TGpInt64List.SetSorted(const value: boolean);
begin
  if (ilSorted <> value) then begin
    if value then
      Sort;
    ilSorted := value;
  end;
end; { TGpInt64List.SetSorted }

procedure TGpInt64List.SetText(const value: string);
var
  p    : PChar;
  s    : string;
  start: PChar;
begin
  Clear;
  p := pointer(value);
  if P <> nil then
    while p^ <> #0 do begin
      start := p;
      while not CharInSet(p^, [#0, #10, #13]) do
        Inc(p);
      SetString(s, start, p - start);
      Add(StrToInt64(s));
      if p^ = #13 then Inc(p);
      if p^ = #10 then Inc(p);
    end;
end; { TGpInt64List.SetText }

procedure TGpInt64List.Sort;
begin
  if not Sorted and (Count > 1) then
    QuickSort(0, Count - 1, Int64ListCompare);
end; { TGpInt64List.Sort }

constructor TGpIntegerObjectList.Create(ownsObjects: boolean);
begin
  inherited Create;
  iolObjects := TObjectList.Create(ownsObjects);
end; { TGpIntegerObjectList.Create }

destructor TGpIntegerObjectList.Destroy;
begin
  FreeAndNil(iolObjects);
  inherited;
end; { TGpIntegerObjectList.Destroy }

{ TGpIntegerObjectList }

function TGpIntegerObjectList.Add(item: integer): integer;
begin
  Result := AddObject(item, nil); 
end; { TGpIntegerObjectList.Add }

function TGpIntegerObjectList.AddObject(item: integer;
  obj: TObject): integer;
begin
  Result := inherited Add(item);
  if not Sorted then // when Sorted, inherited Add calls Insert which updates iolObjects
    iolObjects.Insert(Result, obj)
  else
    Objects[Result] := obj;
end; { TGpIntegerObjectList.AddObject }                    

procedure TGpIntegerObjectList.Clear;
begin
  inherited;
  iolObjects.Clear;
end; { TGpIntegerObjectList.Clear }

procedure TGpIntegerObjectList.Delete(idx: integer);
begin
  inherited;
  iolObjects.Delete(idx);
end; { TGpIntegerObjectList.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpIntegerObjectList.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PDWORD;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := DWORD(Items[iList]);
    Inc(pList);
    pList^ := DWORD(Objects[iList]);
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerObjectList.Dump }

function TGpIntegerObjectList.EnsureObject(item: integer; obj: TObject): integer;
begin
  Result := inherited Ensure(item);
  Objects[Result] := obj;
end; { TGpIntegerObjectList.EnsureObject }

procedure TGpIntegerObjectList.Exchange(idx1, idx2: integer);
begin
  inherited;
  iolObjects.Exchange(idx1, idx2);
end; { TGpIntegerObjectList.Exchange }

function TGpIntegerObjectList.ExtractObject(idxObject: integer): TObject;
begin
  Result := Objects[idxObject];
  iolObjects.Extract(iolObjects[idxObject]);
  inherited Delete(idxObject);
end; { TGpIntegerObjectList.ExtractObject }

function TGpIntegerObjectList.GetObject(idxObject: integer): TObject;
begin
  Result := iolObjects[idxObject];
end; { TGpIntegerObjectList.GetObject }

procedure TGpIntegerObjectList.Insert(idx, item: integer);
begin
  InsertObject(idx, item, nil);
end; { TGpIntegerObjectList.Insert }

procedure TGpIntegerObjectList.InsertObject(idx, item: integer; obj: TObject);
begin
  inherited Insert(idx, item);
  iolObjects.Insert(idx, obj);
end; { TGpIntegerObjectList.InsertObject }

function TGpIntegerObjectList.LoadFromStream(stream: TStream): boolean;
var
  item: integer;
  obj : TObject;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 4);
    if read = 0 then
      break; //repeat
    if read <> 4 then
      Exit;
    read := stream.Read(obj, 4);
    if read <> 4 then
      Exit;
    AddObject(item, obj)
  until read = 0;
  Result := true;
end; { TGpIntegerObjectList.LoadFromStream }

procedure TGpIntegerObjectList.Move(curIdx, newIdx: integer);
begin
  inherited;
  iolObjects.Move(curIdx, newIdx);
end; { TGpIntegerObjectList.Move }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpIntegerObjectList.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  item    : integer;
  numItems: integer;
  pList   : PDWORD;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  iolObjects.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    item := integer(pList^);
    Inc(pList);
    AddObject(item, TObject(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpIntegerObjectList.Restore }

procedure TGpIntegerObjectList.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : integer;
  obj  : TObject;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 4);
    obj := Objects[iItem];
    stream.WriteBuffer(obj, 4);
  end;
end; { TGpIntegerObjectList.SaveToStream }

procedure TGpIntegerObjectList.SetObject(idxObject: integer; const value: TObject);
begin
  iolObjects[idxObject] := value;
end; { TGpIntegerObjectList.SetObject }

{ TGpInt64ObjectList }

constructor TGpInt64ObjectList.Create(ownsObjects: boolean);
begin
  inherited Create;
  iolObjects := TObjectList.Create(ownsObjects);
end; { TGpInt64ObjectList.Create }

destructor TGpInt64ObjectList.Destroy;
begin
  FreeAndNil(iolObjects);
  inherited;
end; { TGpInt64ObjectList.Destroy }

function TGpInt64ObjectList.Add(item: int64): integer;
begin
  Result := AddObject(item, nil);
end; { TGpInt64ObjectList.Add }

function TGpInt64ObjectList.AddObject(item: int64; obj: TObject): integer;
begin
  Result := inherited Add(item);
  if not Sorted then // when Sorted, inherited Add calls Insert which updates iolObjects
    iolObjects.Insert(Result, obj)
  else
    Objects[Result] := obj;
end; { TGpInt64ObjectList.AddObject }                    

procedure TGpInt64ObjectList.Clear;
begin
  inherited;
  iolObjects.Clear;
end; { TGpInt64ObjectList.Clear }

procedure TGpInt64ObjectList.Delete(idx: integer);
begin
  inherited;
  iolObjects.Delete(idx);
end; { TGpInt64ObjectList.Delete }

{:Dumps the list into memory block starting at baseAddr.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpInt64ObjectList.Dump(baseAddr: pointer): pointer;
var
  iList: integer;
  pList: PDWORD;
begin
  pList := baseAddr;
  pList^ := Count;
  Inc(pList);
  for iList := 0 to Count-1 do begin
    pList^ := DWORD(Int64Rec(Items[iList]).Lo);
    Inc(pList);
    pList^ := DWORD(Int64Rec(Items[iList]).Hi);
    Inc(pList);
    pList^ := DWORD(Objects[iList]);
    Inc(pList);
  end;
  Result := pList;
end; { TGpInt64ObjectList.Dump }

function TGpInt64ObjectList.EnsureObject(item: int64; obj: TObject): integer;
begin
  Result := inherited Ensure(item);
  Objects[Result] := obj;
end; { TGpInt64ObjectList.EnsureObject }

procedure TGpInt64ObjectList.Exchange(idx1, idx2: integer);
begin
  inherited;
  iolObjects.Exchange(idx1, idx2);
end; { TGpInt64ObjectList.Exchange }

function TGpInt64ObjectList.ExtractObject(idxObject: integer): TObject;
begin
  Result := Objects[idxObject];
  iolObjects.Extract(iolObjects[idxObject]);
  inherited Delete(idxObject);
end; { TGpInt64ObjectList.ExtractObject }

function TGpInt64ObjectList.GetObject(idxObject: integer): TObject;
begin
  Result := iolObjects[idxObject];
end; { TGpInt64ObjectList.GetObject }

procedure TGpInt64ObjectList.Insert(idx: integer; item: int64);
begin
  InsertObject(idx, item, nil);
end; { TGpInt64ObjectList.Insert }

procedure TGpInt64ObjectList.InsertObject(idx: integer; item: int64; obj: TObject);
begin
  inherited Insert(idx, item);
  iolObjects.Insert(idx, obj);
end; { TGpInt64ObjectList.InsertObject }

function TGpInt64ObjectList.LoadFromStream(stream: TStream): boolean;
var
  item: int64;
  obj : TObject;
  read: integer;
begin
  Result := false;
  Clear;
  repeat
    read := stream.Read(item, 8);
    if read = 0 then
      break; //repeat
    if read <> 4 then
      Exit;
    read := stream.Read(obj, 4);
    if read <> 4 then
      Exit;
    AddObject(item, obj)
  until read = 0;
  Result := true;
end; { TGpInt64ObjectList.LoadFromStream }

procedure TGpInt64ObjectList.Move(curIdx, newIdx: integer);
begin
  inherited;
  iolObjects.Move(curIdx, newIdx);
end; { TGpInt64ObjectList.Move }

{:Restores the list dumped by the Dump method.
  @returns Pointer to the byte immediately after the end of dumped data.
  @since   2004-11-22
}        
function TGpInt64ObjectList.Restore(baseAddr: pointer): pointer;
var
  iList   : integer;
  item    : int64;
  numItems: integer;
  pList   : PDWORD;
begin
  pList := baseAddr;
  numItems := integer(pList^);
  Inc(pList);
  ilList.Capacity := numItems;
  iolObjects.Capacity := numItems;
  for iList := 0 to numItems-1 do begin
    Int64Rec(item).Lo := cardinal(pList^);
    Inc(pList);
    Int64Rec(item).Hi := cardinal(pList^);
    Inc(pList);
    AddObject(item, TObject(pList^));
    Inc(pList);
  end;
  Result := pList;
end; { TGpInt64ObjectList.Restore }

procedure TGpInt64ObjectList.SaveToStream(stream: TStream);
var
  iItem: integer;
  item : int64;
  obj  : TObject;
begin
  for iItem := 0 to Count-1 do begin
    item := Items[iItem];
    stream.WriteBuffer(item, 8);
    obj := Objects[iItem];
    stream.WriteBuffer(obj, 4);
  end;
end; { TGpInt64ObjectList.SaveToStream }

procedure TGpInt64ObjectList.SetObject(idxObject: integer; const value: TObject);
begin
  iolObjects[idxObject] := value;
end; { TGpInt64ObjectList.SetObject }

{ TGpCountedStringList }

function CompareAscending(list: TStringList; index1, index2: integer): integer;
var
  item1: integer;
  item2: integer;
begin
  item1 := TGpCountedStringList(list).Counter[index1];
  item2 := TGpCountedStringList(list).Counter[index2];
  if item1 < item2 then
    Result := -1
  else if item1 > item2 then
    Result := 1
  else
    Result := 0;
end; { CompareAscending }

function CompareDescending(list: TStringList; index1, index2: integer): integer;
var
  item1: integer;
  item2: integer;
begin
  item1 := TGpCountedStringList(list).Counter[index1];
  item2 := TGpCountedStringList(list).Counter[index2];
  if item1 > item2 then
    Result := -1
  else if item1 < item2 then
    Result := 1
  else
    Result := 0;
end; { CompareDescending }

function TGpCountedStringList.Add(const s: string; count: integer): integer;
begin
  Result := inherited AddObject(s, pointer(count));
end; { TGpCountedStringList.Add }

function TGpCountedStringList.Ensure(const s: string;
  count: integer): integer;
begin
  Result := IndexOf(s);
  if Result < 0 then
    Result := Add(s, count)
  else
    Counter[Result] := count;
end; { TGpCountedStringList.Ensure }

function TGpCountedStringList.GetItemCount(idx: integer): integer;
begin
  Result := integer(Objects[idx]);
end; { TGpCountedStringList.GetItemCount }

procedure TGpCountedStringList.SetItemCount(idx: integer; const Value: integer);
begin
  Objects[idx] := pointer(Value);
end; { TGpCountedStringList.SetItemCount }

procedure TGpCountedStringList.SortByCounter(descending: boolean);
begin
  Sorted := false;
  if descending then
    CustomSort(CompareDescending)
  else
    CustomSort(CompareAscending);
  Sorted := false;
end; { TGpCountedStringList.SortByCounter }

{ TGpTMethodListEnumerator }

constructor TGpTMethodListEnumerator.Create(aList: TGpTMethodList);
begin
  inherited Create;
  mleIndex := -1;
  mleList := aList;
end; { TGpTMethodListEnumerator.Create }

function TGpTMethodListEnumerator.GetCurrent: TMethod;
begin
  Result := mleList[mleIndex];
end; { TGpTMethodListEnumerator.GetCurrent }

function TGpTMethodListEnumerator.MoveNext: boolean;
begin
  Result := mleIndex < (mleList.Count - 1);
  if Result then
    Inc(mleIndex);
end; { TGpTMethodListEnumerator.MoveNext }

constructor TGpTMethodList.Create;
begin
  inherited;
  mlCode := TList.Create;
  mlData := TList.Create;
end; { TGpTMethodList.Create }

destructor TGpTMethodList.Destroy;
begin
  FreeAndNil(mlData);
  FreeAndNil(mlCode);
  inherited;
end; { TGpTMethodList.Destroy }

{ TGpTMethodList }

function TGpTMethodList.Add(item: TMethod): integer;
begin
  Result := mlCode.Add(item.Code);
  mlData.Add(item.Data);
end; { TGpTMethodList.Add }

procedure TGpTMethodList.Assign(list: TGpTMethodList);
begin
  Clear;
  mlCode.Assign(list.mlCode);
  mlData.Assign(list.mlData);
end; { TGpTMethodList.Assign }

procedure TGpTMethodList.Clear;
begin
  mlCode.Clear;
  mlData.Clear;
end; { TGpTMethodList.Clear }

procedure TGpTMethodList.Delete(idx: integer);
begin
  mlCode.Delete(idx);
  mlData.Delete(idx);
end; { TGpTMethodList.Delete }

function TGpTMethodList.Ensure(item: TMethod): integer;
begin
  Result := IndexOf(item);
  if Result < 0 then
    Result := Add(item);
end; { TGpTMethodList.Ensure }

function TGpTMethodList.GetCapacity: integer;
begin
  Result := mlCode.Capacity;
end; { TGpTMethodList.GetCapacity }

function TGpTMethodList.GetCount: integer;
begin
  Result := mlCode.Count;
end; { TGpTMethodList.GetCount }

function TGpTMethodList.GetEnumerator: TGpTMethodListEnumerator;
begin
  Result := TGpTMethodListEnumerator.Create(Self);
end; { TGpTMethodList.GetEnumerator }

function TGpTMethodList.GetItems(idx: integer): TMethod;
begin
  Result.Code := mlCode[idx];
  Result.Data := mlData[idx];
end; { TGpTMethodList.GetItems }

function TGpTMethodList.IndexOf(item: TMethod): integer;
begin
  for Result := 0 to Count - 1 do
    if (mlCode[Result] = item.Code) and (mlData[Result] = item.Data) then
      Exit;
  Result := -1;
end; { TGpTMethodList.IndexOf }

procedure TGpTMethodList.Insert(idx: integer; item: TMethod);
begin
  mlCode.Insert(idx, item.Code);
  mlData.Insert(idx, item.Data);
end; { TGpTMethodList.Insert }

procedure TGpTMethodList.Remove(item: TMethod);
var
  idxMethod: integer;
begin
  idxMethod := IndexOf(item);
  if idxMethod >= 0 then
    Delete(idxMethod);
end; { TGpTMethodList.Remove }

procedure TGpTMethodList.SetCapacity(const value: integer);
begin
  mlCode.Capacity := value;
  mlData.Capacity := value;
end; { TGpTMethodList.SetCapacity }

procedure TGpTMethodList.SetCount(const value: integer);
begin
  mlCode.Count := value;
  mlData.Count := value;
end; { TGpTMethodList.SetCount }

procedure TGpTMethodList.SetItems(idx: integer; const value: TMethod);
begin
  mlCode[idx] := value.Code;
  mlData[idx] := value.Data;
end; { TGpTMethodList.SetItems }

{:Creates and initializes ring buffer.
  @since   2003-07-25
}
constructor TGpObjectRingBuffer.Create(bufferSize: integer;
  ownsObjects: boolean);
begin
  if bufferSize = 0 then
    raise Exception.Create('TGpObjectRingBuffer.Create: buffer size is 0');
  orbBufferSize := bufferSize;
  orbOwnsObjects := ownsObjects;
  SetLength(orbBuffer, orbBufferSize+1);
  orbHead := 0;
  orbTail := 0;
end; { TGpObjectRingBuffer.Create }

{:Destroys ring buffer. If OwnsObjects is set, destroys all objects currently
  in the buffer.
  @since   2003-07-26
}
destructor TGpObjectRingBuffer.Destroy;
begin
  Clear;
  inherited;
end; { TGpObjectRingBuffer.Destroy }

{ TGpObjectRingBuffer }

procedure TGpObjectRingBuffer.Clear;
begin
  if orbOwnsObjects then begin
    while not IsEmpty do
      Dequeue.Free;
  end
  else
    orbTail := orbHead;
end; { TGpObjectRingBuffer.Clear }

{:Returns number of objects in the buffer.
  @since   2003-07-26
}        
function TGpObjectRingBuffer.Count: integer;
begin
  Result := ((orbHead + orbBufferSize + 1) - orbTail) mod (orbBufferSize + 1);
end; { TGpObjectRingBuffer.Count }

{:Removes tail object from the buffer, without destroying it. Returns nil if
  buffer is empty.
  @since   2003-07-26
}        
function TGpObjectRingBuffer.Dequeue: TObject;
begin
  if IsEmpty then
    Result := nil
  else begin
    Result := orbBuffer[orbTail];
    orbTail := IncPointer(orbTail);
  end;
end; { TGpObjectRingBuffer.Dequeue }

{:Inserts object into the buffer. Returns false if the buffer is full.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Enqueue(obj: TObject): boolean;
begin
  if IsFull then
    Result := false
  else begin
    orbBuffer[orbHead] := obj;
    orbHead := IncPointer(orbHead);
    Result := true;
  end;
end; { TGpObjectRingBuffer.Enqueue }

function TGpObjectRingBuffer.GetItem(iObject: integer): TObject;
begin
  if (iObject < 0) or (iObject >= Count) then
    raise Exception.CreateFmt('TGpObjectRingBuffer.GetItem: Invalid index %d', [iObject])
  else
    Result := orbBuffer[IncPointer(orbTail, iObject)];
end; { TGpObjectRingBuffer.GetItem }

{:Returns head (newest) object or nil if the buffer is empty.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Head: TObject;
begin
  if IsEmpty then
    Result := nil
  else
    Result := orbBuffer[IncPointer(orbTail, Count-1)];
end; { TGpObjectRingBuffer.Head }

{:Increments internal pointer (head or tail), wraps it to the buffer size and
  returns new value.
  @since   2003-07-26
}        
function TGpObjectRingBuffer.IncPointer(const ptr: integer;
  increment: integer): integer;
begin
  Result := (ptr + increment) mod (orbBufferSize + 1);
end; { TGpObjectRingBuffer.IncPointer }

{:Checks whether the buffer is empty.
  @since   2003-07-26
}
function TGpObjectRingBuffer.IsEmpty: boolean;
begin
  Result := (orbHead = orbTail);
end; { TGpObjectRingBuffer.IsEmpty }

{:Checks whether the buffer is full.
  @since   2003-07-26
}
function TGpObjectRingBuffer.IsFull: boolean;
begin
  Result := (IncPointer(orbHead) = orbTail);
end; { TGpObjectRingBuffer.IsFull }

procedure TGpObjectRingBuffer.SetItem(iObject: integer;
  const Value: TObject);
var
  idxObject: integer;
begin
  if (iObject < 0) or (iObject >= Count) then
    raise Exception.CreateFmt('TGpObjectRingBuffer.SetItem: Invalid index %d', [iObject])
  else begin
    idxObject := IncPointer(orbTail, iObject);
    if orbOwnsObjects then
      orbBuffer[idxObject].Free;
    orbBuffer[idxObject] := Value;
  end;
end; { TGpObjectRingBuffer.SetItem }

{:Returns tail (oldest) object or nil if the buffer is empty.
  @since   2003-07-26
}
function TGpObjectRingBuffer.Tail: TObject;
begin
  if IsEmpty then
    Result := nil
  else
    Result := orbBuffer[orbTail];
end; { TGpObjectRingBuffer.Tail }

constructor TGpObjectMap.Create(ownsObjects: boolean);
begin
  inherited Create;
  omList := TGpIntegerObjectList.Create(ownsObjects);
end; { TGpObjectMap.Create }

destructor TGpObjectMap.Destroy;
begin
  FreeAndNil(omList);
  inherited;
end; { TGpObjectMap.Destroy }

{ TGpObjectMap }

procedure TGpObjectMap.Clear;
begin
  omList.Clear;
end; { TGpObjectMap.Clear }

function TGpObjectMap.Count: integer;
begin
  Result := omList.Count;
end; { TGpObjectMap.Count }

function TGpObjectMap.Exists(value: TObject; compareFunc: TGpObjectMapCompare): boolean;
var
  item: TObject;
begin
  Find(value, compareFunc, item);
  Result := assigned(item);
end; { TGpObjectMap.Exists }

procedure TGpObjectMap.Find(value: TObject; compareFunc: TGpObjectMapCompare;
  var item: TObject);
var
  iItem: integer;
begin
  for iItem := 0 to Count-1 do begin
    if compareFunc(value, omList.Objects[iItem]) then begin
      item := TObject(omList[iItem]);
      Exit;
    end;
  end; //for
  item := nil;
end; { TGpObjectMap.Find }

function TGpObjectMap.GetIndexedItem(idxItem: integer): TObject;
begin
  Result := TObject(omList[idxItem]);
end; { TGpObjectMap.GetIndexedItem }

function TGpObjectMap.GetIndexedValue(idxValue: integer): TObject;
begin
  Result := omList.Objects[idxValue];
end; { TGpObjectMap.GetIndexedObject }

function TGpObjectMap.GetItems(item: TObject): TObject;
var
  idxItem: integer;
begin
  idxItem := omList.IndexOf(integer(item));
  if idxItem >= 0 then
    Result := omList.Objects[idxItem]
  else
    Result := nil;
end; { TGpObjectMap.GetItems }

procedure TGpObjectMap.SetItems(item: TObject; const value: TObject);
var
  idxItem: integer;
begin
  idxItem := omList.IndexOf(integer(item));
  if idxItem >= 0 then begin
    if assigned(value) then
      omList.Objects[idxItem] := value
    else
      omList.Delete(idxItem);
  end
  else
    omList.AddObject(integer(item), value);
end; { TGpObjectMap.SetItems }

constructor TGpObjectObjectMap.Create(ownsObjects: boolean);
begin
  inherited Create;
  oomMap := TGpObjectMap.Create(true);
  oomOwnsObjects := ownsObjects;
end; { TGpObjectObjectMap.Create }

destructor TGpObjectObjectMap.Destroy;
begin
  FreeAndNil(oomMap);
  inherited;
end; { TGpObjectObjectMap.Destroy }

{ TGpObjectObjectMap }

function MapCompare(userValue, mapValue: TObject): boolean;
begin
  with TGpObjectObjectMap(userValue) do begin
    TGpObjectMap(mapValue).Find(oomFindValue, oomCompareFunc, oomItem2);
    Result := assigned(oomItem2);
  end; //with
end; { MapCompare }

procedure TGpObjectObjectMap.Clear;
begin
  oomMap.Clear;
end; { TGpObjectObjectMap.Clear }

function TGpObjectObjectMap.Exists(value: TObject;
  compareFunc: TGpObjectMapCompare): boolean;
var
  item1: TObject;
  item2: TObject;
begin
  Find(value, compareFunc, item1, item2);
  Result := assigned(item1) and assigned(item2);
end; { TGpObjectObjectMap.Exists }

procedure TGpObjectObjectMap.Find(value: TObject; compareFunc: TGpObjectMapCompare;
  var item1, item2: TObject);
begin
  oomFindValue := value;
  oomCompareFunc := compareFunc;
  oomMap.Find(Self, MapCompare, item1);
  item2 := oomItem2;
end; { TGpObjectObjectMap.Find }

function TGpObjectObjectMap.GetItems(item1, item2: TObject): TObject;
begin
  Result := Map(item1)[item2];
end; { TGpObjectObjectMap.GetItems }

function TGpObjectObjectMap.Map(item: TObject): TGpObjectMap;
begin
  Result := TGpObjectMap(oomMap[item]);
  if not assigned(Result) then begin
    Result := TGpObjectMap.Create(oomOwnsObjects);
    oomMap[item] := Result;
  end;
end; { TGpObjectObjectMap.Map }

procedure TGpObjectObjectMap.SetItems(item1, item2: TObject;
  const Value: TObject);
begin
  Map(item1)[item2] := Value;
end; { TGpObjectObjectMap.SetItems }

{ TGpDoublyLinkedListObject }

{:Unlinks the object from the list and destroys it.
  @since   2003-10-28
}        
destructor TGpDoublyLinkedListObject.Destroy;
begin
  Unlink;
  inherited;
end; { TGpDoublyLinkedListObject.Destroy }

{:Inserts self into the doubly-linked list after the specified object.
  @since   2003-10-28
}
procedure TGpDoublyLinkedListObject.LinkAfter(list: TGpDoublyLinkedList;
  obj: TGpDoublyLinkedListObject);
begin
  Unlink;
  list.Lock;
  try
    dlloNext := obj.dlloNext;
    dlloNext.dlloPrevious := Self;
    dlloPrevious := obj;
    dlloPrevious.dlloNext := Self;
    dlloList := list;
    dlloList.Linking(Self);
  finally list.Unlock; end;
end; { TGpDoublyLinkedListObject.LinkAfter }

{:Returns the next object in the list or nil if next object is the tail
  sentinel.
  @since   2003-10-27
}
function TGpDoublyLinkedListObject.Next: TGpDoublyLinkedListObject;
begin
  dlloList.Lock;
  try
    Result := dlloNext;
    if Result.dlloNext = nil then // we are at the tail sentinel
      Result := nil;
  finally dlloList.Unlock; end;
end; { TGpDoublyLinkedListObject.Next }

{:Returns the previous object in the list or nil if next object is the tail
  sentinel.
  @since   2003-10-27
}
function TGpDoublyLinkedListObject.Previous: TGpDoublyLinkedListObject;
begin
  dlloList.Lock;
  try
    Result := dlloPrevious;
    if Result.dlloPrevious = nil then // we are at the head sentinel
      Result := nil;
  finally dlloList.Unlock; end;
end; { TGpDoublyLinkedListObject.Previous }

{:Unlinks the object from the list.
  @since   2003-10-28
}
procedure TGpDoublyLinkedListObject.Unlink;
begin
  if assigned(dlloList) then begin
    dlloList.Lock;
    try
      if assigned(dlloNext) then
        dlloNext.dlloPrevious := dlloPrevious;
      if assigned(dlloPrevious) then
        dlloPrevious.dlloNext := dlloNext;
      dlloList.Unlinking(Self);
    finally dlloList.Unlock; end;
  end;
  dlloList := nil;
  dlloNext := nil;
  dlloPrevious := nil;
end; { TGpDoublyLinkedListObject.Unlink }

{ TGpDoublyLinkedList }

constructor TGpDoublyLinkedList.Create(multithreaded: boolean);
begin
  inherited Create;
  if multithreaded then
    dllLock := TCriticalSection.Create;
  dllHead := TGpDoublyLinkedListObject.Create;
  dllTail := TGpDoublyLinkedListObject.Create;
  dllHead.dlloNext := dllTail;
  dllTail.dlloPrevious := dllHead;
end; { TGpDoublyLinkedList.Create }

destructor TGpDoublyLinkedList.Destroy;
begin
  FreeAndNil(dllHead);
  FreeAndNil(dllTail);
  FreeAndNil(dllLock);
  inherited;
end; { TGpDoublyLinkedList.Destroy }

{:Returns number of items in the list.
  @since   2003-10-28
}
function TGpDoublyLinkedList.Count: integer;
begin
  Result := dllCount;
end; { TGpDoublyLinkedList.Count }

{:Destroy all elements of the list.
  @since   2005-06-02
}
procedure TGpDoublyLinkedList.FreeAll;
begin
  Lock;
  try
    while Tail <> nil do
      Tail.Free;
  finally Unlock; end;
end; { TGpDoublyLinkedList.FreeAll }

{:Returns first element in the list or nil if list is empty.
  @since   2003-10-28
}
function TGpDoublyLinkedList.Head: TGpDoublyLinkedListObject;
begin
  Lock;
  try
    if IsEmpty then
      Result := nil
    else
      Result := dllHead.dlloNext;
  finally Unlock; end;
end; { TGpDoublyLinkedList.Head }

{:Inserts the object into the list after the existing linked object.
  @since   2003-10-28
}        
procedure TGpDoublyLinkedList.InsertAfter(existingObject,
  obj: TGpDoublyLinkedListObject);
begin
  obj.LinkAfter(Self, existingObject);
end; { TGpDoublyLinkedList.InsertAfter }

procedure TGpDoublyLinkedList.InsertAtHead(obj: TGpDoublyLinkedListObject);
begin
  InsertAfter(dllHead, obj);
end; { TGpDoublyLinkedList.InsertAtHead }

procedure TGpDoublyLinkedList.InsertAtTail(obj: TGpDoublyLinkedListObject);
begin
  InsertBefore(dllTail, obj);
end; { TGpDoublyLinkedList.InsertAtTail } 

procedure TGpDoublyLinkedList.InsertBefore(existingObject,
  obj: TGpDoublyLinkedListObject);
begin
  InsertAfter(existingObject.dlloPrevious, obj);
end; { TGpDoublyLinkedList.InsertBefore }

function TGpDoublyLinkedList.IsEmpty: boolean;
begin
  Result := (dllHead.dlloNext = dllTail);
end; { TGpDoublyLinkedList.IsEmpty }

{:Called from the linked object when it is being linked from the list.
  @since   2003-10-28
}
procedure TGpDoublyLinkedList.Linking(obj: TGpDoublyLinkedListObject);
begin
  Inc(dllCount);
end; { TGpDoublyLinkedList.Linking }

procedure TGpDoublyLinkedList.Lock;
begin
  if assigned(dllLock) then
    dllLock.Enter;
end; { TGpDoublyLinkedList.Lock }

function TGpDoublyLinkedList.Next(
  obj: TGpDoublyLinkedListObject): TGpDoublyLinkedListObject;
begin
  Result := obj.Next;
end; { TGpDoublyLinkedList.Next }

function TGpDoublyLinkedList.Previous(
  obj: TGpDoublyLinkedListObject): TGpDoublyLinkedListObject;
begin
  Result := obj.Previous;
end; { TGpDoublyLinkedList.Previous }

function TGpDoublyLinkedList.RemoveFromHead: TGpDoublyLinkedListObject;
begin
  Lock;
  try
    Result := Head;
    if assigned(Result) then
      Result.Unlink;
  finally Unlock; end;
end; { TGpDoublyLinkedList.RemoveFromHead }

function TGpDoublyLinkedList.RemoveFromTail: TGpDoublyLinkedListObject;
begin
  Lock;
  try
    Result := Tail;
    if assigned(Result) then
      Result.Unlink;
  finally Unlock; end;
end; { TGpDoublyLinkedList.RemoveFromTail }

function TGpDoublyLinkedList.Tail: TGpDoublyLinkedListObject;
begin
  Lock;
  try
    if IsEmpty then
      Result := nil
    else
      Result := dllTail.dlloPrevious;
  finally Unlock; end;
end; { TGpDoublyLinkedList.Tail }

procedure TGpDoublyLinkedList.Unlink(obj: TGpDoublyLinkedListObject);
begin
  obj.Unlink;
end; { TGpDoublyLinkedList.Unlink }

{:Remove all elements from the list without destroying them.
  @since   2005-06-02
}
procedure TGpDoublyLinkedList.UnlinkAll;
begin
  Lock;
  try
    while Tail <> nil do
      RemoveFromTail;
  finally Unlock; end;
end; { TGpDoublyLinkedList.UnlinkAll }

{:Called from the linked object when it is being unliniked from the list.
  @since   2003-10-28
}
procedure TGpDoublyLinkedList.Unlinking(obj: TGpDoublyLinkedListObject);
begin
  Dec(dllCount);
end; { TGpDoublyLinkedList.Unlinking }

procedure TGpDoublyLinkedList.Unlock;
begin
  if assigned(dllLock) then
    dllLock.Leave;
end; { TGpDoublyLinkedList.Unlock }

end.

