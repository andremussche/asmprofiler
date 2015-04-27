unit mcExtraImageHlp;

interface

uses
  JwaWinType;

const
  //DbgHelp 6.1 and earlier:  This value is not supported.
  MemoryInfoListStream        = 16;
  ThreadInfoListStream        = 17;
  HandleOperationListStream   = 18;

  //MINIDUMP_TYPE
  MiniDumpWithoutOptionalData              = $00000400;
  MiniDumpWithFullMemoryInfo               = $00000800;
  MiniDumpWithThreadInfo                   = $00001000;
  MiniDumpWithCodeSegs                     = $00002000;
  MiniDumpWithoutAuxiliaryState            = $00004000;
  MiniDumpWithFullAuxiliaryState           = $00008000;
  MiniDumpWithPrivateWriteCopyMemory       = $00010000;
  MiniDumpIgnoreInaccessibleMemory         = $00020000;
  MiniDumpWithTokenInformation             = $00040000;

type
  PMINIDUMP_THREAD_INFO = ^MINIDUMP_THREAD_INFO;
  {$EXTERNALSYM PMINIDUMP_THREAD_INFO}
  _MINIDUMP_THREAD_INFO = record
    ThreadId,
    DumpFlags,                   //The flags that indicate the thread state. This member can be 0 or one of the MINIDUMP_THREAD_INFO_xxx values.
    DumpError,                   //An HRESULT value that indicates the dump status.
    ExitStatus: ULONG32;         //The thread termination status code.
    CreateTime,                  //The time when the thread was created, in 100-nanosecond intervals since January 1, 1601 (UTC).
    ExitTime,                    //The time when the thread exited, in 100-nanosecond intervals since January 1, 1601 (UTC).
    KernelTime,                  //The time executed in kernel mode, in 100-nanosecond intervals.
    UserTime,                    //The time executed in user mode, in 100-nanosecond intervals.
    StartAddress,                //The starting address of the thread.
    Affinity: ULONG64;           //The processor affinity mask.

    //MINIDUMP_THREAD_INFO.DumpFlags: The flags that indicate the thread state:
    const
      //A placeholder thread due to an error accessing the thread. No thread information exists beyond the thread identifier.
      MINIDUMP_THREAD_INFO_ERROR_THREAD = $00000001;
      //The thread has exited (not running any code) at the time of the dump.
      MINIDUMP_THREAD_INFO_EXITED_THREAD = $00000004;
      //Thread context could not be retrieved.
      MINIDUMP_THREAD_INFO_INVALID_CONTEXT = $00000010;
      //Thread information could not be retrieved.
      MINIDUMP_THREAD_INFO_INVALID_INFO = $00000008;
      //TEB information could not be retrieved.
      MINIDUMP_THREAD_INFO_INVALID_TEB = $00000020;
      //This is the thread that called MiniDumpWriteDump.
      MINIDUMP_THREAD_INFO_WRITING_THREAD = $00000002;
  end;
  {$EXTERNALSYM _MINIDUMP_THREAD_INFO}
  MINIDUMP_THREAD_INFO = _MINIDUMP_THREAD_INFO;
  {$EXTERNALSYM MINIDUMP_THREAD_INFO}
  TMinidumpThread_INFO = MINIDUMP_THREAD_INFO;
  PMinidumpThread_INFO = PMINIDUMP_THREAD_INFO;

  PMINIDUMP_THREAD_INFO_LIST = ^MINIDUMP_THREAD_INFO_LIST;
  _MINIDUMP_THREAD_INFO_LIST = record
    SizeOfHeader,                //The size of the header data for the stream, in bytes. This is generally sizeof(MINIDUMP_THREAD_INFO_LIST).
    SizeOfEntry: ULONG;          //The size of each entry following the header, in bytes. This is generally sizeof(MINIDUMP_THREAD_INFO).
    NumberOfEntries: ULONG64;    //The number of entries in the stream. These are generally MINIDUMP_THREAD_INFO structures. The entries follow the header.
    Threads: array[0..0] of _MINIDUMP_THREAD_INFO;
  end;
  MINIDUMP_THREAD_INFO_LIST = _MINIDUMP_THREAD_INFO_LIST;
  TMinidumpThread_INFO_LIST = MINIDUMP_THREAD_INFO_LIST;
  PMinidumpThread_INFO_LIST = PMINIDUMP_THREAD_INFO_LIST;

implementation

end.
