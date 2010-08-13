(*
 * GExperts Debug Window Interface
 * http://www.gexperts.org
 *
 * You are free to use this code in any application to send commands to the
 * GExperts debug window.  This includes usage in commercial, shareware,
 * freeware, public domain, and other applications.
 *)

unit DbugIntf;

interface

uses
  Classes,
  Windows, Dialogs;

type
  TDebugLevel = (dlObject);

procedure SendBoolean(const SenderLevel: TDebugLevel; const Identifier: string; const Value: Boolean);
procedure SendDateTime(const SenderLevel: TDebugLevel; const Identifier: string; Value: TDateTime = 0);
procedure SendDebugEx(const SenderLevel: TDebugLevel; const Msg: string; MType: TMsgDlgType);
procedure SendDebug(const SenderLevel: TDebugLevel; const Msg: string);
procedure SendInteger(const SenderLevel: TDebugLevel; const Identifier: string; const Value: Integer);
procedure SendMethodEnter(const SenderLevel: TDebugLevel; const MethodName: string);
procedure SendMethodExit(const SenderLevel: TDebugLevel; const MethodName: string);

procedure SendDebugFmt(const SenderLevel: TDebugLevel; const Msg: string; const Args: array of const);
procedure SendDebugFmtEx(const SenderLevel: TDebugLevel; const Msg: string; const Args: array of const; MType: TMsgDlgType);
procedure SendDebugFmtEx_Start(const SenderLevel: TDebugLevel; const Msg: string; const Args: array of const; MType: TMsgDlgType);
procedure SendDebugFmtEx_End(const SenderLevel: TDebugLevel; const Msg: string; const Args: array of const; MType: TMsgDlgType);

function  StartDebugWin: hWnd;
procedure SendDebugPause;
procedure SendDebugResume;

//send GDebug messages via thread (faster!)
type
  TGDebugSendThread = class(TThread)
  type
    TLogNotification = reference to procedure(aMsg: string);
  private
    FAllDataIsSend: Boolean;
    FData: TThreadList;
    FDataClone: TList;
    FOnLog: TLogNotification;
  protected
    procedure Execute; override;
    procedure SendAllData;
  public
    procedure  AfterConstruction; override;
    destructor Destroy; override;

    procedure  SendData(aPCDS: PCopyDataStruct);

    property   OnLog: TLogNotification read FOnLog write FOnLog;
  end;

  TThreadListHelper = class helper for TThreadList
  public
    function Count: Integer;
  end;

  function DebugSendThread: TGDebugSendThread;

implementation

uses
  Messages,
  SysUtils,
  Registry,
  Forms,  // We need "Forms" for the Application object
  StrUtils,
  DateUtils;

threadvar
  MsgPrefix: String;
  _IndentTime: array of TDateTime; 

const
  Indentation = '   ';
  chrClearCommand = #3;

var
  PastFailedAttemptToStartDebugWin: Boolean = False;
  SendPaused: Boolean = False;
  _DebugSendThread:TGDebugSendThread;

function DebugSendThread: TGDebugSendThread;
begin
  if _DebugSendThread = nil then
    _DebugSendThread := TGDebugSendThread.Create(False);
  Result := _DebugSendThread;
end;

function StartDebugWin: hWnd;
var
  DebugFileName: string;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  MsgPrefix := '';

  Result := 0;
  if PastFailedAttemptToStartDebugWin then
    Exit;

  with TRegIniFile.Create('\Software\GExperts') do
  try
    DebugFileName := ReadString('Debug', 'FilePath', '');
  finally
    Free;
  end;

  if Trim(DebugFileName) = '' then
  begin
    //GetModuleFileName(HINSTANCE, Buf, SizeOf(Buf)-1);
    DebugFileName := ExtractFilePath(Application.ExeName) + 'GExpertsDebugWindow.exe';
    if not FileExists(DebugFileName) then
      DebugFileName := ExtractFilePath(Application.ExeName) + 'GDebug.exe';
  end;

  if (Trim(DebugFileName) = '') or not FileExists(DebugFileName) then
  begin
    PastFailedAttemptToStartDebugWin := True;
    Exit;
  end;

  FillChar(si, SizeOf(si), #0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOW;
  if not CreateProcess(PChar(DebugFileName), nil,
                       nil, nil,
                       False, 0, nil, nil,
                       si, pi) then
  begin
    PastFailedAttemptToStartDebugWin := True;
    Exit;
  end;

  try
    WaitForInputIdle(pi.hProcess, 3 * 1000); // wait max 3 seconds to get idle
  finally
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
  end;

  Result := FindWindow('TfmDebug', nil);
end;

procedure SendDebugEx(const SenderLevel: TDebugLevel; const Msg: string; MType: TMsgDlgType);
var
  CDS: TCopyDataStruct;
  MessageString: string;
begin
  if SendPaused then
    Exit;

  MessageString := MsgPrefix + Msg;
  //log which app has log this line, useful in case of client-server communication (e.g. PRS + Transaction Service)
  MessageString := Format('PID %d|TID %d| %s - %s',
                          [GetCurrentProcessId, GetCurrentThreadId,
                           ExtractFileName(Application.ExeName), MessageString]);

  CDS.cbData := Length(MessageString) + 4;
  CDS.dwData := 0;
  if Msg = chrClearCommand then
    MessageString := chrClearCommand + Char(Ord(MType)+1) + MessageString + #0
  else
    MessageString := #1 + Char(Ord(MType)+1) + MessageString + #0;
  CDS.lpData := pansichar(AnsiString(MessageString));

  //send GDebug messages via thread (faster!)
  //SendMessage(DebugWin, WM_COPYDATA, WPARAM(Application.Handle), LPARAM(@CDS));
  DebugSendThread.SendData(@CDS);
end;

procedure SendDebug(const SenderLevel: TDebugLevel; const Msg: string);
begin
  SendDebugEx(SenderLevel, Msg, mtInformation);
end;

procedure SendDebugFmt(const SenderLevel: TDebugLevel; const Msg: string; const Args: array of const);
begin
  SendDebugEx(SenderLevel, Format(Msg, Args), mtInformation);
end;

procedure SendDebugFmtEx(const SenderLevel: TDebugLevel; const Msg: string; const Args: array of const; MType: TMsgDlgType);
begin
  SendDebugEx(SenderLevel, Format(Msg, Args), MType);
end;

procedure SendDebugFmtEx_Start(const SenderLevel: TDebugLevel; const Msg: string; const Args: array of const; MType: TMsgDlgType);
var
  i: Integer;
begin
  SendDebugEx(SenderLevel, Format(Msg, Args), MType);
  MsgPrefix := MsgPrefix + Indentation;

  i := Length(_IndentTime);
  SetLength(_IndentTime, i + 1);
  _IndentTime[i] := Now;
end;

procedure SendDebugFmtEx_End(const SenderLevel: TDebugLevel; const Msg: string; const Args: array of const; MType: TMsgDlgType);
var
  i: Integer;
  d: TDatetime;
begin
  Delete(MsgPrefix, 1, Length(Indentation));

  if Length(_IndentTime) > 0 then
  begin
    i := Length(_IndentTime)-1;
    d := _IndentTime[i];
    SendDebugEx(SenderLevel, Format(Msg, Args) +
                             Format(', duration %dms', [MilliSecondsBetween(Now, d)]),
                MType);
  end
  else
  begin
    i := 0;
    SendDebugEx(SenderLevel, Format(Msg, Args) +
                           Format(', duration unknown', []),
              MType);
  end;
  SetLength(_IndentTime, i);
end;

procedure SendMethodEnter(const SenderLevel: TDebugLevel; const MethodName: string);
begin
  MsgPrefix := MsgPrefix + Indentation;
  SendDateTime( SenderLevel, 'Entering ' + MethodName );
end;

procedure SendMethodExit(const SenderLevel: TDebugLevel; const MethodName: string);
begin
  SendDatetime( SenderLevel, 'Exiting ' + MethodName );

  Delete(MsgPrefix, 1, Length(Indentation));
end;

procedure SendBoolean(const SenderLevel: TDebugLevel; const Identifier: string; const Value: Boolean);
begin
  // Note: We deliberately leave "True" and "False" as
  // hard-coded string constants, since these are
  // technical terminology which should not be localised.
  if Value then
    SendDebugEx(SenderLevel, Identifier + ' = True', mtInformation)
  else
    SendDebugEx(SenderLevel, Identifier + ' = False', mtInformation);
end;

procedure SendInteger(const SenderLevel: TDebugLevel; const Identifier: string; const Value: Integer);
begin
  SendDebugEx(SenderLevel, Format('%s = %d', [Identifier, Value]), mtInformation);
end;

procedure SendDateTime(const SenderLevel: TDebugLevel; const Identifier: string; Value: TDateTime );
begin
  if ( Value = 0.0 )
  then Value := Now;
  SendDebugEx(SenderLevel, FormatDateTime( 'dd-mm-yyyy hh:nn:ss:zzz', Value) + '>> ' + Identifier, mtInformation);
end;

procedure SendDebugPause;
begin
  SendPaused := True;
end;

procedure SendDebugResume;
begin
  SendPaused := False;
end;

{ TGDebugSendThread }

procedure TGDebugSendThread.AfterConstruction;
begin
  inherited;
  FData      := TThreadList.Create;
  FDataClone := TList.Create;
  Priority   := tpLower;      //lower prio

  FAllDataIsSend := True;
end;

destructor TGDebugSendThread.Destroy;
begin
  SendAllData;
  FDataClone.Free;
  FData.Free;
  inherited;
end;

procedure TGDebugSendThread.Execute;
var Msg: TMsg;
begin
  inherited;
  TThread.NameThreadForDebugging( AnsiString(Self.ClassName) );

  while not Terminated do
  begin
    FAllDataIsSend := True;
    WaitMessage;

    while PeekMessage(Msg, 0, 0, 0, PM_REMOVE) do
    begin
      //WM_USER is just the first available message, we use only one.
      //It means we have a new data to send
      if Msg.message = WM_USER then
        try
          while FData.Count > 0 do
            SendAllData;
        except
          //ignore, just retry
          Sleep(100);
        end
      else
      begin
        TranslateMessage(Msg);
        DispatchMessage(Msg);
      end;
    end;
  end;
end;

procedure TGDebugSendThread.SendAllData;
var
  pCDS: PCopyDataStruct;
  l: TList;
  DebugWin: hWnd;
  str: TStrings;
  s: string;
begin
  str := nil;
  FDataClone.Clear;
  //first copy all data to new list (so we block new data writers as least as possible)
  l := FData.LockList;
  try
    FDataClone.Assign(l);
    l.Clear;
  finally
    FData.UnlockList;
  end;

  DebugWin := 0;
  //get GDebug windows
  if FDataClone.Count > 0 then
  begin
    DebugWin := FindWindow('TfmDebug', nil);

    if DebugWin = 0 then
      DebugWin := StartDebugWin;
  end;

  try
    //send all data to GDebug
    while FDataClone.Count > 0 do
    begin
      pCDS := FDataClone.Items[0];

      if DebugWin <> 0 then
      begin
        //SendMessage(DebugWin, WM_COPYDATA, WPARAM(Application.Handle), LPARAM(pCDS));
        SendMessage(DebugWin, WM_COPYDATA, 0{WPARAM(Self.Handle)}, LPARAM(pCDS));
      end;

      //log to console etc
      if Assigned(FOnLog) then
      begin
        s := pansichar(pCDS.lpData);
        if s <> '' then
          s := Copy(s, 3, Length(s));  //remove gdebug control char
        FOnLog(s);
      end;

      FreeMem(pCDS.lpData);   //free string
      Dispose(pCDS);          //free record
      FDataClone.Delete(0);
    end;

  finally
    str.Free;
  end;

  FAllDataIsSend := True;
end;

procedure TGDebugSendThread.SendData(aPCDS: PCopyDataStruct);
var
  pCDS: PCopyDataStruct;
  pData: PAnsiChar;
begin
  //get new mem
  new(pCDS);
  //clone record data (so we have our own copy)
  pCDS.dwData := aPCDS.dwData;
  pCDS.cbData := aPCDS.cbData;

  //clone string
  GetMem(pData, aPCDS.cbData);
  pData := StrCopy(pData, pansichar(aPCDS.lpData) );
  pCDS.lpData := pData;

  //add to list
  FData.Add(pCDS);

  //notify thread (only if "sleeping")
  if FAllDataIsSend then
  begin
    FAllDataIsSend := False;
    PostThreadMessage(Self.ThreadID, WM_USER, 0, 0);
  end;
end;

{ TThreadListHelper }

function TThreadListHelper.Count: Integer;
var
  l: TList;
begin
  l := LockList;
  try
    Result := l.Count;
  finally
    UnlockList;
  end;
end;

initialization

finalization
  if _DebugSendThread <> nil then
  begin
    _DebugSendThread.Terminate;
    PostThreadMessage(_DebugSendThread.ThreadID, WM_USER, 0, 0);  //wake up
    _DebugSendThread.Waitfor;
    FreeAndNil(_DebugSendThread);
  end;

end.

