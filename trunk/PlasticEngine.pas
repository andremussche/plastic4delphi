{**************************************************************************************************}
{                                                                                                  }
{ Plastic SCM for Delphi plugin (Plastic4Delphi)                                                   }
{                                                                                                  }
{ The contents of this file are subject to the Mozilla Public License Version 1.1 (the "License"); }
{ you may not use this file except in compliance with the License. You may obtain a copy of the    }
{ License at http://www.mozilla.org/MPL/                                                           }
{                                                                                                  }
{ Software distributed under the License is distributed on an "AS IS" basis, WITHOUT WARRANTY OF   }
{ ANY KIND, either express or implied. See the License for the specific language governing rights  }
{ and limitations under the License.                                                               }
{                                                                                                  }
{ The Initial Developer of the Original Code is Chris Fairall,                                     }
{ but most code is completely rewritten.                                                           }
{                                                                                                  }
{ Contributor(s): André Mussche (andre.mussche at gmail dot com)                                   }
{                 Consultant of: DTS b.v, www.dts.nl                                               }
{ Hired by      : RBK Group, www.rbk.nl                                                            }
{                 Wik Groeneveld (wgroeneveld at rbk dot nl)                                       }
{                                                                                                  }
{ All rights reserved.                                                                             }
{**************************************************************************************************}
unit PlasticEngine;

interface

uses
  JvCreateProcess,
  //JclRegistry,
  Windows, Messages, SysUtils, Classes, Forms, Dialogs, SyncObjs,
  Generics.Collections;

type
  EPlasticError = class(Exception);

  TPlasticStatus    = (psUnknown, psPrivate, psCheckedIn, psCheckedOut, psCheckedInAndLocalChanged);
  TPlasticStatusSet = set of TPlasticStatus;
  TMessageNotify    = procedure(aType: TMsgDlgType; const aMessage: string) of object;

  TPlasticFileInfo = class
    FileName: string;
    Status: TPlasticStatus;
  end;
  TFileCacheItem = class
    FileName: string;
    Status: TPlasticStatus;
    LastUpdate: TDatetime;
  end;

  TPlasticCommThread = class(TThread)
  private
    FCMExe: string;
    FCM : TJvCreateProcess;
    FLastError: string;
    procedure StartPlasticShell;
    procedure JvCPSReadEvent(Sender: TObject; const S: string; const StartsOnNewLine: Boolean);

    var FCommandBusy: boolean;
        FCommandResults: TStrings;
    function  InternalExcuteCommand(const aCommand: string; out aResultLines: TStrings): Boolean;

    var FNewCommandEvent, FCommandDoneEvent: TEvent;
        FCurrentCommand: string;
        FCurrentResult: boolean;
        FCurrentResultLines: TStrings;
  protected
    procedure Execute;override;
  public
    constructor Create(const aCM:TFileName);
    destructor  Destroy;override;

    procedure Terminate;
    function  ExcuteCommand(const aCommand: string; out aResultLines: TStrings): Boolean;

    property LastError: string read FLastError;
  end;

  TPlasticEngine = class(TObject)
  private
    FLastCmd,
    FLastOutput            : String;
    FServerUp              : Boolean;
    function GetIsServerUp : Boolean;
  protected
    FMainPath: string;
    FPlasticCommThread: TPlasticCommThread;
    procedure StartPlasticThread;
    function  InternalExcuteCommand(const aCommand: string; out aResultLines: TStrings): Boolean;

    var FFileCache: TObjectDictionary<string,TFileCacheItem>;
    procedure InternalClearStatusCache;
    function  InternalGetFileInfo(const aFileName : String): TFileCacheItem;

    class procedure HandleException(E:Exception);
    class procedure LogMessage(aType: TMsgDlgType; const aMessage: string);

    class function  GetPlasticExePath: string;
    class function  ExcuteCommand(const aCommand: string; out aResultLines: TStrings): Boolean;

    class procedure FilterControlledFiles(var aFileList: TStrings; aAllowedStatus: TPlasticStatusSet);
  public
    constructor Create; virtual;
    destructor Destroy; override;

    property  IsServerUp : Boolean read GetIsServerUp;

    class procedure ClearStatusCache;

    class function GetLastError: string;

    class function GetFileInfo(const aFileName : String): TPlasticFileInfo;
    class function FileArchived(const FileName : String) : Boolean;
    class procedure ShowVisualClient;

    class function UpdateFiles(FileList : TStrings; const Force : Boolean) : boolean;
    class function UpdateFile(const aFileName : String; const Force : Boolean) : boolean;

    class function Diff(FileList: TStrings): boolean;
    class function DiffFile(const aFileName: String): boolean;

    class function AddFiles(FileList : TStrings): boolean; overload;
    class function AddFile(aFileName: String): boolean; overload;
    class function AddFile(aFileName: String; aSearchCorrespondingFiles: boolean): boolean; overload;

    class function CheckoutFiles(FileList : TStrings): boolean; overload;
    class function CheckoutFile(aFileName: String): boolean; overload;
    class function CheckoutFile(aFileName: String; aSearchCorrespondingFiles: boolean): boolean; overload;

    class function ShelveFiles(FileList : TStrings): boolean; overload;
    class function ShelveFile(aFileName: String): boolean; overload;
    class function ShelveFile(aFileName: String; aSearchCorrespondingFiles: boolean): boolean; overload;

    class function DeleteFiles(FileList : TStrings): boolean; overload;
    class function DeleteFile(aFileName: String): boolean; overload;
    class function RenameFile(aFileName, aNewFile: String): boolean; overload;

    class function UndoCheckoutFiles(FileList : TStrings) : boolean;
    class function UndoCheckoutFile (aFileName: String)   : boolean;

    class function CheckinFiles(FileList : TStrings): boolean; overload;
    class function CheckinFile (aFileName: String)  : boolean; overload;
  end;

const
  C_PlasticStatus: array[TPlasticStatus] of string = ('Unknown', 'Private', 'Checked in', 'Checked out', 'Checked in + Modified');

var
  GPlasticEngine : TPlasticEngine;

implementation

uses
  PlasticUtils, DateUtils, Registry, PlasticExpert,
  DbugIntf, StrUtils, ShellAPI, IOUtils;

constructor TPlasticEngine.Create;
begin
  inherited Create;
  FLastCmd := '';
  FLastOutput := '';

  FFileCache := TObjectDictionary<string,TFileCacheItem>.Create([doOwnsValues], 10);

  StartPlasticThread;
end;

class function TPlasticEngine.DeleteFile(aFileName: String): boolean;
var
  sDir   : string;
  strData: Tstrings;
  info   : TPlasticFileInfo;
begin
  ClearStatusCache;

  Result := False;
  try
    SendDebugFmtEx(dlObject, 'DeleteFile(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      info := GetFileInfo(aFileName);
      try
        if (info <> nil) then
        begin
          SendDebugFmtEx(dlObject, 'Status of file "%s" is %s', [aFileName, C_PlasticStatus[info.Status] ], mtInformation);

          //uncheckout if file is checked out... (cannot delete checked out file)
          if (info.Status = psCheckedOut) then
          begin
            UndoCheckoutFile(aFileName);
            //if we just added a file, and uncheckout, it is not in plastic anymore
            if not FileArchived(aFileName) then
            begin
              //on disk? then delete from disk
              if FileExists(aFileName) then
                Result := SysUtils.DeleteFile(aFileName)
              else
                Result := True;
              //quit, do nothing in plastic
              Exit;
            end;

          end;
        end;
      finally
        info.Free;
      end;

      //eerst dir uitchecken!
      sDir := ExtractFileDir(aFileName);   //without trailing \!
      info := GetFileInfo(sDir);
      try
        if (info <> nil) then
        begin
          SendDebugFmtEx(dlObject, 'Status of directory "%s" is %s', [sDir, C_PlasticStatus[info.Status] ], mtInformation);

          if info.Status = psPrivate then
            //nothing
          else if (info.Status = psCheckedIn) then
            CheckoutFile(sDir);
        end;
      finally
        info.Free;
      end;

      //remove file
      if ExcuteCommand('remove "' + aFilename + '"', strData) and
         (strData <> nil) then
      begin
        assert(strData.Count >= 1);
//        if pos('was correctly checked out', strData.Text) > 0 then
        Result := True;
      end;

      if Result then
        LogMessage(Dialogs.mtInformation, 'Deleted file: ' + aFileName)
      else
        LogMessage(Dialogs.mtError, 'Delete failed for: ' + aFileName + ' - Error: ' + IfThen(strData <> nil, strData.Text, '<empty>'));

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.DeleteFiles(FileList: TStrings): boolean;
var
  s    : string;
//  info : TPlasticFileInfo;
begin
  //FilterControlledFiles(FileList, [psCheckedIn, psCheckedOut, psCheckedInAndLocalChanged]);
  Result := FileList.Count > 0;
  for s in FileList do
  begin
    //controlled? then delete in Plastic
    if FileArchived(s) then
      Result := Result and
                Self.DeleteFile(s)
    //else delete from disk
    else
      Result := Result and
                SysUtils.DeleteFile(s)
  end;

  if Result then
    SendDebugFmtEx(dlObject, 'Delete succesfull:'#13'%s', [FileList.Text], mtInformation)
  else
    SendDebugFmtEx(dlObject, 'Delete NOT succesfull:'#13'%s', [FileList.Text], mtInformation)
end;

destructor TPlasticEngine.Destroy;
begin
  FPlasticCommThread.Terminate;

  FFileCache.Free;
  FPlasticCommThread.WaitFor;
  FPlasticCommThread.Free;

  inherited Destroy;
end;

class function TPlasticEngine.Diff(FileList: TStrings): boolean;
var s: string;
begin
  FilterControlledFiles(FileList, [psCheckedIn, psCheckedOut, psCheckedInAndLocalChanged]);
  Result := FileList.Count > 0;
  for s in FileList do
    Result := Result and
              DiffFile(s);

  if Result then
    SendDebugFmtEx(dlObject, 'Diff succesfull:'#13'%s', [FileList.Text], mtInformation)
  else
    SendDebugFmtEx(dlObject, 'Diff NOT succesfull:'#13'%s', [FileList.Text], mtInformation)
end;

class function TPlasticEngine.DiffFile(const aFileName: String): boolean;
var
  strData: Tstrings;
//  sLine  : String;
begin
  Result := False;
  try
    SendDebugFmtEx(dlObject, 'DiffFile(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      {
      diff c:\temp\test.txt
      CommandResult 0
      }
      if ExcuteCommand('diff "' + aFilename + '"', strData) and
         (strData <> nil) then
      begin
        Result := True;
      end;

    finally
      FreeAndNil(strData);
    end;

    if Result then
      LogMessage(Dialogs.mtInformation, 'Diffed file: ' + aFileName)
    else
      LogMessage(Dialogs.mtWarning, 'Diff failed for: ' + aFileName)

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.ExcuteCommand(const aCommand: string;
  out aResultLines: TStrings): Boolean;
begin
  assert(GPlasticEngine <> nil);
  Result := GPlasticEngine.InternalExcuteCommand(aCommand, aResultLines);
end;

class procedure TPlasticEngine.LogMessage(aType: Dialogs.TMsgDlgType;
  const aMessage: string);
begin
  PlasticUtils.LogMessageToIDE(aType, aMessage);
end;

class function TPlasticEngine.CheckoutFile(aFileName: String): boolean;
var
  strData: Tstrings;
begin
  ClearStatusCache;

  Result := False;
  try
    SendDebugFmtEx(dlObject, 'OpenForEdit(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      {
      checkout c:\temp
      The selected items are about to be checked out. Please wait ...
      Item c:\temp was correctly checked out
      CommandResult 0
      }
      if ExcuteCommand('checkout "' + aFilename + '"', strData) and
         (strData <> nil) then
      begin
        assert(strData.Count >= 1);

        if pos('was correctly checked out', strData.Text) > 0 then
          Result := True;
      end;

      if Result then
        LogMessage(Dialogs.mtInformation, 'Checked out file: ' + aFileName)
      else
        LogMessage(Dialogs.mtError, 'Check out failed for: ' + aFileName + ' - Error: ' + IfThen(strData <> nil, strData.Text, '<empty>'));

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.GetFileInfo(const aFileName: String): TPlasticFileInfo;
var
  strData: Tstrings;
  sLine  : String;
  fc: TFileCacheItem;
begin
  Result := Nil;
  try
    strData := nil;
    try
      //get from cache
      assert(GPlasticEngine <> nil);
      fc := GPlasticEngine.InternalGetFileInfo(aFileName);

      //not expired? then give cached value
      if (fc.LastUpdate > 0) and
         (MilliSecondsBetween(Now, fc.LastUpdate) < 15 * 1000) then
      begin
        Result := TPlasticFileInfo.Create;
        Result.FileName := aFileName;
        Result.Status   := fc.Status;
        Exit;
      end;

      SendDebugFmtEx(dlObject, 'GetFileInfo(%s)...', [aFileName], mtInformation);

      //getstatus c:\temp\test.txt
      if ExcuteCommand('getstatus "' + aFilename + '"', strData) and
         (strData <> nil) then
      begin
        Result := TPlasticFileInfo.Create;
        Result.FileName := aFileName;

        assert(strData.Count >= 1);
        sLine := strData.Strings[0];
        //SendDebugFmtEx(dlObject, 'GetFileInfo: %s', [aFileName, sLine], mtInformation);
        //The item c:\temp\test.txt is under Plastic control and is checked in.
        //The item c:\temp\test.txt is under Plastic control and is checked out.
        //The item c:\temp\test.txt2 is not on disk.

        if pos('checked in', sLine) > 0 then
        begin
          Result.Status   := psCheckedIn;
          if ((FileGetAttr(aFileName) and faDirectory) = 0) and  //not directory
             not FileIsReadOnly(aFileName)
          then
            Result.Status := psCheckedInAndLocalChanged;
        end
        else if pos('checked out', sLine) > 0 then
          Result.Status   := psCheckedOut
        else if pos('not on disk', sLine) > 0 then
          Result.Status   := psUnknown
        else if pos('is private', sLine) > 0 then
          Result.Status   := psPrivate
        else
          Result.Status   := psUnknown;

        fc.Status := Result.Status;
        fc.LastUpdate := Now;
      end;

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.UpdateFiles(FileList: TStrings; const Force : Boolean) : boolean;
var s: string;
begin
  FilterControlledFiles(FileList, [psCheckedIn, psCheckedOut, psCheckedInAndLocalChanged]);
  Result := FileList.Count > 0;
  for s in FileList do
    Result := Result and
              UpdateFile(s, Force);

  if Result then
    SendDebugFmtEx(dlObject, 'Update succesfull:'#13'%s', [FileList.Text], mtInformation)
  else
    SendDebugFmtEx(dlObject, 'Update NOT succesfull:'#13'%s', [FileList.Text], mtInformation)
end;

class function TPlasticEngine.UpdateFile(const aFileName: String;
  const Force: Boolean): boolean;
var
  strData: Tstrings;
begin
  ClearStatusCache;

  Result := False;
  try
    SendDebugFmtEx(dlObject, 'SyncFile(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      {
      update c:\temp\test.txt
      CommandResult 0
      }

      if Force then
      begin
        if ExcuteCommand('update --forced "' + aFilename + '"', strData) and
           (strData <> nil) then
        begin
          //assert(strData.Count >= 1);
          Result := True;
        end;

        if Result then
          LogMessage(Dialogs.mtInformation, 'Updated (forced) file: ' + aFileName)
        else
          LogMessage(Dialogs.mtError, 'Update (forced) failed for: ' + aFileName)
      end
      else
      begin
        if ExcuteCommand('update "' + aFilename + '"', strData) and
           (strData <> nil) then
        begin
          Result := True;
        end;

        if Result then
          LogMessage(Dialogs.mtInformation, 'Updated file: ' + aFileName)
        else
          LogMessage(Dialogs.mtError, 'Update failed for: ' + aFileName)
      end;

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.FileArchived(const FileName: String): Boolean;
var
//  slData   : TStringList;
  info     : TPlasticFileInfo;
begin
//  slData := TStringList.Create;
//  try
    info := GetFileInfo(FileName);
    Result := (info <> nil) and
              (info.Status in [psCheckedIn, psCheckedOut, psCheckedInAndLocalChanged]);
//  finally
//    slData.Free;
//  end;
end;

class procedure TPlasticEngine.FilterControlledFiles(var aFileList: TStrings; aAllowedStatus: TPlasticStatusSet);
var
  I: Integer;
  sFile: string;
  info     : TPlasticFileInfo;
begin
  if (aFileList = nil) or (aFileList.Count = 0) then Exit;

  i := 0;
  repeat
    sFile := aFileList[i];
    info  := GetFileInfo(sFile);
    if (info = nil) or
       not (info.Status in aAllowedStatus) //[psCheckedIn, psCheckedOut, psCheckedInAndLocalChanged]);
    then
      aFileList.Delete(i)
    else
      inc(i);
  until i >= aFileList.Count;
end;

class function TPlasticEngine.AddFile(aFileName: String;
  aSearchCorrespondingFiles: boolean): boolean;
var
  slFileList: TStringList;
begin
  slFileList := TStringList.Create;
  try
    SearchCorrespondingFiles(aFileName, slFileList);
    Result := AddFiles(slFileList);
  finally
    slFileList.Free;
  end;
end;

class function TPlasticEngine.AddFiles(FileList: TStrings): boolean;
var s: string;
begin
  assert(FileList <> nil);
  FilterControlledFiles(FileList, [psPrivate]);
  Result := FileList.Count > 0;
  for s in FileList do
    Result := Result and
              AddFile(s);

  if Result then
    SendDebugFmtEx(dlObject, 'AddArchives succesfull:'#13'%s', [FileList.Text], mtInformation)
  else
    SendDebugFmtEx(dlObject, 'AddArchives NOT succesfull:'#13'%s', [FileList.Text], mtInformation)
end;

class function TPlasticEngine.CheckoutFile(aFileName: String;
  aSearchCorrespondingFiles: boolean): boolean;
var
  slFileList: TStringList;
begin
  slFileList := TStringList.Create;
  try
    SearchCorrespondingFiles(aFileName, slFileList);
    Result := CheckoutFiles(slFileList);
  finally
    slFileList.Free;
  end;
end;

class function TPlasticEngine.CheckoutFiles(FileList: TStrings): boolean;
var s: string;
begin
  FilterControlledFiles(FileList, [psCheckedIn, psCheckedInAndLocalChanged]);
  Result := FileList.Count > 0;
  for s in FileList do
    Result := Result and
              CheckoutFile(s);

  if Result then
    SendDebugFmtEx(dlObject, 'OpenForEdit succesfull:'#13'%s', [FileList.Text], mtInformation)
  else
    SendDebugFmtEx(dlObject, 'OpenForEdit NOT succesfull:'#13'%s', [FileList.Text], mtInformation)
end;

class procedure TPlasticEngine.ClearStatusCache;
begin
  assert(GPlasticEngine <> nil);
  GPlasticEngine.InternalClearStatusCache;
end;

class function TPlasticEngine.RenameFile(aFileName, aNewFile: String): boolean;
var
  strData: Tstrings;
begin
  Result := False;
  try
    SendDebugFmtEx(dlObject, 'RenameFile(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      if ExcuteCommand('move "' + aFilename + '" "' + aNewFile + '"', strData) and
         (strData <> nil) then
      begin
        assert(strData.Count >= 1);
//        if pos('was correctly checked out', strData.Text) > 0 then
        Result := True;
      end;

      if Result then
        LogMessage(Dialogs.mtInformation, 'Renamed file: ' + aFileName)
      else
        LogMessage(Dialogs.mtError, 'Rename failed for: ' + aFileName + ' - Error: ' + IfThen(strData <> nil, strData.Text, '<empty>'));

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.UndoCheckoutFile(aFileName: String): boolean;
var
  strData: Tstrings;
begin
  ClearStatusCache;

  Result := False;
  try
    SendDebugFmtEx(dlObject, 'RevertFile(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      {
      undocheckout c:\temp\test.txt
      c:\temp\test.txt unchecked out correctly
      CommandResult 0
      }

      if ExcuteCommand('undocheckout "' + aFilename + '"', strData) and
         (strData <> nil) then
      begin
        assert(strData.Count >= 1);

        if pos('unchecked out correctly', strData.Text) > 0 then
          Result := True;
      end;

      if Result then
        LogMessage(Dialogs.mtInformation, 'Undo checked out file: ' + aFileName)
      else
        LogMessage(Dialogs.mtError, 'Undo check out failed for: ' + aFileName + ' - Error: ' + IfThen(strData <> nil, strData.Text, '<empty>'));

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.UndoCheckoutFiles(FileList: TStrings): boolean;
var s: string;
begin
  FilterControlledFiles(FileList, [psCheckedOut]);
  Result := FileList.Count > 0;
  for s in FileList do
    Result := Result and
              UndoCheckoutFile(s);

  if Result then
    SendDebugFmtEx(dlObject, 'RevertFiles succesfull:'#13'%s', [FileList.Text], mtInformation)
  else
    SendDebugFmtEx(dlObject, 'RevertFiles NOT succesfull:'#13'%s', [FileList.Text], mtInformation)
end;

class function TPlasticEngine.AddFile(aFileName: String): boolean;
var
  sDir: string;
  strData: Tstrings;
  info   : TPlasticFileInfo;
begin
  ClearStatusCache;

  Result := False;
  try
    SendDebugFmtEx(dlObject, 'AddArchive(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      {
      add c:\temp\test.txt
      The selected items are about to be added. Please wait ...
      Item c:\temp\test.txt was correctly added
      CommandResult 0
      }

      //eerst dir uitchecken!
      sDir := ExtractFileDir(aFileName);   //without trailing \!
      info := GetFileInfo(sDir);
      try
        if (info <> nil) then
        begin
          SendDebugFmtEx(dlObject, 'Status of directory "%s" is %s', [sDir, C_PlasticStatus[info.Status] ], mtInformation);

          if info.Status = psPrivate then
            AddFile(sDir)
          else if (info.Status = psCheckedIn) then
            CheckoutFile(sDir);
        end;
      finally
        info.Free;
      end;

      if ExcuteCommand('add "' + aFilename + '"', strData) and
         (strData <> nil) then
      begin
        assert(strData.Count >= 1);

        if pos('was correctly added', strData.Text) > 0 then
          Result := True;
      end;

      if Result then
        LogMessage(Dialogs.mtInformation, 'Added file: ' + aFileName)
      else
        LogMessage(Dialogs.mtError, 'Add failed for: ' + aFileName + ' - Error: ' + IfThen(strData <> nil, strData.Text, '<empty>'));

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.CheckinFile(aFileName: String): boolean;
var
  strData: Tstrings;
begin
  ClearStatusCache;

  Result := False;
  try
    SendDebugFmtEx(dlObject, 'CheckinFile(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      {
      checkin c:\temp\test.txt
      The selected items are about to be checked in. Please wait ...
      Checking in c:\temp\test.txt ... Done
      Created changeset
      cs:5365@br:/main/test@rep:FobisPS41@repserver:test:8084
      CommandResult 0
      }

      if ExcuteCommand('checkin "' + aFilename + '"', strData) and
         (strData <> nil) then
      begin
        assert(strData.Count >= 1);

        if pos('Done', strData.Text) > 0 then
          Result := True;
      end;

      if Result then
        LogMessage(Dialogs.mtInformation, 'Checked in file: ' + aFileName)
      else
        LogMessage(Dialogs.mtError, 'Check in failed for: ' + aFileName + ' - Error: ' + IfThen(strData <> nil, strData.Text, '<empty>'));

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.CheckinFiles(FileList: TStrings): boolean;
var s: string;
begin
  FilterControlledFiles(FileList, [psCheckedOut, psCheckedInAndLocalChanged]);
  Result := FileList.Count > 0;
  for s in FileList do
    Result := Result and
              CheckinFile(s);

  if Result then
    SendDebugFmtEx(dlObject, 'CheckinFiles succesfull:'#13'%s', [FileList.Text], mtInformation)
  else
    SendDebugFmtEx(dlObject, 'CheckinFiles NOT succesfull:'#13'%s', [FileList.Text], mtInformation)
end;

function TPlasticEngine.GetIsServerUp: Boolean;
begin
  FServerUp := (FPlasticCommThread <> nil) and
               (FPlasticCommThread.FCM <> nil) and
               (FPlasticCommThread.FCM.State in [psRunning, psWaiting]);
  Result := FServerUp;
end;

class function TPlasticEngine.GetLastError: string;
begin
  assert(GPlasticEngine <> nil);
  assert(GPlasticEngine.FPlasticCommThread <> nil);
  Result := GPlasticEngine.FPlasticCommThread.LastError;
end;

class function TPlasticEngine.GetPlasticExePath: string;
const
  C_RegKey   = 'SOFTWARE\\Codice Software S.L.\\Codice Software PlasticSCM professional\\';
  C_RegValue = 'Location';
  C_REG_MACHINE_LOCATION = 'System\\CurrentControlSet\\Control\\Session Manager\\Environment\\';
var
  r : TRegistry;
  //str: tstrings;
  s, sPath: string;
begin
  Result := '';
  r := TRegistry.create(KEY_READ);
  try
    r.Rootkey := HKEY_LOCAL_MACHINE;
    if r.OpenKey(C_RegKey, False) then
    begin
      Result := r.ReadString(C_RegValue);
      r.CloseKey;
    end;
    r.CloseKey;
  finally
    r.Free;
  end;

  //no registry value found? search via environment path
  if Result = '' then
  begin
    //first search from via environment path (user)
    s := FileSearch('cm.exe', GetEnvironmentVariable('path'));
    SendDebugEx(dlObject, Format('Path search: "%s" - Environment: ' + GetEnvironmentVariable('path'), [s]), mtInformation);

    //else search via system environment path
    if s = '' then
    begin
      sPath := '';
      //get system path
      r := TRegistry.create(KEY_READ);
      try
        r.Rootkey := HKEY_LOCAL_MACHINE;
        if r.OpenKey(C_REG_MACHINE_LOCATION, False) then
        begin
          sPath := r.ReadString('Path');
          if sPath = '' then
            sPath := r.ReadString('path');
          if sPath = '' then
            sPath := r.ReadString('PATH');
          r.CloseKey;
        end;
        r.CloseKey;
      finally
        r.Free;
      end;

      //search
      s := FileSearch('cm.exe', sPath);
      SendDebugEx(dlObject, Format('Path search: "%s" - System Environment: %s', [s, sPath]), mtInformation);
    end;

    //strip client dir
    if s <> '' then
    begin
      s := ExtractFileDir(s);  //C:\Program Files\PlasticSCM\Client\cm.exe' -> C:\Program Files\PlasticSCM\Client'
      s := ExtractFileDir(s);  //C:\Program Files\PlasticSCM\Client' -> C:\Program Files\PlasticSCM'
      Result := s;
    end;
  end;

  //Result := RegReadStringDef(HKLM, RegKey, RegValue, 'C:\Program Files\PlasticSCM');
end;

class procedure TPlasticEngine.HandleException(E: Exception);
var
  sError: string;
begin
  if E = nil then Exit;
  sError := Format('%s: %s'#13'Stack:'#13'%s',
                   [E.ClassName, e.Message, GetLastStackAsString]);
  SendDebugEx(dlObject, sError, mtError);

  MessageDlg(sError, Dialogs.mtError, [mbOK], 0);
end;

procedure TPlasticEngine.InternalClearStatusCache;
begin
  //reset cache
  System.TMonitor.Enter(FFileCache);
  try
    FFileCache.Clear;
  finally
    System.TMonitor.Exit(FFileCache);
  end;
end;

function TPlasticEngine.InternalExcuteCommand(const aCommand: string;
  out aResultLines: TStrings): Boolean;
begin
  Result := False;
  if FPlasticCommThread = nil then Exit;

  try
    assert(FPlasticCommThread <> nil);
    //threadsafe (locked) execute command and wait for result...
    Result := FPlasticCommThread.ExcuteCommand(aCommand, aResultLines);
  except
    on e:exception do
    begin
      HandleException(e);
      //oude thread weg
      FPlasticCommThread.FreeOnTerminate := True;
      FPlasticCommThread.Terminate;
      FPlasticCommThread := nil;
      //nieuwe aanmaken
      StartPlasticThread;
    end;
  end;
end;

function TPlasticEngine.InternalGetFileInfo(
  const aFileName: String): TFileCacheItem;
begin
  Result := nil;
  System.TMonitor.Enter(FFileCache);
  try
    FFileCache.TryGetValue(aFileName, Result);
    if Result = nil then
    begin
      Result := TFileCacheItem.Create;
      Result.FileName := aFileName;
      FFileCache.Add(aFileName, Result);
    end;
  finally
    System.TMonitor.Exit(FFileCache);
  end;
end;

class function TPlasticEngine.ShelveFile(aFileName: String): boolean;
var
  strData: Tstrings;
begin
  Result := False;
  try
    SendDebugFmtEx(dlObject, 'ShelveFile(%s)...', [aFileName], mtInformation);

    strData := nil;
    try
      if ExcuteCommand('shelve "' + aFilename + '"', strData) and
         (strData <> nil) then
      begin
        assert(strData.Count >= 1);
//        if pos('was correctly checked out', strData.Text) > 0 then
        Result := True;
      end;

      if Result then
        LogMessage(Dialogs.mtInformation, 'Shelved file: ' + aFileName)
      else
        LogMessage(Dialogs.mtError, 'Shelve failed for: ' + aFileName + ' - Error: ' + IfThen(strData <> nil, strData.Text, '<empty>'));

    finally
      FreeAndNil(strData);
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

class function TPlasticEngine.ShelveFile(aFileName: String;
  aSearchCorrespondingFiles: boolean): boolean;
var
  slFileList: TStringList;
begin
  slFileList := TStringList.Create;
  try
    SearchCorrespondingFiles(aFileName, slFileList);
    Result := ShelveFiles(slFileList);
  finally
    slFileList.Free;
  end;
end;

class function TPlasticEngine.ShelveFiles(FileList: TStrings): boolean;
var s: string;
begin
  FilterControlledFiles(FileList, [psCheckedOut]);
  Result := FileList.Count > 0;
  for s in FileList do
    Result := Result and
              ShelveFile(s);

  if Result then
    SendDebugFmtEx(dlObject, 'Shelve succesfull:'#13'%s', [FileList.Text], mtInformation)
  else
    SendDebugFmtEx(dlObject, 'Shelve NOT succesfull:'#13'%s', [FileList.Text], mtInformation)
end;

class procedure TPlasticEngine.ShowVisualClient;
var
  sClient: string;
begin
  sClient := GetPlasticExePath + '\client\plastic.exe';
  ShellExecute(Application.Handle, 'open', pchar(sClient), nil, nil, SW_SHOWNORMAL) ;
end;

procedure TPlasticEngine.StartPlasticThread;
var
  sPath, sCM: string;
begin
  sPath := GetPlasticExePath;
  SendDebugFmt(dlObject, 'Plastic SCM Path: %s', [sPath]);

  //if sPath <> '' then
  begin
    sCM := sPath + '\client\cm.exe';
    if not FileExists(sCM) then
    begin
      SendDebugEx(dlObject, Format('Plastic SCM cm.exe not found (%s)', [sCM]), mtError);
      Exit;
    end;
  end;

  FMainPath := sPath;
  //create thread
  FPlasticCommThread := TPlasticCommThread.Create(sCM);
end;

{ TPlasticCommThread }

constructor TPlasticCommThread.Create(const aCM: TFileName);
begin
  FCMExe := aCM;
  FNewCommandEvent  := TEvent.Create;
  FCommandDoneEvent := TEvent.Create;
  inherited Create(False);
end;

function TPlasticCommThread.InternalExcuteCommand(const aCommand: string;
  out aResultLines: TStrings): Boolean;
var
  iCommandResult: Integer;
  i: Integer;
  s, sPrev: String;
begin
  SendDebugFmtEx_Start(dlObject, '-> ExcuteCommand(%s)', [aCommand], mtInformation);
  try
    Result := False;
    aResultLines    := nil;
    FCommandBusy    := True;
    FCommandResults := TStringlist.Create;

    FLastError := '';  //clear error before new command
    //send to plastic
    FCM.WriteLn(AnsiString(aCommand));
    //e.g.: getstatus c:\temp\test.txt

    //wait till all results are received (by "JvCPSReadEvent" procedure)
    while FCommandBusy do
    begin
      Application.ProcessMessages;
      Sleep(10);
    end;
    {e.g.:
    c:\temp\test.txt is under Plastic control and is checked in.
    CommandResult 0}

    sPrev := '';
    //check CommandResult
    for i := 0 to FCommandResults.Count - 1 do
    begin
      s := FCommandResults.Strings[i];
      if Pos('CommandResult', s) = 1 then
      begin
        iCommandResult := StrToIntDef( Trim(Copy(s, Length('CommandResult')+1, Length(s))), -1);
        Result         := (iCommandResult = 0);
        SendDebugFmtEx(dlObject, 'CommandResult = %d, Result = %s', [iCommandResult, BoolToStr(Result, True)], mtInformation);

        //last error
        if not Result then
          FLastError := sPrev;

        Break;
      end;

      if trim(s) <> '' then
        sPrev := s;
    end;

    aResultLines := FCommandResults;
  finally
    SendDebugFmtEx_End(dlObject, '<- ExcuteCommand(%s) finished', [aCommand], mtInformation);
  end;
end;

destructor TPlasticCommThread.Destroy;
begin
  SendDebugEx(dlObject, 'Destroying Plastic thread', mtInformation);
  FNewCommandEvent.Free;
  FCommandDoneEvent.Free;
  FreeAndNil(FCM);
  inherited;
end;

function TPlasticCommThread.ExcuteCommand(const aCommand: string;
  out aResultLines: TStrings): Boolean;
var
  iTimeout: integer;
begin
  System.TMonitor.Enter(Self);
  try
    assert(FCurrentCommand = '');

    Result              := False;
    aResultLines        := nil;
    FCurrentCommand     := aCommand;
    FCurrentResultLines := nil;
    FCommandDoneEvent.ResetEvent;
    FNewCommandEvent.SetEvent;
    try
      if StartsText('diff', aCommand) then
        iTimeout := INFINITE   //wait till user closes diff viewer
      else
        iTimeout := 30 * 1000; //max 30s

      //wait till ready
      if FCommandDoneEvent.WaitFor(iTimeout) = wrSignaled then
      begin
        //get result
        aResultLines := FCurrentResultLines;
        Result       := FCurrentResult;
      end
      else
        //Abort;   //dirty, but used to create a new thread (should never happen)
        raise EPlasticError.CreateFmt('Plastic "cm.exe shell" did not respond within %d seconds',[iTimeout div 1000]);
    finally
      //reset
      FCurrentResultLines  := nil;
      FCurrentCommand      := '';
    end;
  finally
    System.TMonitor.Exit(Self);
  end;
end;

procedure TPlasticCommThread.Execute;
begin
  SendDebugEx(dlObject, 'Starting TPlasticCommThread', mtInformation);
  Application.ProcessMessages;
  SendDebugEx(dlObject, 'Starting Plastic CM.exe', mtInformation);
  StartPlasticShell;

  SendDebugEx(dlObject, 'Waiting for commands :-)', mtInformation);
  while not Terminated do
  begin
    try
      if FNewCommandEvent.WaitFor(1 * 1000) = wrSignaled then
      begin
        FNewCommandEvent.ResetEvent;
        if Terminated then Break;

        //execute
        FCurrentResult := InternalExcuteCommand(FCurrentCommand, FCurrentResultLines);

        //done
        FCommandDoneEvent.SetEvent;
        FCurrentCommand := '';
      end;
    except
      on E:Exception do HandleException(E);
    end;
  end;

  SendDebugEx(dlObject, 'Closing Plastic thread', mtInformation);
  FCM.CloseApplication(True);
end;

procedure TPlasticCommThread.JvCPSReadEvent(Sender: TObject; const S: string;
  const StartsOnNewLine: Boolean);
begin
  try

    SendDebugFmtEx(dlObject, 'JvCPSReadEvent: %s', [s], mtInformation);
    if FCommandBusy then
    begin
      Assert(FCommandResults <> nil);
      FCommandResults.Add(s);

      if Pos('CommandResult', s) = 1 then     //CommandResult 0
      begin
        //SendDebugFmtEx(dlObject, 'JvCPSReadEvent: command done', [s], mtInformation);
        FCommandBusy := False;
      end;
    end;

  except
    on E:Exception do HandleException(E);
  end;
end;

procedure TPlasticCommThread.StartPlasticShell;
begin
  FCM := TJvCreateProcess.Create(nil);
  FCM.ApplicationName  := FCmExe;
  FCM.CommandLine      := 'cm.exe shell';
  FCM.WaitForTerminate := True;

  FCM.StartupInfo.ShowWindow         := swHide;
  FCM.StartupInfo.DefaultSize        := False;
  FCM.StartupInfo.DefaultWindowState := False;

  FCM.OnRead           := JvCPSReadEvent;
  FCM.ConsoleOptions   := [coRedirect];
  try
    FCM.Run;
  except
    on e:exception do
      SendDebugEx(dlObject, Format('%s: %s' +
                                   'Stack: ' + #13 + '%s',
                                   [e.classname, e.Message, GetLastStackAsString]),
                  mtError);
  end;
end;

procedure TPlasticCommThread.Terminate;
begin
  SendDebugEx(dlObject, 'Terminating Plastic thread...', mtInformation);
  inherited Terminate;
  FNewCommandEvent.SetEvent;
end;

initialization
  GPlasticEngine := TPlasticEngine.Create;

finalization
  try
    FreeAndNil(GPlasticEngine);
  except
    on E:Exception do HandleException(E);
  end;

{
cm shell
help

These are PlasticSCM available commands:

Command                      ShortName      Description
---------------------------------------------------------------------------
activateuser                 au             Activate a previously deactivated licensed user.
add                          add            Add an item in current workspace.
addrepository                addrep         Add an existing repository database to the system.
annotate                     ann            Lists the content of a file or directory. Each line is annotated with the ow
ner and revision.
changepassword               cp             Change current user password, when security is based on user/password
changerevisiontype           crt            Changes all revision type of an item.
changetrigger                chtr           It changes the information associated to an existing trigger
changeworkspace              cwk            Change the location of a workspace.
checkconnection              cc             Check Plastic SCM client connection against server.
checkdatabase                chkdb          Check databases integrity.
checkin                      ci             Create a new revision of a changed item.
checkout                     co             Checkout an item (file or folder) in order to modify it.
deactivateuser               du             Deactivate a previously activated licensed user.
descbranchbase               dbb            Describe the starting points of a smart branch at a certain point in time
differences                  diff           Show differences between two revisions.
diffmetrics                  dm             Show difference metrics between two revisions.
dir                          ls             Get a list of items
find                         find           Get a list of objects following search criteria.
findbranchcontent            fbc            Display items in specified branch.
findchanged                  fc             Get a list of changed files in workspace.
findcheckouts                fco            Get a list of checked out items.
findprivate                  fp             Get a list of private items in workspace.
getconfig                    gc             Obtains the requested configuration info.
getfile                      cat            Get the specified revision.
getrevision                  gr             Get the specified revision of an item.
getstatus                    gs             Get the status of an item.
getworkspacefrompath         gwp            Get workspace information from a path
help                         help           Show help about a command
history                      hst            Display revision history of specified item.
label                        lb             Apply a label to a given revision.
licenseinfo                  li             Display information about Plastic SCM server licensing.
listrepositories             lrep           Show the repository list of the specified server.
listtriggers                 ltr            It shows information related to a certain type of triggers
listusers                    lu             Lists users and groups that the server knows.
listworkspaces               lwk            Show a list of registered workspaces.
location                     location       Shows the absolute path where the Plastic SCM client is installed
log                          l              Returns information related with the revisions created in the specified chan
geset in the changeset branch or within the specified interval of changesets.
makeattribute                mkatt          Create new a attribute.
makebranch                   mkbr           Create a new branch in current repository.
makelabel                    mklb           Create new a label.
makerepository               mkrep          Create a repository in a given server.
maketrigger                  mktr           It creates a new trigger on a certain server
makeworkspace                mkwk           Create a new workspace.
merge                        m              Merge contents of current branch, the content of specified branch.
move                         mv             Move or rename an item.
obliterate                   obl            Destroys permanently an item of the system.
query                        q              Execute SQL queries against server database
remove                       rm             Unbind an item form version control.
removeattribute              rmatt          Remove one or more attributes.
removeattributerealization   rmattr         Remove an atrribute that has previously been set into an object
removebranch                 rmbr           Remove one or more branches.
removelabel                  rmlb           Remove one or more labels.
removerepository             rmrep          Remove a repository from the repository server.
removetrigger                rmtr           It deletes a trigger identified by a type and a position on a certain server

removeworkspace              rmwk           Remove a workspace.
renameattribute              rnatt          Rename an attribute.
renamebranch                 rnbr           Rename a branch.
renamelabel                  rnlb           Rename a label.
renamerepository             rnrep          Rename a repository.
renameworkspace              rnwk           Rename a workspace.
replicate                    rp             Replicates data from a remote repository
revert                       rv             Creates a new revision of an item with the content of the specified revision
.
setacl                       acl            Set permissions for a user to specified object.
setattribute                 statt          Set an attribute to a given object.
setbranchbase                sbb            Set a specific base point (a changeset or a label) to a smart branch
setowner                     sto            Set the owner of an object.
setselector                  sts            Set a selector to a workspace.
shelvefile                   shelve         Shelves the content of a file revision
showacl                      sa             Show the ACL (Access Control List) of a object.
showcommands                 showcommands   Show every available command
showfindobjects              sfo            Display available objects and attributes list.
showowner                    so             Show the owner of a object.
showpermissions              sp             Display available permission list
showselector                 ss             Show current workspace selector.
showtriggertypes             stt            Display available trigger types.
subtractivemerge             sm             It allows deleting changes introduced by a checkin or a merge.
switchtobranch               stb            Set specified branch as working branch
tree                         tree           Show a revision tree for a given item.
uncounchanged                unuc           Undo checkout on items not changed.
undochange                   unc            Undo the changes on an item.
undocheckout                 unco           Undo the checkout on an item.
update                       upd            Update items in the workspace.
version                      version        Show current client version number
whoami                       id             Shows the current user in Plastic SCM system
workspaceinfo                wi             Show information about workspace selector
workspacestatus              status         Notify the loaded changesets on a workspace

* Executing a command:
      cm command_name

* Get usage of a command:
      cm command_name --usage
      cm command_name -?

* Get help from a command
      cm help command_name
      cm command_name --help

CommandResult 0
}

end.
