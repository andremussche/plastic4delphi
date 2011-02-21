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
unit PlasticUtils;

interface

uses
  Dialogs,
  SysUtils, Windows, Classes, Forms, ShellAPI;

const
  C_WhiteListedExt : array[0..9] of string = ('.pas', '.dfm', '.todo', '.dpr', '.res', '.dpk',
                                              '.rc', '.inc',
                                              '.dres', 'Resource.rc' {Project1.dpr -> Project1Resource.rc -> Project1.dres: contains extra resources}
                                              );
  C_BlackListedExt : array[0..4] of string = ('.ddp', '.dof', '.cfg', '.dproj', '.bdsproj');
  C_ProjectFilesExt: array[0..6] of string = ('.todo', '.dpr', '.res', '.dpk', '.dproj',
                                              '.dres', 'Resource.rc' {Project1.dpr -> Project1Resource.rc -> Project1.dres: contains extra resources}
                                              );

  procedure LogMessageToIDE(aMessageType: TMsgDlgType; const aMessageStr: string;
      const aFileName: string = ''; const aPrefixStr: string = '');

  procedure HandleException(E: Exception);
  function  GetLastStackAsString: string;

  procedure SearchCorrespondingFiles(const aFilename: string; aFileList: TStrings);

  function IsWhiteListedFile(const aFilename: string): boolean;
  function IsBlackListedFile(const aFilename: string): boolean;

implementation

uses
  Registry, DbugIntf,
  {$IFDEF DEBUG}
  JclDebug,
  {$ENDIF}
  PlasticEngine,
  Generics.Collections, PlasticExpert,
  ToolsAPI;

procedure LogMessageToIDE(aMessageType: TMsgDlgType; const aMessageStr: string;
      const aFileName: string = ''; const aPrefixStr: string = '');
var
  lGroup: IOTAMessageGroup;
  lDummyLineRef: pointer;
  lPrefix: string;
begin
  //must be execute in mainthread
  TThread.Queue(nil,
    procedure
    begin
      with BorlandIDEServices as IOTAMessageServices60 do
      begin
        lGroup := GetGroup('Plastic');
        if not Assigned(lGroup) then lGroup := AddMessageGroup('Plastic');

        lPrefix := aPrefixStr;
        if lPrefix = '' then lPrefix := 'Note';
        AddToolMessage(aFilename, aMessageStr, lPrefix, -1, -1, nil, lDummyLineRef, lGroup);
      end; { with }
    end );
end;

function GetLastStackAsString: string;
{$IFDEF DEBUG}
var
  lStack: TJclStackInfoList;
  str   : TStrings;
begin
  try
    lStack := jclDebug.JclLastExceptStackList;
    str    := TStringList.create;
    try
      if (lstack = nil) or (lstack.Count < 7) then
      //otherwise try raw dump (with some false positives)
      begin
        lstack.Free;
        lStack := TJclStackInfoList.Create(True, 0, nil);
      end;

      lstack.AddToStrings(str, True, False, True);
      Result := str.Text;
    finally
      str.Free;
      lStack.Free;
    end;
  except
    result := 'error';
  end;
end;
{$ELSE}
begin
  Result := '(stacktrace only in debug version)'
end;
{$ENDIF}

procedure HandleException(E: Exception);
var
  sError: string;
begin
  if E = nil then Exit;
  sError := Format('TPlasticCommThread: %s: %s'#13'Stack:'#13'%s',
                   [E.ClassName, e.Message, GetLastStackAsString]);
  SendDebugEx(dlObject, sError, mtError);

  //log to IDE
  LogMessageToIDE(mtError, sError);
end;

function IsWhiteListedFile(const aFilename: string): boolean;
var
  s, sExt: string;
//  i: integer;
begin
  Result    := False;
  sExt      := LowerCase(ExtractFileExt(aFilename));
  //sResource := LowerCase(ChangeFileExt(aFilename, 'Resource.rc'));  //FobisPM.dpr -> FobisPMResource.rc
  //SendDebugFmt(dlObject, 'Extension: %s',[sExt]);
  for s in C_WhiteListedExt do
  begin
    if (s = sExt) then
    begin
      Result := True;
      Break;
    end
  end;

  //does not work?:
  //Result := TArray.BinarySearch<string>(C_WhiteListedExt, sExt, i);
end;

function IsBlackListedFile(const aFilename: string): boolean;
var
  s, sExt: string;
//  i: integer;
begin
  Result := False;

  //todo: use the same plastic's ignore.conf!?
  //cm getccnfig location
  //C:\Users\<user>\AppData\Local\plastic\client.conf
  sExt := LowerCase(ExtractFileExt(aFilename));

  for s in C_BlackListedExt do
  begin
    if s = sExt then
    begin
      Result := True;
      Break;
    end;
  end;

  //does not work?:
  //Result := TArray.BinarySearch<string>(C_BlackListedExt, sExt, i);
end;

procedure SearchCorrespondingFiles(const aFilename: string; aFileList: TStrings);
var
  s,
  sFilename: string;
begin
  sFilename := aFilename;
  if (aFileList.IndexOf(sFilename) < 0) and
     FileExists(sFilename)
  then
    aFileList.Add(sFilename);

  for s in C_WhiteListedExt do
  begin
    sFilename := ChangeFileExt(sFilename, s);

    if FileExists(sFilename) then
      SendDebugFmt(dlObject, 'SearchCorrespondingFiles: found %s',[sFilename]);

    if (aFileList.IndexOf(sFilename) < 0) and
       FileExists(sFilename)
    then
      aFileList.Add(sFilename);
  end;
end;

end.
