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
unit PlasticNotify;

interface

uses
  Windows, SysUtils, Controls, Graphics, Classes, Menus, ActnList, ToolsAPI,
  Dialogs, Forms, Generics.Collections;

type
  TFileNotifier = class;

  TIDENotifier = class(TInterfacedObject, IOTAIDENotifier)
  private
    FNotifyIndex: integer;
    FFileWatches: TDictionary<string, TFileNotifier>;
    procedure AddFileWatch(const aFilename: string);
  protected
    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;

    { IOTAIDENotifier }
    procedure AfterCompile(Succeeded: Boolean);
    procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
    procedure FileNotification(NotifyCode: TOTAFileNotification; const FileName: String; var Cancel: Boolean);
  public
    procedure  AfterConstruction;override;
    destructor Destroy;override;

    procedure  RemoveNotifier;
  end;

  TFileNotifier = class(TInterfacedObject, IOTAModuleNotifier)
  private
    FFileName: string;
    FModified: boolean;
    FBusy: boolean;
    FModule: IOTAModule;
    FNotifyIndex: integer;
  protected
    { IOTAModuleNotifier }
    function  CheckOverwrite: Boolean;
    procedure ModuleRenamed(const NewName: string);

    { IOTANotifier }
    procedure AfterSave;
    procedure BeforeSave;
    procedure Destroyed;
    procedure Modified;
  public
    constructor Create(const aFilename: string; aModule: IOTAModule);

    procedure  RemoveNotifier;
  end;

implementation

uses
  PlasticEngine, DbugIntf, PlasticUtils, PlasticAsync, PlasticExpert;

var
  _IDENotifier: TIDENotifier;

{ TIDENotifier }

procedure TIDENotifier.AddFileWatch(const aFilename: string);
var
  Serv: IOTAModuleServices;
  Module: IOTAModule;
  iIndex, iCounter: integer;
  filewatch: TFileNotifier;
  EditorServices: IOTAEditorServices;
begin
  Serv := (BorlandIDEServices as IOTAModuleServices);

  //search all modules for file
  for iCounter := 0 to Serv.ModuleCount - 1 do
  begin
    if Supports(Serv.Modules[iCounter], IOTAModule, Module) then
    begin
      //module found, now try to remove the readonly flag...
      if LowerCase(Module.FileName) = LowerCase(aFilename) then
      begin
        //try first via edit buffer interface...
        if (Module.CurrentEditor <> nil) and
           Supports(Module.CurrentEditor, IOTAEditBuffer) and
           (Module.CurrentEditor as IOTAEditBuffer).IsReadOnly then
        begin
          (Module.CurrentEditor as IOTAEditBuffer).IsReadOnly := False;
          SendDebugFmt(dlObject, 'Readonly removed from editor of file: %s',[Module.FileName]);
        end
        else
        begin
          EditorServices := (BorlandIDEServices as IOTAEditorServices);
          //...else via topbuffer...
          if (EditorServices.TopBuffer <> nil) and
             (EditorServices.TopBuffer.FileName = aFilename) and
             EditorServices.TopBuffer.IsReadOnly then
          begin
            EditorServices.TopBuffer.IsReadOnly := False;
            SendDebugFmt(dlObject, 'Readonly removed from top editor of file: %s',[EditorServices.TopBuffer.FileName]);
          end
          //...last chance: via file editor
          else
            for iIndex := 0 to Module.ModuleFileCount-1 do
            if Supports(Module.ModuleFileEditors[iIndex], IOTAEditBuffer) then
            begin
              (Module.CurrentEditor as IOTAEditBuffer).IsReadOnly := False;
              SendDebugFmt(dlObject, 'Readonly removed from editor %d of file: %s',[iIndex, Module.FileName]);
            end;
        end;

        //exist already? then remove (to refresh)
        if FFileWatches.TryGetValue(aFilename, filewatch) then
        begin
          FFileWatches.Remove(aFilename);
          filewatch.Destroyed;
        end;

        filewatch := TFileNotifier.Create(aFilename, Module);
        FFileWatches.Add(aFilename, filewatch);
        SendDebugFmt(dlObject, 'FileWatch added for file: %s',[Module.FileName]);

        //found or added, so we're done
        Exit;
      end;
    end;
  end;

  SendDebugFmt(dlObject, 'No module found for file: %s',[Module.FileName]);
end;

procedure TIDENotifier.AfterCompile(Succeeded: Boolean);
begin
  // Do nothing
end;

procedure TIDENotifier.AfterConstruction;
begin
  inherited;
  FFileWatches := TDictionary<string, TFileNotifier>.Create;

  //add to IDE
  with BorlandIDEServices as IOTAServices do
    FNotifyIndex := AddNotifier(Self);
end;

procedure TIDENotifier.AfterSave;
begin
  //
end;

procedure TIDENotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
  // Do nothing
end;

procedure TIDENotifier.BeforeSave;
begin
  //
end;

destructor TIDENotifier.Destroy;
begin
  SendDebug(dlObject, 'TIDENotifier.Destroy');
  RemoveNotifier;
  FFileWatches.Free;
  inherited;
end;

procedure TIDENotifier.Destroyed;
begin
  SendDebug(dlObject, 'TIDENotifier.Destroyed');
  RemoveNotifier;
end;

procedure TIDENotifier.FileNotification(
  NotifyCode: TOTAFileNotification; const FileName: String;
  var Cancel: Boolean);
var
  fi        : TFileNotifier;
begin
  try
    if (NotifyCode = ofnFileOpened) then
    begin
      SendDebugFmt(dlObject, 'FileNotification: file opened: %s',[FileName]);
      //add notification
      AddFileWatch(FileName);
    end
    else if (NotifyCode = ofnFileClosing) then
    begin
      SendDebugFmt(dlObject, 'FileNotification: file closed: %s',[FileName]);
      //remove save notification
      if FFileWatches.TryGetValue(FileName,fi) then
      begin
        FFileWatches.Remove(FileName);
        fi.RemoveNotifier;
      end;
    end;

    GPlasticExpert.UpdateMenu(True);
  except
    on E:Exception do HandleException(E);
  end;
end;

procedure TIDENotifier.Modified;
begin
  //
end;

procedure TIDENotifier.RemoveNotifier;
var
  fi: TFileNotifier;
begin
  SendDebug(dlObject, 'TIDENotifier: removing notifier');
  try
    //remove watches
    for fi in FFileWatches.Values do
      fi.RemoveNotifier;

    //remove self
    if FNotifyIndex >= 0 then
    with BorlandIDEServices as IOTAServices do
      RemoveNotifier(FNotifyIndex);
  except
    on E:Exception do HandleException(E);
  end;
  FNotifyIndex := -1;
end;

{ TFileNotifier }

procedure TFileNotifier.AfterSave;
var info: TPlasticFileInfo;
begin
//  try
  SendDebugFmt(dlObject, 'FileNotifier: file saved: %s',[FFileName]);
  //get status async
  GAsyncThread.ExecuteASync(
    procedure
    begin
      info := TPlasticEngine.GetFileInfo(FFileName);
      try
        if (info <> nil) then
        begin
          //shelve?
          if info.Status in [psCheckedOut] then
            TPlasticEngine.ShelveFile(FFileName, True)
          //add?
          else if (info.Status in [psPrivate]) and
               //not IsBlackListedFile(FFileName) or
               IsWhiteListedFile(FFileName) then
          begin
            //ask question to user...
            TThread.Queue(nil,
              procedure
              begin
                if MessageDlg(Format('Saved private file:'+#13#10+
                                     '%s'+#13+#10+''+#13+#10+
                                     'Do you want to add it to Plastic?',[FFileName]),
                              Dialogs.mtConfirmation, [mbYes, mbNo], 0) = mrYes then
                begin
                  GAsyncThread.ExecuteASync(
                    procedure
                    begin
                      TPlasticEngine.AddFile(FFileName, True);
                    end);
                end;
              end)
          end
          //check out?
          else if info.Status in [psCheckedIn] then
          begin
            //ask question to user...
            TThread.Queue(nil,
              procedure
              begin
                if MessageDlg(Format('File is saved but not checked out:'+#13#10+
                                     '%s'+#13#10+''+#13#10+
                                     'Do you want to check it out now?', [FFileName]),
                              Dialogs.mtConfirmation, [mbYes, mbNo], 0) = mrYes then
                begin
                  GAsyncThread.ExecuteASync(
                    procedure
                    begin
                      TPlasticEngine.CheckoutFile(FFileName, True);
                    end);
                end;
             end)
          end;
        end;
      finally
        info.Free;
      end;
    end);
//  except
//    on E:Exception do HandleException(E);
//  end;
end;

procedure TFileNotifier.BeforeSave;
begin
//  try
//    SendDebugFmt(dlObject, 'FileNotifier: saving file: %s',[FFileName]);
//  except
//    on E:Exception do HandleException(E);
//  end;
end;

function TFileNotifier.CheckOverwrite: Boolean;
begin
  Result := True; //overwrite is OK
end;

constructor TFileNotifier.Create(const aFilename: string; aModule: IOTAModule);
begin
  inherited Create;

  FFileName    := aFilename;
  FModule      := aModule;
  FNotifyIndex := aModule.AddNotifier(Self);
end;

procedure TFileNotifier.Destroyed;
begin
  RemoveNotifier;
end;

procedure TFileNotifier.Modified;
var
  result: TModalResult;
  info: TPlasticFileInfo;
begin
  try
    //SendDebugFmt(dlObject, 'FileNotifier: file modified: %s',[FFileName]);
    if not FModified then
    if not FBusy then
    begin
      FBusy := True;
      info  := TPlasticEngine.GetFileInfo(FFileName);
      try
        if (info <> nil) then
        if info.Status in [psCheckedIn, psCheckedInAndLocalChanged] then
        begin
          result := MessageDlg(Format('File is changed but not checked out:'+#13#10+
                                      '%s'+#13#10+''+#13#10+
                                      'Do you want to check it out now?', [FFileName]),
                                      Dialogs.mtConfirmation, [mbYes, mbNo, mbCancel], 0);
          if result = mrCancel then Exit;
          if result = mrYes then
            TPlasticEngine.CheckoutFile(FFileName, True);
          GPlasticExpert.UpdateMenu(True);
        end;
      finally
        FBusy := False;
        info.Free;
      end;

      FModified := True;
    end;
  except
    on E:Exception do HandleException(E);
  end;
end;

procedure TFileNotifier.ModuleRenamed(const NewName: string);
begin
  //
end;

procedure TFileNotifier.RemoveNotifier;
begin
  SendDebug(dlObject, 'TFileNotifier: removing notifier');
  try
    //remove from file list
    if _IDENotifier <> nil then
      _IDENotifier.FFileWatches.Remove(FFilename);

    //remove from IDE
    if FNotifyIndex >= 0 then
      FModule.RemoveNotifier(FNotifyIndex);
  except
    on E:Exception do HandleException(E);
  end;
  FNotifyIndex := -1;
end;

initialization
  _IDENotifier := TIDENotifier.Create;

finalization
  _IDENotifier.RemoveNotifier;
  _IDENotifier := nil;

end.
