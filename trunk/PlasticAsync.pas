unit PlasticAsync;

interface

uses
  Classes, Generics.Collections, SyncObjs, SysUtils;

type
  TAsyncThread = class(TThread)
  protected
    FNewMethods, FMethods: TList<TThreadProcedure>;
    FNewEvent: TEvent;
    procedure Execute;override;
  public
    procedure  AfterConstruction;override;
    destructor Destroy;override;

    procedure Terminate;
    procedure ExecuteASync(aMethod: TThreadProcedure);
  end;

var
  GAsyncThread   : TAsyncThread;

implementation

uses
  Dialogs, DbugIntf, PlasticUtils;

{ TAsyncThread }

procedure TAsyncThread.AfterConstruction;
begin
  inherited;
  FMethods    := TList<TThreadProcedure>.Create;
  FNewMethods := TList<TThreadProcedure>.Create;
  FNewEvent   := TEvent.Create;
end;

destructor TAsyncThread.Destroy;
begin
  FNewEvent.Free;
  FMethods.Free;
  FNewMethods.Free;
  inherited;
end;

procedure TAsyncThread.Execute;
var
  m: TThreadProcedure;
begin
  SendDebugEx(dlObject, 'Starting TAsyncThread', mtInformation);

  while not Terminated do
  try
    //wait for something new
    if FNewEvent.WaitFor(2*1000) = wrSignaled then
    begin
      FNewEvent.ResetEvent;
      //SendDebugFmt(dlObject, 'New async methods (%d) received',[FNewMethods.Count]);

      if not Terminated and (FNewMethods.Count > 0) then
      begin
        //lock + copy all functions to other list
        System.TMonitor.Enter(Self);
        try
          FMethods.Clear;
          for m in FNewMethods do
            FMethods.Add(m);
          FNewMethods.Clear;
        finally
          System.TMonitor.Exit(Self);
        end;

        //execute functions
        for m in FMethods do
        try
          m();
        except
          on e:Exception do HandleException(e);
        end;

      end;
    end;
  except
    on e:Exception do HandleException(e);
  end;
end;

procedure TAsyncThread.ExecuteASync(aMethod: TThreadProcedure);
begin
  FNewMethods.Add(aMethod);
  FNewEvent.SetEvent;
end;

procedure TAsyncThread.Terminate;
begin
  inherited Terminate;
  FNewEvent.SetEvent;
end;

initialization
  try
    GAsyncThread   := TAsyncThread.Create(False);
  except
    on E:Exception do HandleException(E);
  end;

finalization
  try
    GAsyncThread.Terminate;
    GAsyncThread.WaitFor;
    GAsyncThread.Free;
  except
    on E:Exception do HandleException(E);
  end;
  GAsyncThread   := nil;

end.
