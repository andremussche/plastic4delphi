unit fDebugConsole;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls;

type
  TfrmDebugConsole = class(TForm)
    Panel1: TPanel;
    btnClose: TButton;
    Memo1: TMemo;
    btnClear: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

  function DebugConsole: TfrmDebugConsole;

implementation

uses
  DbugIntf;

{$R *.dfm}

var
  frmDebugConsole: TfrmDebugConsole;

function DebugConsole: TfrmDebugConsole;
begin
  if frmDebugConsole = nil then
    frmDebugConsole := TfrmDebugConsole.Create(Application);
  Result := frmDebugConsole;
end;

procedure TfrmDebugConsole.btnClearClick(Sender: TObject);
begin
  Memo1.Clear;
end;

procedure TfrmDebugConsole.btnCloseClick(Sender: TObject);
begin
  Hide;
end;

procedure TfrmDebugConsole.FormCreate(Sender: TObject);
begin
  Memo1.Clear;

  DebugSendThread.OnLog :=
    //attach log procedure
    procedure(aMsg: string)
    begin
      //thread safe add msg to memo
      TThread.Queue(nil,
        procedure
        begin
          Memo1.Lines.BeginUpdate;
          try
            Memo1.Lines.Add(aMsg);
            //max 1000 msg
            while Memo1.Lines.Count > 1000 do
              Memo1.Lines.Delete(0);
          finally
            Memo1.Lines.EndUpdate;
          end;
        end);
    end;
end;

procedure TfrmDebugConsole.FormDestroy(Sender: TObject);
begin
  //detach
  DebugSendThread.OnLog := nil;
  //dirty: reset var to nil
  frmDebugConsole := nil;
end;

end.
