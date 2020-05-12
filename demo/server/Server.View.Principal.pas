unit Server.View.Principal;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo,
  FMX.StdCtrls, FMX.Edit, SCO.Message.Base, SCO.Message.Server,
  SCO.Message.Interfaces;

type
  TServerPrincipal = class(TForm)
    Timer1: TTimer;
    Memo1: TMemo;
    EditRemoteAdress: TEdit;
    GroupBoxRemoteServer: TGroupBox;
    LabelAdress: TLabel;
    EditRemotePort: TEdit;
    LabelPort: TLabel;
    SwitchRemote: TSwitch;
    GroupBox1: TGroupBox;
    SwitchLocal: TSwitch;
    EditLocalPort: TEdit;
    Label1: TLabel;
    SCOMessageServer: TSCOMessageServer;
    procedure Timer1Timer(Sender: TObject);
    procedure SwitchLocalSwitch(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure SwitchRemoteSwitch(Sender: TObject);
    procedure SCOMessageServerReceberMensagem(Sender: TObject;
      AMensagem: IMessage);
  private
    { Private declarations }
    procedure StartLocalServer(const aPort: integer = 211);
    procedure StopLocalServer;
    procedure StartRemoteServer(const aAddress: string; const aPort: integer);
    procedure StopRemoteServer;

  public
    { Public declarations }
  end;

var
  ServerPrincipal: TServerPrincipal;

implementation

{$R *.fmx}

uses Jsons;

procedure TServerPrincipal.FormCreate(Sender: TObject);
begin
  SCOMessageServer.UserName := TGUID.NewGuid.ToString;
end;

procedure TServerPrincipal.SCOMessageServerReceberMensagem(Sender: TObject;
  AMensagem: IMessage);
begin
  Memo1.Lines.Insert(0,AMensagem.ToText);
end;

procedure TServerPrincipal.StartLocalServer(const aPort: integer);
begin
  SCOMessageServer.Port := aPort;
  SCOMessageServer.Open;
end;

procedure TServerPrincipal.StartRemoteServer(const aAddress: string;
  const aPort: integer);
begin
  SCOMessageServer.Server.Address := aAddress;
  SCOMessageServer.Server.Port    := aPort;
  SCOMessageServer.Running := True;
end;

procedure TServerPrincipal.StopLocalServer;
begin
  SCOMessageServer.Close;
end;

procedure TServerPrincipal.StopRemoteServer;
begin
  SCOMessageServer.Running := False;
end;

procedure TServerPrincipal.SwitchLocalSwitch(Sender: TObject);
var
  vPort: integer;
begin
  if SwitchLocal.IsChecked then
  begin
    vPort := StrToInt(EditLocalPort.Text);
    StartLocalServer(vPort);
  end
  else
    StopLocalServer;
end;

procedure TServerPrincipal.SwitchRemoteSwitch(Sender: TObject);
var
  vPort: integer;
begin
  if SwitchRemote.IsChecked then
  begin
    vPort := StrToInt(EditLocalPort.Text);
    StartRemoteServer(EditRemoteAdress.Text, vPort);
  end
  else
  begin
    StopRemoteServer;
  end;
end;

procedure TServerPrincipal.Timer1Timer(Sender: TObject);
//var
//  vJSon: TJson;
begin
//  vJSon := TJson.Create;
//  try
//    vJSon.Parse(MensageriaServidor1.ActiveUsers);
//    Memo1.Text := vJSon.Stringify;
//  finally
//    vJSon.DisposeOf;
//  end;
  Memo1.Text := SCOMessageServer.ActiveUsers;
end;

end.
