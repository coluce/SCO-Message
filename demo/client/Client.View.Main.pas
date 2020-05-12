unit Client.View.Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.StdCtrls, FMX.Controls.Presentation, FMX.Edit, FMX.ListView.Types,
  FMX.ListView.Appearances, FMX.ListView.Adapters.Base, FMX.ListView,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, SCO.Message.Base, SCO.Message.Client,
  SCO.Message.Interfaces;

type
  TForm1 = class(TForm)
    EditUsuario: TEdit;
    btnSendMessage: TButton;
    edtMessage: TEdit;
    edtDestinatario: TEdit;
    Button1: TButton;
    ListView1: TListView;
    StatusBar1: TStatusBar;
    Timer1: TTimer;
    Switch1: TSwitch;
    BindingsList1: TBindingsList;
    LinkControlToPropertyEnabled: TLinkControlToProperty;
    Clear: TButton;
    SCOMessageClient: TSCOMessageClient;
    procedure btnSendMessageClick(Sender: TObject);
    procedure MensageriaCliente1Conectar(Sender: TObject);
    procedure MensageriaCliente1Desconectar(Sender: TObject);
    procedure MensageriaCliente1Reconectar(Sender: TObject);
    procedure MensageriaCliente1ReceberMensagem(Sender: TObject; AMensagem: IMessage);
    procedure Button1Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ClearClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure Add(const AMessage: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Add(const AMessage: string);
begin
  ListView1.Items.Insert(0).Text := FormatDateTime('hh:nn:ss:zzz',Now) + ': ' + AMessage;
  if ListView1.Items.Count > 100 then
  begin
    ListView1.Items.Delete(100);
  end;
end;

procedure TForm1.ClearClick(Sender: TObject);
begin
  ListView1.Items.Clear;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SCOMessageClient.Server.Address := '127.0.0.1';
  SCOMessageClient.Server.Port := 211;
end;

procedure TForm1.btnSendMessageClick(Sender: TObject);
var
  vMsg: IMessage;
begin
  vMsg := TMessageFactory.New;
  vMsg.Params.Add('chat',edtMessage.Text);
  vMsg.Destiny := edtDestinatario.Text;
  SCOMessageClient.EnviarMensagem(vMsg);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  if SCOMessageClient.ConectadoRemoto then
  begin
    SCOMessageClient.Close;
  end
  else
  begin
    SCOMessageClient.UserName := EditUsuario.Text;
    SCOMessageClient.Open;
  end;
end;

procedure TForm1.MensageriaCliente1Conectar(Sender: TObject);
begin
  Add('conectado');
end;

procedure TForm1.MensageriaCliente1Desconectar(Sender: TObject);
begin
  Add('desconectado');
end;

procedure TForm1.MensageriaCliente1ReceberMensagem(Sender: TObject; AMensagem: IMessage);
var
  vChat: string;
begin
  if AMensagem.Params.TryGetValue('chat',vChat) then
  begin
    Add(vChat);
  end;

  if AMensagem.Params.TryGetValue('usuarios.online.resposta',vChat) then
  begin
    Add(vChat);
  end;

end;

procedure TForm1.MensageriaCliente1Reconectar(Sender: TObject);
begin
  Add('reconectado');
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  btnSendMessageClick(btnSendMessage);
end;

end.
