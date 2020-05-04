unit SCO.Message.Server;

interface

uses
  System.SysUtils,
  Data.SqlExpr,
  Data.DBXJSON,
  System.JSON,
  Data.DBXCommon,
  IPPeerClient,
  System.Classes,
  System.SyncObjs,
  System.StrUtils,
  DateUtils,
  System.Generics.Collections,
  SCO.Message.Functions,
  SCO.Message.Base,
  IdBaseComponent,
  IdComponent,
  IdCustomTCPServer,
  IdTCPServer,
  IdContext,
  IdServerIOHandler,
  IdServerIOHandlerSocket,
  IdServerIOHandlerStack,
  IdGlobal,
  SCO.Message.Interfaces;

type

  TMensageriaServidor  = class;

  TThreadProcessamentoServidor = class(TThread)
  private
    FMessageServer : TMensageriaServidor;
    procedure MsgUsuariosOnline(AServidorMensageria : TMensageriaServidor; AMensagem: IMessage);
    procedure MsgStatus(pMessage: IMessage);
    procedure MsgSubir(pMessage: IMessage);
  protected
    procedure Execute; override;
  public
    { Public declarations }
    constructor Create(AServer: TMensageriaServidor); reintroduce;
    destructor  Destroy; override;
  end;

  TMensageriaServidor = class(TMensageriaBase)
  private
    { Private declarations }
    FSocketServer: TIdTCPServer;
    FThreadProcessamento: TThreadProcessamentoServidor;
    FRouteController: IRouteController;
    procedure SetRunning(const Value: boolean);
    procedure RotaOffline(AUsuario, ADispositivo, AIDCliente : string);
    function  GetRunning: boolean;
    procedure ThreadProcessamentoDestruir;
    procedure CriarThreadProcessamento;
    procedure DoOnServerExecute(AContext: TIdContext);
    procedure DoOnDisconnect(AContext: TIdContext);
    procedure DesconectarCliente(AIDCliente: string);
    function GetPort: Word;
    procedure SetPort(const Value: Word);
  protected
    { Protected declarations }
    procedure SendToServer(AMensagem: IMessage); override;
    procedure DoAfterConectar; override;
    procedure DoAfterDesconectar; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure   EnviarMensagem(AMensagem: IMessage); override;
    procedure   Open; override;
    procedure   Close; override;

    function    ActiveUsers : string;
    function    CloseExternalConnections : integer;
  published
    { Published declarations }
    property Running: Boolean read GetRunning write SetRunning;
    property Port: Word read GetPort write SetPort;
  end;

implementation

uses
  FireDAC.Comp.Client,
  Data.DB,
  System.Threading;

{ TMensageriaServidor }

procedure TMensageriaServidor.Close;
begin
  inherited;
  Self.Running := False;
end;

constructor TMensageriaServidor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {Inicializar conexão local}
  FSocketServer := TIdTCPServer.Create(Self);
  FSocketServer.OnExecute := DoOnServerExecute;
  FSocketServer.OnDisconnect := DoOnDisconnect;

  FRouteController := TRouteControllerFacory.New;
end;

destructor TMensageriaServidor.Destroy;
begin
  ThreadProcessamentoDestruir;

  {Destruir conexão local}
  if Assigned(FSocketServer) then
  begin
    if (FSocketServer.Active) then
    begin
         FSocketServer.Active := False;
    end;
    FSocketServer.DisposeOf;
    FSocketServer := nil;
  end;

  inherited;
end;

procedure TMensageriaServidor.DoAfterConectar;
begin

end;

procedure TMensageriaServidor.DoAfterDesconectar;
begin
  inherited;

end;

procedure TMensageriaServidor.DoOnDisconnect(AContext: TIdContext);
begin
  FRouteController.UnregisterRoute(AContext);
end;

procedure TMensageriaServidor.DoOnServerExecute(AContext: TIdContext);
  procedure RedirectMessage(const AMessage: IMessage);
  var
    xRota: IRoute;
  begin
    for xRota in FRouteController.Routes do
    begin
      if xRota.HasUser(AMessage.Destiny) then
      begin
        xRota.QueueMessage(AMessage);
        Exit;
      end;
    end;
  end;
var
  xStr: string;
  xMessage: IMessage;
begin
  xStr := AContext.Connection.IOHandler.ReadString(AContext.Connection.IOHandler.ReadInt32, IndyTextEncoding_UTF8);

  if not xStr.Trim.IsEmpty then
  try
    // Converter string em record
    xMessage := TMessageFactory.New(xStr);
    FRouteController.RegisterRoute(AContext, xMessage.UserName);

    if SameStr(xMessage.Destiny.Trim, Self.UserName.Trim) then
    begin
      // enfileirar a mensagem para tratamento
      MessageQueue(xMessage);
    end
    else
    begin
      RedirectMessage(xMessage);
    end;

  except on E: Exception do

  end;
end;

function TMensageriaServidor.CloseExternalConnections : integer;
var
   i               : integer;
   xContextsList   : TList;
   xCurrentContext : TIdContext;
begin
  Result := 0;
//  try
//    xContextsList := FSocketServer.Contexts.LockList;
//    try
//      for i := 0 to Pred(xContextsList.Count) do
//      begin
//        xCurrentContext := TIdContext(xContextsList.Items[i]);
//
//        if not TControleRota.ExistePorID(xCurrentContext.Connection.IOHandler.GetHashCode.ToString) then
//        begin
//          xCurrentContext.Connection.IOHandler.Close;
//          Result := Result + 1;
//        end;
//      end;
//    finally
//      FSocketServer.Contexts.UnlockList;
//    end;
//  except
//
//  end;
end;

procedure TMensageriaServidor.RotaOffline(AUsuario, ADispositivo, AIDCliente: string);
begin
  {deletar a rota da minha lista de rotas}
  FRouteController.UnregisterRoute(AUsuario);
end;

function TMensageriaServidor.ActiveUsers: string;
  function InsertUsers(ARoute: IRoute): string;
  var
    vJson: TStrings;
    vItem: string;
  begin
    vJson := TStringList.Create;
    try
      for vItem in ARoute.Users do
      begin
        if vJson.Count > 0 then
        begin
          vJson.Add(',');
        end;
        vJson.Add('        "' + vItem + '"');
      end;
      Result := vJson.Text;
    finally
      vJson.DisposeOf;
    end;
  end;
  function InsertRoute(ARoute: IRoute): string;
  var
    vJson: TStrings;
  begin
    vJson := TStringList.Create;
    try
      vJson.Add('    "Adrress":"' + ARoute.IP + '",');
      vJson.Add('    "ID":"' + ARoute.ID.ToString + '",');
      vJson.Add('    "Users":[' + chr(13) +InsertUsers(ARoute) + '    ]');
      Result := '  {' + chr(13) + vJson.Text + '  }';
    finally
      vJson.DisposeOf;
    end;
  end;
var
  vRoute: IRoute;
  vJson: TStrings;
begin
  vJson := TStringList.Create;
  try
    for vRoute in FRouteController.Routes do
    begin
      if vJson.Count > 0 then
      begin
        vJson.Add(',');
      end;
      vJson.Add(InsertRoute(vRoute));
    end;
    Result :=
      '{"nodes":[' + chr(13) +
        vJson.Text +
      ']}';
  finally
    vJson.DisposeOf;
  end;
end;

procedure TMensageriaServidor.SendToServer(AMensagem: IMessage);
var
  xMessageStr: string;
begin
  if Self.Servidor.IsConnected then
  begin
    try
      if AMensagem.UserName.Trim.IsEmpty then
      begin
        AMensagem.UserName := Self.UserName;
      end;
      xMessageStr := AMensagem.ToText;
      Self.QueueMessage(xMessageStr);
    except on E: Exception do

    end;
  end;
end;

procedure TMensageriaServidor.SetRunning(const Value: boolean);
begin
  if csDesigning in Self.ComponentState then
  begin
    Exit;
  end;

  if FSocketServer.Active then
    FSocketServer.Active := False;

  {Limpar as rotas}
  FRouteController.EmptyRoutes;

  if Value then
  begin
    FSocketServer.Active := True;
  end;
  CriarThreadProcessamento;
end;

procedure TMensageriaServidor.SetPort(const Value: Word);
begin
  FSocketServer.DefaultPort := Value;
end;

procedure TMensageriaServidor.CriarThreadProcessamento;
begin
  ThreadProcessamentoDestruir;
  if GetRunning then
  begin
    FThreadProcessamento := TThreadProcessamentoServidor.Create(Self);
  end;
end;

procedure TMensageriaServidor.ThreadProcessamentoDestruir;
begin
  if Assigned(FThreadProcessamento) then
  begin
    FThreadProcessamento.Terminate;
    if not FThreadProcessamento.Suspended then
    begin
      FThreadProcessamento.WaitFor;
    end;
    FThreadProcessamento.DisposeOf;
    FThreadProcessamento := nil;
  end;
end;

procedure TMensageriaServidor.DesconectarCliente(AIDCliente: string);
var
  xContextsList: TList;
  i: Integer;
  xCurrentContext: TIdContext;
begin
  xContextsList := FSocketServer.Contexts.LockList;
  try
    for i := 0 to Pred(xContextsList.Count) do
    begin
      xCurrentContext := TIdContext(xContextsList.Items[i]);
      if xCurrentContext.Connection.IOHandler.GetHashCode.ToString = AIDCliente then
      begin
        xCurrentContext.Connection.IOHandler.Close;
        Break;
      end;
    end;
  finally
    FSocketServer.Contexts.UnlockList;
  end;
end;

procedure TMensageriaServidor.EnviarMensagem(AMensagem: IMessage);
begin
  // procurar rotas
  if not FRouteController.QueueMessage(AMensagem) then
  begin
    // enviar para o servidor remoto
    Self.SendToServer(AMensagem);
  end;
end;

function TMensageriaServidor.GetRunning: boolean;
begin
  try
    Result := FSocketServer.Active;
  except
    Result := False;
  end;
end;

function TMensageriaServidor.GetPort: Word;
begin
  Result := FSocketServer.DefaultPort;
end;

procedure TMensageriaServidor.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
end;

procedure TMensageriaServidor.Open;
begin
  inherited;
  Self.Running := True;
end;

{ TThreadProcessamento }

constructor TThreadProcessamentoServidor.Create(AServer: TMensageriaServidor);
begin
  inherited Create(False);
  FMessageServer  := AServer;
end;

destructor TThreadProcessamentoServidor.Destroy;
begin
  inherited;
end;

procedure TThreadProcessamentoServidor.Execute;
var
  xMsgInterna: IMessage;
begin
  inherited;
  while not Self.Terminated do
  begin
    try
      if not FMessageServer.MessageDequeue(xMsgInterna) then
      begin
        // caso não tenha mensagem na pilha, consome 100% do processamento
        // pois estamos em um while infinito
        Sleep(100);

        if Self.Terminated then Exit;
      end else begin

        if Self.Terminated then Exit;

        if (SameStr(xMsgInterna.Destiny.Trim, FMessageServer.UserName.Trim)) then
        begin

          // Mensagens de Status Online/Offline
          MsgUsuariosOnline(FMessageServer,xMsgInterna);

          if Self.Terminated then Exit;

          Self.FMessageServer.MsgEntregar(xMsgInterna);

          if Self.Terminated then Exit;

          // Mensagens de Status Online/Offline
          MsgStatus(xMsgInterna);

        end else begin

          if Self.Terminated then
            Exit;

          MsgSubir(xMsgInterna);

        end;
      end;
    except on E: Exception do
      sleep(100);
    end;
  end;
end;

procedure TThreadProcessamentoServidor.MsgStatus(pMessage: IMessage);
begin
  {Mensagem informando que o Client não esta mais Online}
  if pMessage.Params.ContainsKey('status.offline') then
  begin
    FMessageServer.FRouteController.UnregisterRoute(pMessage.UserName);
  end;
  if pMessage.Params.ContainsKey('status.verificacao') then
  begin
    if pMessage.Destiny = FMessageServer.UserName then
    begin
      FMessageServer.StatusOnline(pMessage.UserName);
    end;
  end;
end;

procedure TThreadProcessamentoServidor.MsgSubir(pMessage: IMessage);
begin
  if not SameStr(pMessage.Destiny.Trim, FMessageServer.UserName.Trim) then
  begin
    FMessageServer.SendToServer(pMessage);
  end;
end;

procedure TThreadProcessamentoServidor.MsgUsuariosOnline(AServidorMensageria : TMensageriaServidor; AMensagem: IMessage);
var
  xMsg: IMessage;
begin
  {Solicitacao de usuarios Online}
  if AMensagem.Params.ContainsKey('usuarios.online') then
  begin
    xMsg := TMessagefactory.New;
    xMsg.Destiny := AMensagem.UserName;
    xMsg.Params.Add('usuarios.online.resposta', AServidorMensageria.ActiveUsers);
    AServidorMensageria.EnviarMensagem(xMsg);
  end;
end;

end.
