unit SCO.Message.Base;

interface

uses
  System.Classes,
  Data.DBXJSON,
  System.JSON,
  System.SysUtils,
  Data.DBXCommon,
  Data.SqlExpr,
  System.SyncObjs,
  SCO.Message.Interfaces,
  System.Generics.Collections,
  DateUtils,
  IdBaseComponent,
  IdComponent,
  IdTCPConnection,
  IdTCPClient,
  IdIOHandler,
  IdIOHandlerSocket,
  IdIOHandlerStack,
  IdGlobal,
  FireDAC.Comp.DataSet;

const
  cstVersaoMensagem    : string = '0';

type

  TSCOMessageCommon = class;
  TSCOReadThread = class;

  TOnMessageEvent = procedure (Sender : TObject; AMensagem : IMessage) of object;

  ISCOMessageListener = Interface(IInterface)
    { Interfaced declarations }
    ['{3D52D526-3E9F-4BE1-93DC-C64539FA6440}']
    procedure DoReceiveMessage(const Sender: TObject; const Mensagem: IMessage);
    function GetId : string;
  end;

  TSCORemoteServer = class(TPersistent)
  private
    FReconectando: boolean;
    FPort: integer;
    FAddress: string;
    FSocketClient: TIdTCPClient;
    FOwner: TSCOMessageCommon;
    FThreadLeitura: TSCOReadThread;

    procedure CreateThread;
    procedure DestroyThread;

  public
    constructor Create(AOwner: TSCOMessageCommon; ASocket: TIdTCPClient);
    destructor Destroy; override;
    function QueueMessage(AMensagem : string): boolean;
    function IsConnected: Boolean;
    procedure Reconnect;
  published
    property Address: string read FAddress write FAddress;
    property Port: integer read FPort write FPort;
  end;

  { TThreadEnvio }

  TSCOSendThread = class(TThread)
  private
    FListaMensagem: TArray<string>;
    FServidorRemoto: TSCORemoteServer;
    FTCPClient: TIdTCPClient;
  protected
    procedure Execute; override;
  public
    constructor Create;
    destructor  Destroy ; override;
    procedure   Enfileirar(AStrMensagem : string);
  //published
    property RemoteServer: TSCORemoteServer read FServidorRemoto write FServidorRemoto;
    property TCPClient: TIdTCPClient read FTCPClient write FTCPClient;
  end;

  TSCOReadThread = class(TThread)
  private
    FServidorRemoto : TSCORemoteServer;
    FTCPClient      : TIdTCPClient;
  protected
    procedure Execute; override;
  public
    constructor Create(const ARemoteServer: TSCORemoteServer; const AConnection: TIdTCPClient);
    destructor  Destroy ; override;
  //published
    property RemoteServer : TSCORemoteServer read FServidorRemoto;
    property TCPClient      : TIdTCPClient    read FTCPClient;
  end;

  TSCOReconectThread = class(TThread)
  private
    FServidorRemoto : TSCORemoteServer;
  protected
    procedure Execute; override;
  public
    constructor Create(AServidorRemoto : TSCORemoteServer); reintroduce;
    destructor  Destroy ; override;
  end;

  TSCOMessageCommon = class(TComponent)
  private
    FConectarRemoto   : boolean;
    FUserName: string;
    FSocketClient     : TIdTCPClient;
    FServer         : TSCORemoteServer;
    FMessageList      : TArray<IMessage>;
    FOnReconnect     : TNotifyEvent;
    FOnConnect       : TNotifyEvent;
    FOnDisconnect    : TNotifyEvent;
    FThreadEnvio      : TSCOSendThread;
    FThreadReconectar : TSCOReconectThread;
    FOnReceiveMessage: TOnMessageEvent;
    FConectadoRemotoStoredUsage: TFDStoredActivationUsage;
    procedure DesconectarSocketClient;
    function  GetConectadoRemoto : boolean;
    procedure SetConectadoRemoto(const Value: boolean);
    function  IniciarConexaoServidor : boolean;
    procedure DoOnClientDisconnected(Sender: TObject);
    procedure DoOnStatus(ASender: TObject; const AStatus: TIdStatus; const AStatusText: string);
    procedure DestruirThreadReconectar;
    procedure DestruirThreadEnviar;
    procedure CriarThreadReconectar;
    procedure CriarThreadEnviar;
    procedure DestruirServidorRemoto;
  protected
    function  QueueMessage(AStrMensagem : string) : boolean;
    procedure SendToServer(AMensagem : IMessage); virtual; abstract;
    procedure DoAfterConectar; virtual; abstract;
    procedure DoAfterDesconectar; virtual; abstract;
    procedure RemoteOpen;
    procedure RemoteClose;

    function    HasRemoteServer : boolean;
    procedure   MsgEntregar(AMensagem : IMessage);
    procedure   MessageQueue(AMensagem : IMessage);
    function    MessageDequeue(out AMensagemInterna : IMessage): boolean;
    procedure   StatusOffline;

    procedure Loaded; override;

  public
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   SendStatusOnline(ADestinatario : string = '');

    procedure   Open; virtual; abstract;
    procedure   Close; virtual; abstract;
    procedure   SendMessage(AMensagem : IMessage); virtual; abstract;
  published
    property IsRemoteConnected : Boolean         read GetConectadoRemoto write SetConectadoRemoto;
    property IsRemoteConnectedStoredUsage : TFDStoredActivationUsage read FConectadoRemotoStoredUsage write FConectadoRemotoStoredUsage default [auDesignTime];
    property UserName: string read FUserName write FUserName;
    property Server          : TSCORemoteServer read FServer          write FServer;
  published
    property OnReconnect      : TNotifyEvent    read FOnReconnect      write FOnReconnect;
    property OnConnect        : TNotifyEvent    read FOnConnect        write FOnConnect;
    property OnDisconnect     : TNotifyEvent    read FOnDisconnect     write FOnDisconnect;
    property OnReceiveMessage : TOnMessageEvent read FOnReceiveMessage write FOnReceiveMessage;

  end;

implementation

uses
  SCO.Message.Functions;

var
  secMessageQueue       : TCriticalSection;
  secSendMessageSession : TCriticalSection;

{ TReadingThread }

constructor TSCOReadThread.Create(const ARemoteServer: TSCORemoteServer; const AConnection: TIdTCPClient);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FServidorRemoto := ARemoteServer;
  FTCPClient := AConnection;
end;

destructor TSCOReadThread.Destroy;
begin
  inherited;
end;

procedure TSCOReadThread.Execute;
const
  SPEEP_TIME: integer = 150;
  function LerStrMensagem: String;
  begin
    Result := EmptyStr;
    try
      if Assigned(FTCPClient) then
      begin
        if FTCPClient.Connected then
        begin
          if Assigned(FTCPClient.IOHandler) then
          begin
            FTCPClient.IOHandler.CheckForDataOnSource(SPEEP_TIME);
            if FTCPClient.IOHandler.InputBufferIsEmpty then
            begin
              Exit;
            end;
            Result := FTCPClient.IOHandler.ReadString(FTCPClient.IOHandler.ReadInt32, IndyTextEncoding_UTF8);
          end;
        end
        else
        begin
          sleep(SPEEP_TIME);
        end;
      end;
    except on E: Exception do
      begin
        Result := EmptyStr;
        Sleep(SPEEP_TIME);
      end;
    end;
  end;
var
  xMensagem : IMessage;
  xStrMsg   : string;
begin
  while not Self.Terminated do
  begin
    try
      if FTCPClient.Connected then
      begin
        xStrMsg := LerStrMensagem;
        if not xStrMsg.IsEmpty then
        begin
          xMensagem := TMessagefactory.New(xStrMsg);
          FServidorRemoto.FOwner.MessageQueue(xMensagem);
        end;
      end
      else
      begin
        sleep(SPEEP_TIME);
      end;
    except
      sleep(SPEEP_TIME);
    end;
  end;
end;

{ TMensageriaBase }

constructor TSCOMessageCommon.Create(AOwner: TComponent);
begin
  inherited;
  FConectarRemoto        := False;

  {Inicializar conexão remota}
  FSocketClient                := TIdTCPClient.Create(Self);
  FSocketClient.OnStatus       := DoOnStatus;
  FSocketClient.ConnectTimeout := 5000;

  {Inicializando Dados do Servidor Remoto}
  FServer := TSCORemoteServer.Create(Self, FSocketClient);
  FServer.Address := 'localhost';

  SetLength(FMessageList,0);

end;

procedure TSCOMessageCommon.DesconectarSocketClient;
begin
  StatusOffline;
  try
    if Assigned(FSocketClient) then
    begin
      if Assigned(FSocketClient.IOHandler) and Assigned(FSocketClient.IOHandler.InputBuffer) then
      begin
        FSocketClient.IOHandler.InputBuffer.clear;
        FSocketClient.IOHandler.CloseGracefully;
      end;
      FSocketClient.Disconnect;
      Sleep(250);
    end;
  except

  end;
  DestruirThreadReconectar;
  DoAfterDesconectar;
end;

destructor TSCOMessageCommon.Destroy;
begin
  DesconectarSocketClient;
  DestruirThreadEnviar;
  DestruirThreadReconectar;
  DestruirServidorRemoto;

  SetLength(FMessageList,0);

  inherited;
end;

procedure TSCOMessageCommon.DoOnClientDisconnected(Sender: TObject);
begin
  //DestruirThreadLeitura;
  //DestruirThreadEnviar;
  if Assigned(FOnDisconnect) then
  begin
    FOnDisconnect(Sender);
  end;
end;

procedure TSCOMessageCommon.DoOnStatus(ASender: TObject; const AStatus: TIdStatus;
  const AStatusText: string);
begin
  case AStatus of
    hsResolving: ;
    hsConnecting: ;
    hsConnected: ;
    hsDisconnecting,
    hsDisconnected:
    begin
      DoOnClientDisconnected(Asender);
    end;
    hsStatusText: ;
    ftpTransfer: ;
    ftpReady: ;
    ftpAborted: ;
  end;
end;

function TSCOMessageCommon.IniciarConexaoServidor : Boolean;
  function Conectar(const AEndereco : string; const APorta : integer) : boolean;
  begin
    try

      if FServer.IsConnected then
      begin
        DesconectarSocketClient;
      end;

      sleep(150);

      FSocketClient.Host := AEndereco;
      FSocketClient.Port := APorta;

      FSocketClient.Connect;

      Result := FServer.IsConnected;

    except
      Result := False;
    end;
  end;
begin
  Result := False;
  try
    FConectarRemoto := True;
    if Conectar(FServer.Address,FServer.Port) then
    begin
      DoAfterConectar;
      CriarThreadReconectar;
      FServer.CreateThread;
      CriarThreadEnviar;
      SendStatusOnline;
      if Assigned(FOnConnect) then
      begin
        FOnConnect(Self);
      end;
      Result := True;
    end;
  except on E: Exception do
    begin
      Result := False;
    end;
  end;
end;
procedure TSCOMessageCommon.Loaded;
begin
  inherited;
  SetConectadoRemoto(FConectarRemoto);
end;

function TSCOMessageCommon.GetConectadoRemoto: boolean;
begin
  Result := FServer.IsConnected;
end;

function TSCOMessageCommon.HasRemoteServer: boolean;
begin
  {Esta função retorna se o componente tem que conectar a um servidor remoto}
  Result := FConectarRemoto;
end;

function TSCOMessageCommon.MessageDequeue(out AMensagemInterna: IMessage): boolean;
begin
  Result := False;
  if Assigned(secMessageQueue) then
  begin
    secMessageQueue.Enter;
    try
      if Length(FMessageList) > 0 then
      begin
        AMensagemInterna := FMessageList[0];
        Delete(FMessageList,0,1);
        Result := True;
      end;
    finally
      secMessageQueue.Leave;
    end;
  end;
end;

procedure TSCOMessageCommon.MessageQueue(AMensagem: IMessage);
begin
  secMessageQueue.Enter;
  try
    SetLength(FMessageList,Length(FMessageList)+1);
    FMessageList[Length(FMessageList)-1] := AMensagem;
  finally
    secMessageQueue.Leave;
  end;
end;

procedure TSCOMessageCommon.MsgEntregar(AMensagem : IMessage);
var
  xMsg : IMessage;
begin
  if Assigned(FOnReceiveMessage) then
  begin
    xMsg := AMensagem;
    TThread.Queue(TThread.CurrentThread,
      procedure
      begin
        FOnReceiveMessage(Self,xMsg);
      end
    );
  end;
end;

procedure TSCOMessageCommon.RemoteOpen;
begin
  Self.IsRemoteConnected := True;
end;

procedure TSCOMessageCommon.RemoteClose;
begin
  Self.IsRemoteConnected := False;
end;

function TSCOMessageCommon.QueueMessage(AStrMensagem: string): boolean;
begin
  Result := FServer.QueueMessage(AStrMensagem);
end;

procedure TSCOMessageCommon.SetConectadoRemoto(const Value: boolean);
begin

  if (csLoading in Self.ComponentState) then
  begin
    if FDCheckStoredUsage(Self.ComponentState, Self.IsRemoteConnectedStoredUsage) then
    begin
      FConectarRemoto := Value;
    end;
    Exit;
  end;

  if Value and FUserName.Trim.IsEmpty then
  begin
    raise Exception.Create('Propriedade Usuario inválida.');
  end;

  if Self.IsRemoteConnected then
  begin
    DesconectarSocketClient;
  end;
  FSocketClient.Host := '';
  FSocketClient.Port := 0;

  if Value then
  begin
    IniciarConexaoServidor;
  end;
end;

procedure TSCOMessageCommon.StatusOffline;
var
  xStatus : IMessage;
begin
  {Montar mensagem de notificação de status online}
  xStatus := TMessageFactory.New;
  xStatus.UserName   := FUserName;
  xStatus.Params.Add('status.offline',EmptyStr);
  SendMessage(xStatus);
  sleep(50);
end;

procedure TSCOMessageCommon.SendStatusOnline(ADestinatario : string = '');
var
  xStatus : IMessage;
begin
  {Montar mensagem de notificação de status online}
  xStatus := TMessageFactory.New;
  xStatus.UserName   := FUserName;
  if not ADestinatario.Trim.IsEmpty then
  begin
    xStatus.Destiny := ADestinatario;
  end;
  xStatus.Params.Add('status.online',EmptyStr);
  SendMessage(xStatus);
end;

procedure TSCOMessageCommon.DestruirServidorRemoto;
begin
  {Destruir dados do servidor remoto}
  if Assigned(FServer) then
  begin
    FServer.DisposeOf;
    FServer := nil;
  end;
end;

procedure TSCOMessageCommon.CriarThreadEnviar;
begin
  DestruirThreadEnviar;
  FThreadEnvio                := TSCOSendThread.Create;
  FThreadEnvio.RemoteServer := FServer;
  FThreadEnvio.TCPClient      := FSocketClient;
  FThreadEnvio.Start;
end;

procedure TSCOMessageCommon.CriarThreadReconectar;
begin
  DestruirThreadReconectar;
  FThreadReconectar := TSCOReconectThread.Create(FServer);
end;

procedure TSCOMessageCommon.DestruirThreadEnviar;
begin
  if Assigned(FThreadEnvio) then
  begin
    FThreadEnvio.Terminate;
    if not FThreadEnvio.Finished then
    begin
      FThreadEnvio.WaitFor;
    end;
    FThreadEnvio.DisposeOf;
    FThreadEnvio := nil;
  end;
end;

procedure TSCOMessageCommon.DestruirThreadReconectar;
begin
  if Assigned(FThreadReconectar) then
  begin
    FThreadReconectar.Terminate;
    if not FThreadReconectar.Finished then
    begin
      FThreadReconectar.WaitFor;
    end;
    FThreadReconectar.DisposeOf;
    FThreadReconectar := nil;
  end;
end;

{ TServidorRemoto }

constructor TSCORemoteServer.Create(AOwner : TSCOMessageCommon ;ASocket : TIdTCPClient);
begin
  FOwner        := AOwner;
  FSocketClient := ASocket;
  FReconectando := False;
end;

procedure TSCORemoteServer.CreateThread;
begin
  DestroyThread;
  FThreadLeitura := TSCOReadThread.Create(Self, FSocketClient);
  FThreadLeitura.Start;
end;

destructor TSCORemoteServer.Destroy;
begin
  DestroyThread;
  FOwner.DesconectarSocketClient;
  FSocketClient.DisposeOf;
  FSocketClient := nil;
  inherited;
end;

procedure TSCORemoteServer.DestroyThread;
begin
  if Assigned(FThreadLeitura) then
  begin
    FThreadLeitura.Terminate;
    if not FThreadLeitura.Terminated then
    begin
      FThreadLeitura.WaitFor;
    end;
    FThreadLeitura.DisposeOf;
    FThreadLeitura := nil;
  end;
end;

function TSCORemoteServer.IsConnected: Boolean;
begin
  try
    Result := FSocketClient.Connected;
  except
    Result := False;
    if Assigned(FSocketClient.IOHandler) and Assigned(FSocketClient.IOHandler.InputBuffer) then
    begin
      FSocketClient.IOHandler.InputBuffer.Clear;
      FSocketClient.IOHandler.CloseGracefully;
    end;
    FSocketClient.Disconnect;
  end;
end;

procedure TSCORemoteServer.Reconnect;
begin
  if not FReconectando then
  begin
    FReconectando := True;
    try
      try
        if IsConnected then
        begin
          FOwner.DesconectarSocketClient;
        end;
        Self.FOwner.IniciarConexaoServidor;

        if IsConnected then
        begin
          // disparar evento para o componente de reconectado
          if Assigned(FOwner.FOnReconnect) then
          begin
            TThread.Synchronize(nil, procedure
            begin
              FOwner.FOnReconnect(Self.FOwner);
            end);
          end;
        end;
      except

      end;
    finally
      FReconectando := False;
    end;
  end;
end;

function TSCORemoteServer.QueueMessage(AMensagem: string): boolean;
begin
  Result := False;
  if Assigned(FOwner.FThreadEnvio) then
  begin
    FOwner.FThreadEnvio.Enfileirar(AMensagem);
    Result := True;
  end;
end;

{ TThreadEnvio }

constructor TSCOSendThread.Create;
begin
  inherited Create(True);
  FreeOnTerminate := False;
  SetLength(FListaMensagem,0);
end;

destructor TSCOSendThread.Destroy;
begin
  SetLength(FListaMensagem,0);
  inherited;
end;

procedure TSCOSendThread.Execute;
var
  xString:   string;
  Buf:       TIdBytes;
  //xMsgIndex: integer;
const
  MsgBuffer: integer = 100;
begin
  inherited;
  //xMsgIndex := -1;
  while not Terminated do
  begin
    // caso tenha mensagens
    if (Length(FListaMensagem) > 0) and FTCPClient.Connected then
    begin
      //pegar a mensagem para enviar
      secSendMessageSession.Enter;
      try
        //xMsgIndex := xMsgIndex + 1;
        xString := FListaMensagem[0];
        Delete(FListaMensagem,0,1);
//        xString := FListaMensagem[xMsgIndex];
//        if xMsgIndex >= MsgBuffer then
//        begin
//          xMsgIndex := -1;
//          Delete(FListaMensagem,0,MsgBuffer);
//        end;
      finally
        secSendMessageSession.Leave;
      end;

      if Assigned(FTCPClient) then
      begin
        if Assigned(FTCPClient.IOHandler) then
        begin
          // enviar a mensagem
          try
            Buf := IndyTextEncoding_UTF8.GetBytes(xString);
            FTCPClient.IOHandler.Write(Int32(Length(Buf)));
            FTCPClient.IOHandler.Write(Buf);
          except on E: exception do
            begin
              sleep(50);
            end;
          end;
        end else begin
          sleep(50);
        end;
      end else begin
        sleep(50);
      end;
    end
    else
    begin
      sleep(50);
    end;
  end;

end;

procedure TSCOSendThread.Enfileirar(AStrMensagem: string);
begin
  secSendMessageSession.Enter;
  try
    SetLength(FListaMensagem,Length(FListaMensagem)+1);
    FListaMensagem[Length(FListaMensagem)-1] := AStrMensagem;
  finally
    secSendMessageSession.Leave;
  end;
end;

{ TReconectarThread }

constructor TSCOReconectThread.Create(AServidorRemoto: TSCORemoteServer);
begin
  inherited Create(False);
  FreeOnTerminate := False;
  FServidorRemoto := AServidorRemoto;
end;

destructor TSCOReconectThread.Destroy;
begin

  inherited;
end;

procedure TSCOReconectThread.Execute;
var
  I: Integer;
begin
  inherited;
  while not Self.Terminated do
  begin
    for I := 0 to 1500 do
    begin
      if Self.Terminated then
        Exit;
      sleep(100);
    end;
    if not FServidorRemoto.IsConnected then
    begin
      TThread.Synchronize(nil, procedure
        begin
         FServidorRemoto.Reconnect;
        end);
    end;
  end;
end;

initialization
  secMessageQueue       := TCriticalSection.Create;
  secSendMessageSession := TCriticalSection.Create;

finalization
  secMessageQueue.DisposeOf;
  secMessageQueue := nil;

  secSendMessageSession.DisposeOf;
  secSendMessageSession := nil;

end.