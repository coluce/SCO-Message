unit SCO.Message.Client;

interface

uses
  System.SysUtils,
  System.Classes,
  System.UITypes,
  IPPeerClient,
  SCO.Message.Base,
  SCO.Message.Interfaces,
  Data.SqlExpr,
  System.Generics.Collections;

type

  TSCOClientProcessThread = class;
  TSCOMessageClient          = class;

  { TSCOListenerSendThread }
  TSCOListenerSendThread = class(TThread)
  private
    { Private declarations }
    FException: Exception;
    FListener: ISCOMessageListener;
    FMensagem: IMessage;
    procedure DoHandleException;
  protected
    { Protected declarations }
    procedure Execute; override;
    procedure HandleException; virtual;
  public
    { Public declarations }
    constructor Create(const AListener : ISCOMessageListener; const AMensagem: IMessage); reintroduce;
    property Listener : ISCOMessageListener read FListener;
    property Mensagem : IMessage read FMensagem;
  end;

  TSCOClientProcessThread = class(TThread)
  private
    FMessageClient: TSCOMessageClient;
    procedure MsgStatus(AClienteMensageria : TSCOMessageClient; AMensagem: IMessage);
  protected
    procedure Execute; override;
  public
    { Public declarations }
    constructor Create(AClienteMensageria : TSCOMessageClient); reintroduce;
    destructor  Destroy; override;
  end;

  TSCOMessageClient = class(TSCOMessageCommon)
  private
    FMensagemProcessamento : TSCOClientProcessThread;
    FListeners             : TList<ISCOMessageListener>;
    procedure DestruirThreadMonitora;
    procedure CriarThreadMonitora;
    { Private declarations }
  protected
    { Protected declarations }
    procedure SendToServer(AMensagem: IMessage); override;
    procedure DoAfterConectar; override;
    procedure DoAfterDesconectar; override;
  public
    { Public declarations }
    constructor Create(AOwner : TComponent); override;
    destructor  Destroy; override;
    procedure   EnviarMensagem(AMensagem: IMessage); override;

    procedure   VerificarOnline(ADestinatario : string);
    procedure   RequestActiveUsers;
    procedure   RegisterListener(const AListener: ISCOMessageListener);
    procedure   UnregisterListener(const AListener: ISCOMessageListener);

    procedure   Open; override;
    procedure   Close; override;
  end;

implementation

{ TMensageriaCliente }

procedure TSCOMessageClient.Close;
begin
  Self.ConectadoRemoto := False;
end;

procedure TSCOMessageClient.CriarThreadMonitora;
begin
  DestruirThreadMonitora;
  FMensagemProcessamento := TSCOClientProcessThread.Create(Self);
end;

constructor TSCOMessageClient.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FListeners := TList<ISCOMessageListener>.Create;
end;

destructor TSCOMessageClient.Destroy;
begin
  DestruirThreadMonitora;
  FListeners.DisposeOf;
  FListeners := nil;
  inherited;
end;

procedure TSCOMessageClient.Open;
begin
  Self.ConectadoRemoto := True;
end;

procedure TSCOMessageClient.RegisterListener(const AListener: ISCOMessageListener);
begin
  if not FListeners.Contains(AListener) then
  begin
    FListeners.Add(AListener);
    FListeners.TrimExcess;
  end;
end;

procedure TSCOMessageClient.RequestActiveUsers;
var
  xMsg: IMessage;
begin
  xMsg := TMessageFactory.New;
  xMsg.Destiny := 'servidor';
  xMsg.Params.Add('usuarios.online',EmptyStr);
  EnviarMensagem(xMsg);
end;

procedure TSCOMessageClient.SendToServer(AMensagem: IMessage);
begin
  try
    if AMensagem.UserName.Trim.IsEmpty then
    begin
      AMensagem.UserName := Self.UserName;
    end;
    {Subir a mensagem}
    Server.QueueMessage(AMensagem.ToText);
  except
  end;
end;

procedure TSCOMessageClient.UnregisterListener(const AListener: ISCOMessageListener);
begin
  if FListeners.Contains(AListener) then
  begin
    FListeners.Remove(AListener);
    FListeners.TrimExcess;
  end;
end;

procedure TSCOMessageClient.VerificarOnline(ADestinatario: string);
var
  xMsg: IMessage;
begin
  xMsg := TMessageFactory.New;
  xMsg.Destiny := ADestinatario;
  xMsg.Params.Add('status.verificacao',EmptyStr);
  EnviarMensagem(xMsg);
end;

procedure TSCOMessageClient.DestruirThreadMonitora;
begin
  if Assigned(FMensagemProcessamento) then
  begin
    if FMensagemProcessamento.Started then
    begin
      FMensagemProcessamento.Terminate;
      if not FMensagemProcessamento.Suspended then
      begin
        FMensagemProcessamento.WaitFor;
      end;
    end;
    FMensagemProcessamento.DisposeOf;
    FMensagemProcessamento := nil;
  end;
end;

procedure TSCOMessageClient.DoAfterConectar;
begin
  inherited;
  CriarThreadMonitora;
end;

procedure TSCOMessageClient.DoAfterDesconectar;
begin
  inherited;
  DestruirThreadMonitora;
end;

procedure TSCOMessageClient.EnviarMensagem(AMensagem: IMessage);
begin
  Self.SendToServer(AMensagem);
end;

{ TMensagemProcessamentoCliente }

constructor TSCOClientProcessThread.Create(
  AClienteMensageria: TSCOMessageClient);
begin
  inherited Create(False);
  FMessageClient := AClienteMensageria;
end;

destructor TSCOClientProcessThread.Destroy;
begin
  inherited;
end;

procedure TSCOClientProcessThread.Execute;
var
  xMensagem : IMessage;
  xListener : ISCOMessageListener;
begin
  inherited;
  while not Self.Terminated do
  begin
    try
      if Self.Terminated then Exit;
      if not FMessageClient.MessageDequeue(xMensagem) then
      begin
        if Self.Terminated then Exit;
        // caso não tenha mensagem na pilha, consome 100% do processamento
        // pois estamos em um while infinito
        Sleep(50);
        if Self.Terminated then Exit;
      end
      else
      begin
        if Self.Terminated then Exit;

        // Mensagens de status
        MsgStatus(FMessageClient,xMensagem);

        if Self.Terminated then Exit;

        FMessageClient.MsgEntregar(xMensagem);

        if Self.Terminated then Exit;

        //Varro os listeners registrados
        for xListener in FMessageClient.FListeners do
        begin
          //Crio thread passando o listener e a mensagem
            //Crio uma thread para que eu possa estourar excessões dentro dos listeners
            //Se programar aqui direto, sem criar thread, a excessão gerada pelo listener é "ocultada" pelo except deste procedimento
          TSCOListenerSendThread.Create(xListener, xMensagem);
        end;
        Sleep(100);
      end;
    except on E: Exception do

    end;
  end;
end;

procedure TSCOClientProcessThread.MsgStatus(AClienteMensageria: TSCOMessageClient; AMensagem: IMessage);
begin
  if AMensagem.Params.ContainsKey('status.verificacao') then
  begin
    if AMensagem.Destiny = AClienteMensageria.UserName then
    begin
      AClienteMensageria.StatusOnline(AMensagem.UserName);
    end;
  end;
end;

{ TSCOListenerSendThread }

constructor TSCOListenerSendThread.Create(const AListener: ISCOMessageListener; const AMensagem: IMessage);
begin
  inherited Create(False);
  FreeOnTerminate := True;
  FListener := AListener;
  FMensagem := AMensagem;
  FException := nil;
end;

procedure TSCOListenerSendThread.DoHandleException;
begin
  if Assigned(FException) then
  begin
    // Entregar pra main thread
    //MessageDlg(FException.Message, System.UITypes.TMsgDlgType.mtError, [System.UITypes.TMsgDlgBtn.mbok], 0);
  end;
end;

procedure TSCOListenerSendThread.Execute;
begin
  if Assigned(Listener) then
  begin
    try
      Synchronize(
        Self,
        procedure
        begin
          Listener.DoReceiveMessage(nil, Mensagem);
        end
      );
    except
      HandleException;
    end;
  end;
end;

procedure TSCOListenerSendThread.HandleException;
begin
  FException := Exception(ExceptObject);
  try
    if Assigned(FException) then
    begin
      //Não trato excessões do tipo 'Abort'
      if not (FException is EAbort) then
      begin
        Synchronize(DoHandleException);
      end;
    end;
  finally
    FException := nil;
  end;
end;


end.


