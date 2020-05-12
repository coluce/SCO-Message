unit SCO.Message.Route;

interface

uses
  System.Classes,
  System.Generics.Collections,
  IdContext,
  SCO.Message.Interfaces,
  System.SyncObjs;

type

  TThreadSendMessage = class(TThread)
  private
    FContext: TIdContext;
    FQueue: TQueue<string>;
    FSec : TCriticalSection;
    FState: TRouteState;
  protected
    procedure Execute; override;
  public
    constructor Create(const AContext: TIdContext);
    destructor Destroy; override;
    procedure Enqueue(const AText: string);
    property State: TRouteState read FState;
  end;

  TRoute = class(TInterfacedObject, IRoute)
  private
    FSendMessage: TThreadSendMessage;
    FCreationDate: TDateTime;
    FContext: TIdContext;
    FUsers: TList<string>;
    function GetIP: string;
    function GetCreationDate: TDateTime;
    function GetState: TRouteState;
    function GetContext: TIdContext;
  public
    constructor Create(AContext: TIdContext);
    destructor Destroy; override;
    procedure RegisterUser(const AUserName: string);
    procedure UnregisterUser(const AUserName: string);
    procedure QueueMessage(const AMessage: IMessage);
    function HasUser(const AUsername: string): Boolean;
    function IsEmpty: boolean;
    function Users: TArray<string>;
    function ID: integer;
  //published
    property IP: string read GetIP;
    property CreationDate: TDateTime read GetCreationDate;
    property State: TRouteState read GetState;
    property Context: TIdContext read GetContext;
  end;

implementation

uses
  System.SysUtils, IdGlobal;

{ TRoute }

constructor TRoute.Create(AContext: TIdContext);
begin
  FCreationDate := Now;
  FContext := AContext;
  FUsers := TList<string>.Create;
  FSendMessage := TThreadSendMessage.Create(FContext);
  FSendMessage.Start;
end;

destructor TRoute.Destroy;
begin
  FUsers.Clear;
  FUsers.TrimExcess;
  FUsers.DisposeOf;

  FSendMessage.Terminate;
  if not FSendMessage.Terminated then
  begin
    FSendMessage.WaitFor;
  end;
  FSendMessage.DisposeOf;

  inherited;
end;

function TRoute.GetContext: TIdContext;
begin
  Result := FContext;
end;

function TRoute.GetCreationDate: TDateTime;
begin
  Result := FCreationDate;
end;

function TRoute.GetIP: string;
begin
  if Assigned(FContext) then
  begin
    if Assigned(FContext.Connection) then
    begin
      Result := FContext.Connection.Socket.Binding.PeerIP;
    end
    else
    begin
      { TODO : desconectado, marcar para destruir rota }
    end;
  end;
end;

function TRoute.GetState: TRouteState;
begin
  Result := TRouteState.Error;
  if Assigned(FSendMessage) then
  begin
    Result := FSendMessage.State;
  end;
end;

function TRoute.HasUser(const AUsername: string): Boolean;
begin
  Result := FUsers.Contains(AUsername);
end;

function TRoute.ID: integer;
begin
  Result := -1;
  if Assigned(FContext) then
  begin
    if Assigned(FContext.Connection) then
    begin
      Result := FContext.Connection.IOHandler.GetHashCode;
    end
    else
    begin
      { TODO : desconectado, marcar para destruir rota }
    end;
  end;
end;

function TRoute.IsEmpty: boolean;
begin
  Result := FUsers.Count = 0;
end;

procedure TRoute.QueueMessage(const AMessage: IMessage);
begin
  FSendMessage.Enqueue(AMessage.ToText);
end;

procedure TRoute.RegisterUser(const AUserName: string);
begin
  if not FUsers.Contains(AUserName) then
  begin
    FUsers.Add(AUserName);
  end;
end;

procedure TRoute.UnregisterUser(const AUserName: string);
begin
  if FUsers.Contains(AUserName) then
  begin
    FUsers.Remove(AUserName);
  end;
  FUsers.TrimExcess;
end;

function TRoute.Users: TArray<string>;
begin
  Result := FUsers.ToArray;
end;

{ TThreadSendMessage }

constructor TThreadSendMessage.Create(const AContext: TIdContext);
begin
  inherited Create(True);
  FreeOnTerminate := False;
  FContext := AContext;
  FQueue := TQueue<string>.Create;
  FSec := TCriticalSection.Create;
  FState := TRouteState.Active;
end;

destructor TThreadSendMessage.Destroy;
begin
  FQueue.Clear;
  FQueue.TrimExcess;
  FQueue.DisposeOf;

  FSec.DisposeOf;

  inherited;
end;

procedure TThreadSendMessage.Enqueue(const AText: string);
begin
  FSec.Enter;
  try
    FQueue.Enqueue(AText);
  finally
    FSec.Leave;
  end;
end;

procedure TThreadSendMessage.Execute;
  function GetText: string;
  begin
    Result := EmptyStr;
    FSec.Enter;
    try
      if FQueue.Count > 0 then
      begin
        Result := FQueue.Dequeue;
        FQueue.TrimExcess;
      end;
    finally
      FSec.Leave;
    end;
  end;
var
  vBuf: TIdBytes;
  vText: string;
begin
  inherited;
  while not Terminated do
  begin
    vText := GetText;
    if vText = EmptyStr then
    begin
      sleep(250);
    end else begin
      sleep(5);
      try
        if Assigned(FContext.Connection) then
        begin
          vBuf := IndyTextEncoding_UTF8.GetBytes(vText);
          FContext.Connection.Socket.Write(Int32(Length(vBuf)));
          FContext.Connection.Socket.Write(vBuf);
        end;
      except
        FState := TRouteState.Error;
        sleep(500);
      end;
    end;
  end;
end;

end.
