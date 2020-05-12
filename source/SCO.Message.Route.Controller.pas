unit SCO.Message.Route.Controller;

interface

uses
  System.Classes,
  System.Generics.Collections,
  IdContext,
  SCO.Message.Interfaces,
  System.SyncObjs;

type
  TRouteController = class(TInterfacedObject, IRouteController)
  strict private
    FRoutes: TDictionary<integer,IRoute>;
    FSec: TCriticalSection;
    function GetRoutes: TArray<IRoute>;
    procedure KillRoute(const ARouteID: integer);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RegisterRoute(const AContext: TIdContext; const AUserName: string);
    procedure UnregisterRoute(const AUserName: string); overload;
    procedure UnregisterRoute(AContext: TIdContext); overload;
    procedure VerifyInactiveRoutes;
    procedure EmptyRoutes;
    function QueueMessage(const AMessage: IMessage): boolean;
  //published
    property Routes: TArray<IRoute> read GetRoutes;
  end;

implementation

{ TRouteController }

constructor TRouteController.Create;
begin
  FRoutes := TDictionary<integer,IRoute>.Create;
  FSec := TCriticalSection.Create;
end;

destructor TRouteController.Destroy;
begin
  FRoutes.Clear;
  FRoutes.DisposeOf;

  FSec.DisposeOf;

  inherited;
end;

procedure TRouteController.EmptyRoutes;
var
  vRoute: IRoute;
begin
  FSec.Enter;
  try
    for vRoute in FRoutes.Values do
    begin
      if Assigned(vRoute.Context) then
      begin
        if Assigned(vRoute.Context.Connection) then
        begin
          vRoute.Context.Connection.Disconnect;
        end;
      end;
    end;
    FRoutes.Clear;
    FRoutes.TrimExcess;
  finally
    FSec.Leave;
  end;
end;

function TRouteController.GetRoutes: TArray<IRoute>;
begin
  Result := FRoutes.Values.ToArray;
end;

procedure TRouteController.KillRoute(const ARouteID: integer);
begin
  FSec.Enter;
  try
    FRoutes.Remove(ARouteID);
  finally
    FSec.Leave;
  end;
end;

function TRouteController.QueueMessage(const AMessage: IMessage): boolean;
var
  vRoute: IRoute;
begin
  Result := False;
  FSec.Enter;
  try
    for vRoute in FRoutes.Values do
    begin
      if vRoute.HasUser(AMessage.Destiny) then
      begin
        vRoute.QueueMessage(AMessage);
        Result := True;
        Exit;
      end;
    end;
  finally
    FSec.Leave;
  end;
end;

procedure TRouteController.RegisterRoute(const AContext: TIdContext; const AUserName: string);
var
  vID: integer;
  vRoute: IRoute;
begin
  vID := AContext.Connection.IOHandler.GetHashCode;
  FSec.Enter;
  try
    if not FRoutes.TryGetValue(vID, vRoute) then
    begin
      vRoute := TRouteFactory.New(AContext);
      FRoutes.Add(vID, vRoute);
    end;
    vRoute.RegisterUser(AUserName);
  finally
    FSec.Leave;
  end;
end;

procedure TRouteController.UnregisterRoute(AContext: TIdContext);
var
  vRoute: IRoute;
  vKey: integer;
begin
  vKey := 0;
  FSec.Enter;
  try
    for vRoute in FRoutes.Values do
    begin
      if vRoute.Context = AContext then
      begin
        vKey := vRoute.ID;
        Break;
      end;
    end;
    if vKey > 0 then
    begin
      FRoutes.Remove(vKey);
    end;
  finally
    FSec.Leave;
  end;
end;

procedure TRouteController.UnregisterRoute(const AUserName: string);
var
  vRoute: IRoute;
begin
  FSec.Enter;
  try
    for vRoute in FRoutes.Values do
    begin
      if vRoute.HasUser(AUserName) then
      begin
        vRoute.UnregisterUser(AUserName);
        Exit;
      end;
    end;
  finally
    FSec.Leave;
  end;
end;

procedure TRouteController.VerifyInactiveRoutes;
var
  i: integer;
  vArray: TArray<IRoute>;
begin
  FSec.Enter;
  try
    vArray := FRoutes.Values.ToArray;
    for i := Length(vArray) - 1 downto 0 do
    begin
      if
        (vArray[i].State = TRouteState.Error) or
        (vArray[i].IsEmpty)
      then
      begin
        KillRoute(vArray[i].ID);
      end;
    end;
    FRoutes.TrimExcess;
  finally
    FSec.Leave;
  end;
end;

end.
