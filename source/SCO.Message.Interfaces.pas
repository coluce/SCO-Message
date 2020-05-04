unit SCO.Message.Interfaces;

interface

uses
  IdContext,
  System.Generics.Collections;

type

  TRouteState = (Active, Error);

  IMessage = interface
    ['{BA2A587C-CFD1-4EB8-9268-97B8A4D87B1E}']
    function GetID: string;
    procedure SetID(const Value: string);
    property ID: string read GetID write SetID;

    function GetUserName: string;
    procedure SetUserName(const Value: string);
    property UserName: string read GetUserName write SetUserName;

    function GetDestiny: string;
    procedure SetDestiny(const Value: string);
    property Destiny: string read GetDestiny write SetDestiny;

    function GetParams: TDictionary<string,string>;
    property Params: TDictionary<string,string> read GetParams;

    procedure FromText(const Value: string);
    function ToText: string;

  end;

  TMessageFactory = class
    class function New: IMessage; overload;
    class function New(const AText: string): IMessage; overload;
  end;

  IRoute = interface
    ['{50A476D0-68C7-48BB-9CEB-7D8752F4F765}']
    function GetIP: string;
    function GetCreationDate: TDateTime;
    function GetState: TRouteState;
    function GetContext: TIdContext;

    procedure RegisterUser(const AUserName: string);
    procedure UnregisterUser(const AUserName: string);
    procedure QueueMessage(const AMessage: IMessage);
    function Users: TArray<string>;
    function HasUser(const AUsername: string): Boolean;
    function IsEmpty: boolean;
    function ID: integer;

    property IP: string read GetIP;
    property CreationDate: TDateTime read GetCreationDate;
    property State: TRouteState read GetState;
    property Context: TIdContext read GetContext;
  end;

  TRouteFactory = class
  public
    class function New(const AContext: TIdContext): IRoute;
  end;

  IRouteController = interface
    ['{AF40ADFB-37DA-49A8-8ECA-1E57088C08B8}']

    procedure RegisterRoute(const AContext: TIdContext; const AUserName: string);
    procedure UnregisterRoute(const AUserName: string); overload;
    procedure UnregisterRoute(AContext: TIdContext); overload;
    procedure EmptyRoutes;
    function QueueMessage(const AMessage: IMessage): boolean;

    //procedure KillRoute(const ARouteID: integer);
    procedure VerifyInactiveRoutes;

    function GetRoutes: TArray<IRoute>;
    property Routes: TArray<IRoute> read GetRoutes;
  end;

  TRouteControllerFacory = class
  public
    class function New: IRouteController;
  end;

implementation

uses
  SCO.Message.Imp,
  SCO.Message.Route,
  SCO.Message.Route.Controller;

{ TRoutefactory }

class function TRouteFactory.New(const AContext: TIdContext): IRoute;
begin
  Result := TRoute.Create(AContext);
end;

{ TRouteControllerFacory }

class function TRouteControllerFacory.New: IRouteController;
begin
  Result := TRouteController.Create;
end;

{ TMessageFactory }

class function TMessageFactory.New: IMessage;
begin
  Result := TMessage.Create;
end;

class function TMessageFactory.New(const AText: string): IMessage;
begin
  Result := New;
  Result.FromText(AText);
end;

end.
