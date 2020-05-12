unit SCO.Message.Imp;

interface

uses
  System.Classes,
  System.Generics.Collections,
  SCO.Message.Functions,
  SCO.Message.Interfaces;

type
  TMessage = class(TInterfacedObject, IMessage)
  private
    FID: string;
    FUserName: string;
    FDestiny: string;
    FParams: TDictionary<string,string>;

    function GetID: string;
    procedure SetID(const Value: string);

    function GetUserName: string;
    procedure SetUserName(const Value: string);

    function GetDestiny: string;
    procedure SetDestiny(const Value: string);

    function GetParams: TDictionary<string,string>;

  public
    constructor Create;
    destructor Destroy; override;
    procedure FromText(const Value: string);
    function ToText: string;
  //published
    property ID: string read GetID write SetID;
    property UserName: string read GetUserName write SetUserName;
    property Destiny: string read GetDestiny write SetDestiny;
    property Params: TDictionary<string,string> read GetParams;
  end;

implementation

uses
  System.SysUtils,
  Soap.EncdDecd,
  SCO.Message.Zip;

const
  cstCaracterSeparador: string = '¦';

{ TMessage }

constructor TMessage.Create;
begin
  FParams := TDictionary<string,string>.Create;
  FID := TMyGuid.New;
end;

destructor TMessage.Destroy;
begin
  FParams.Clear;
  FParams.DisposeOf;
  FParams := nil;
  inherited;
end;

function TMessage.GetDestiny: string;
begin
  Result := FDestiny;
end;

function TMessage.GetID: string;
begin
  Result  := FID;
end;

function TMessage.GetParams: TDictionary<string, string>;
begin
  Result := FParams;
end;

function TMessage.GetUserName: string;
begin
  Result := FUserName;
end;

procedure TMessage.SetDestiny(const Value: string);
begin
  FDestiny := Value;
end;

procedure TMessage.SetID(const Value: string);
begin
  FID := Value;
end;

procedure TMessage.SetUserName(const Value: string);
begin
  FUserName := Value;
end;

function TMessage.ToText: string;
  function Convert(const AText: string): string;
  begin
    Result := EncodeString(AText);
  end;
  function ConvertParams: string;
  var
    vPair: TPair<string,string>;
  begin
    Result := EmptyStr;
    for vPair in FParams do
    begin
      Result := Result + Convert(vPair.Key) + ':' + Convert(vPair.Value) + '|';
    end;
  end;
begin
  Result :=
    '0' + cstCaracterSeparador +
    Convert(FID) + cstCaracterSeparador +
    Convert(FUserName) + cstCaracterSeparador +
    Convert(FDestiny) + cstCaracterSeparador +
    ConvertParams;
  Result := EncodeText(Result);
end;

procedure TMessage.FromText(const Value: string);
  function Revert(const AText: string): string;
  begin
    Result := DecodeString(AText);
  end;
  procedure RevertParams(const AText: string);
  var
    vArrayParams: TArray<string>;
    vItem: string;
    vPair: TArray<string>;
    vKey: string;
    vValue: string;
  begin
    FParams.Clear;
    vArrayParams := AText.Split(['|']);
    for vItem in vArrayParams do
    begin
      vPair := vItem.Split([':']);

      if Length(vPair) > 0 then
      begin
        vKey := Revert(vPair[0]);
        vValue := Revert(vPair[1]);

        FParams.AddOrSetValue(vKey,vValue);
      end;
    end;
  end;
var
  vArray: TArray<string>;
  vDecodedText: string;
begin
  vDecodedText := DecodeText(Value);
  vArray := vDecodedText.Split([cstCaracterSeparador]);
  if Length(vArray) > 0 then
  begin
    Self.FID       := Revert(vArray[1]);
    Self.FUserName := Revert(vArray[2]);
    Self.FDestiny  := Revert(vArray[3]);
    RevertParams(vArray[4]);
  end;
end;

end.
