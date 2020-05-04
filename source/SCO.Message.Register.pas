unit SCO.Message.Register;

interface

uses
  System.Classes;

procedure Register;

implementation

uses
  SCO.Message.Client,
  SCO.Message.Server;

procedure Register;
begin
  RegisterComponents(
    'SCO Message',
    [
      TMensageriaCliente,
      TMensageriaServidor
    ]
  );
end;

end.
