program ProjectServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Server.View.Principal in 'Server.View.Principal.pas' {ServerPrincipal};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServerPrincipal, ServerPrincipal);
  Application.Run;
end.
