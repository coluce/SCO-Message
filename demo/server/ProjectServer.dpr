program ProjectServer;

uses
  System.StartUpCopy,
  FMX.Forms,
  Server.View.Principal in 'Server.View.Principal.pas' {ServerPrincipal},
  Jsons in 'json4delphi\Jsons.pas',
  JsonsUtilsEx in 'json4delphi\JsonsUtilsEx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TServerPrincipal, ServerPrincipal);
  Application.Run;
end.
