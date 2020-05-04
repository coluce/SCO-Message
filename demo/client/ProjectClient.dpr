program ProjectClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  Client.View.Main in 'Client.View.Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
