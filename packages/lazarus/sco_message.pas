{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit sco_message;

{$warn 5023 off : no warning about unused units}
interface

uses
  SCO.Message.Base, SCO.Message.Client, SCO.Message.Functions, 
  SCO.Message.Imp, SCO.Message.Interfaces, SCO.Message.Register, 
  SCO.Message.Route.Controller, SCO.Message.Route, SCO.Message.Server, 
  SCO.Message.Zip, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('SCO.Message.Register', @SCO.Message.Register.Register);
end;

initialization
  RegisterPackage('sco_message', @Register);
end.
