unit SCO.Message.Functions;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  Generics.Collections,
  DBXJSon,
  Data.DBXJSONReflect,
  System.JSON,
  FireDAC.Comp.Client,
  Data.DB,
  System.DateUtils,
  Soap.EncdDecd,
  FireDAC.Comp.DataSet;

const
  cstChaveGuid : string = '0123456789ABCDEFGHIJKLMNOPQRSTUVXZ';

type

  ECrypType = (tcEncrypt, tcDecrypt);

  TMyGuid = class
  public
    class function New : string;
  end;

  TCompressao = class
  public
    class function Descriptografar(const Conteudo: String) : string;
    class function Criptografar(const Conteudo: String) : string;
  end;

  { Funcoes }

  function SearchReplace(AString, ASearchFor, AReplaceFor: String): string;
  function Utf8StringToString(AString : string) : string;
  function StringToUtf8String(AString : string) : string;
  function ClearString(AString: string): string;
  function SplitString(const ASeparator : char; const AText : string) : TStringList;

  procedure RefreshObjectInspector(const AComponent : TComponent);

implementation

uses
  FMX.Forms,
  FMX.Graphics,
  FireDAC.Stan.Intf,
  SCO.Message.Zip,
  System.Rtti;

const
  cstArrayUTF8Chars   : Array [0..52] of string = ('\u00e1','\u00e0','\u00e2','\u00e3','\u00e4','\u00c1','\u00c0','\u00c2','\u00c3','\u00c4','\u00e9','\u00e8','\u00ea','\u00ea',
                                                   '\u00c9','\u00c8','\u00ca','\u00cb','\u00ed','\u00ec','\u00ee','\u00ef','\u00cd','\u00cc','\u00ce','\u00cf','\u00f3','\u00f2',
                                                   '\u00f4','\u00f5','\u00f6','\u00d3','\u00d2','\u00d4','\u00d5','\u00d6','\u00fa','\u00f9','\u00fb','\u00fc','\u00da','\u00d9',
                                                   '\u00db','\u00e7','\u00c7','\u00f1','\u00d1','\u0026','\u0027', '\u003c', '\u003e', '\u003d', '\u0026');

  cstArrayUtf8Symbols : Array [0..52] of string = ('á','à','â','ã','ä','Á','À','Â','Ã','Ä','é','è','ê','ê','É','È','Ê','Ë','í','ì','î','ï','Í','Ì','Î','Ï','ó','ò','ô','õ','ö','Ó',
                                                   'Ò','Ô','Õ','Ö','ú','ù','û','ü','Ú','Ù','Û','ç','Ç','ñ','Ñ','&','''', '<', '>', '=', '&');

{ TMyGuid }

class function TMyGuid.New: string;
var
  xNumero  : Int64;
  xPosicao : integer;
  xAux     : string;
begin
  Result := EmptyStr;
  sleep(1);
  xNumero :=  Trunc(((Now - 41000) * 1000000000));
  while xNumero > 34 do
  begin
    xPosicao := xNumero mod 34 ;
    xNumero  := Trunc(xNumero/34);
    xAux     := cstChaveGuid[xPosicao+1];
    Result   := xAux + Result;
  end;
  while Length(Result) < 15 do
  begin
     Result := Result + cstChaveGuid[Random(34)+1];
  end;
end;

{ Funcoes }

function SearchReplace(AString, ASearchFor, AReplaceFor: String): string;
var
  i : integer;
begin
  Result := EmptyStr;
  while (Pos(ASearchFor, AString) > 0) do
  begin
    i := Pos(ASearchFor ,AString);
    Result := Result + Copy(AString, 1, i-1) + AReplaceFor;
    Delete(AString, 1, Pred(Length(ASearchFor)) + i);
  end;
  Result := Result + AString;
end;

function StringToUtf8String(AString : string) : string;
var
  i : integer;
begin
  Result := AString;
  for i := Low(cstArrayUtf8Symbols) to High(cstArrayUtf8Symbols) do
  begin
    Result := SearchReplace(Result, cstArrayUtf8Symbols[i], cstArrayUTF8Chars[i]);
  end;
end;

function Utf8StringToString(AString : string) : string;
var
  i : integer;
begin
  Result := AString;
  for i := Low(cstArrayUtf8Symbols) to High(cstArrayUtf8Symbols) do
  begin
    Result := SearchReplace(Result, cstArrayUTF8Chars[i], cstArrayUtf8Symbols[i]);
  end;
end;

function ClearString(AString : string) : string;
begin
  Result := StringReplace(AString, #$D#$A, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #13#10, ' ', [rfReplaceAll]);
  Result := StringReplace(Result, #10, '', [rfReplaceAll]);
end;

function SplitString(const ASeparator : char; const AText : string) : TStringList;
begin
   Result := TStringList.Create;
   ExtractStrings([ASeparator], [' '], pChar(AText), Result);
end;

{ TCompressao }

class function TCompressao.Criptografar(const Conteudo: String): string;
var
  xStreamOrigem,
  xStreamCompactado : TStringStream;
begin
  Result := EmptyStr;
  xStreamOrigem := TStringStream.Create(Conteudo);
  try
    xStreamOrigem.Seek(0, soFromBeginning);

    xStreamCompactado := TStringStream.Create;
    try
      ZipStream(xStreamOrigem, xStreamCompactado);
      Result := xStreamCompactado.DataString;
    finally
      xStreamCompactado.DisposeOf;
    end;
  finally
    xStreamOrigem.DisposeOf;
  end;
end;

class function TCompressao.Descriptografar(const Conteudo: String): string;
var
  xStreamCompactado,
  xStreamDestino : TStringStream;
begin
  Result := EmptyStr;
  xStreamCompactado := TStringStream.Create(Conteudo);
  try
    xStreamCompactado.Seek(0, soFromBeginning);

    xStreamDestino := TStringStream.Create;
    try
      UnzipStream(xStreamCompactado, xStreamDestino);
      Result := xStreamDestino.DataString;
    finally
      xStreamDestino.DisposeOf;
    end;
  finally
    xStreamCompactado.DisposeOf;
  end;
end;

procedure RefreshObjectInspector(const AComponent : TComponent);
{$IFDEF MSWINDOWS}
var
  xForm : TForm;
{$ENDIF}
begin
{$IFDEF MSWINDOWS}
  if Assigned(AComponent) then
  begin
    if (csDesigning in AComponent.ComponentState) then
    begin
      if Assigned(AComponent.Owner) then
      begin
        if (AComponent.Owner is TForm) then
        begin
          xForm := TForm(AComponent.Owner);
          if Assigned(xForm) then
          begin
            if Assigned(xForm.Designer) then
            begin
              xForm.Designer.Modified;
            end;
          end;
        end;
      end;
    end;
  end;
{$ENDIF}
end;


end.

