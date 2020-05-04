unit SCO.Message.Zip;

interface

uses
  System.Classes,
  System.SysUtils,
  System.Zip,
  System.ZLib;

type

  { TMyZipStream }

  TMyZipStream = class(TCollectionItem)
  private
    { Private declarations }
    FFileName: string;
    FStream: TStream;
  public
    { Public declarations }
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Stream   : TStream read FStream   write FStream;
    property FileName : string  read FFileName write FFileName;
  end;

  { TMyZipStreams }

  TMyZipStreams = class(TCollection)
  private
    { Private declarations }
    function GetItem(AIndex: Integer): TMyZipStream;
    procedure SetItem(AIndex: Integer; const AValue: TMyZipStream);
  public
    { Public declarations }
    constructor Create;
    function Add(AStream : TStream; AFileName : string) : TMyZipStream;
    property Items[pIndex: Integer]: TMyZipStream read GetItem write SetItem; default;
  end;

  { Métodos }

  procedure ZipStream(AInputStream, AOutputStream : TStream);
  procedure UnzipStream(AInputStream, AOutputStream : TStream);

  procedure ZipFile(AInputFile, AOutputFile : string);
  procedure UnizipFile(AInputFile, AOutputFile : string);

  function  ZipFiles(AInputFiles : TStringList; AOutputFile : string) : Boolean; overload;
  function  UnzipFiles(AInputFile, AOutputDir : string) : Boolean; overload;

  function  ZipFiles(AZipStreams : TMyZipStreams) : TMemoryStream; overload;
  function  UnzipFiles(AFileStream : TMemoryStream; AOutputDir : string) : Boolean; overload;

  function EncodeText(ATextToEncode: string): string;
  function DecodeText(ATextToDecode: string): string;

implementation

{ TZipStream }

constructor TMyZipStream.Create(Collection: TCollection);
begin
  inherited;
  FStream := nil;
  FFileName := EmptyStr;
end;

destructor TMyZipStream.Destroy;
begin
  if Assigned(FStream) then
  begin
    FStream.DisposeOf;
    FStream := nil;
  end;
  inherited;
end;

{ TZipStreams }

constructor TMyZipStreams.Create;
begin
  inherited Create(TMyZipStream);
end;

function TMyZipStreams.Add(AStream: TStream; AFileName: string): TMyZipStream;
begin
  Result          := TMyZipStream(inherited Add);
  Result.FileName := AFileName;
  Result.Stream   := AStream;
end;

function TMyZipStreams.GetItem(AIndex: Integer): TMyZipStream;
begin
  Result := TMyZipStream(inherited GetItem(AIndex));
end;

procedure TMyZipStreams.SetItem(AIndex: Integer; const AValue: TMyZipStream);
begin
  inherited SetItem(AIndex, AValue);
end;

{ TZip }

procedure ZipStream(AInputStream, AOutputStream : TStream);
var
   xZip: TZCompressionStream;
begin
  AInputStream.Seek(0, soFromBeginning);
  xZip := TZCompressionStream.Create(clMax, AOutputStream);
  try
    xZip.CopyFrom(AInputStream, AInputStream.Size);
    AOutputStream.Seek(0, soFromBeginning);
  finally
    xZip.DisposeOf;
  end;
end;

procedure UnzipStream(AInputStream, AOutputStream : TStream);
var
  xUnZip: TZDecompressionStream;
begin
  AInputStream.Seek(0, soFromBeginning);
  xUnZip := TZDecompressionStream.Create(AInputStream);
  try
    AOutputStream.CopyFrom(xUnZip, 0);
    AOutputStream.Seek(0, soFromBeginning);
  finally
    xUnZip.DisposeOf;
  end;
end;

procedure ZipFile(AInputFile, AOutputFile : string);
var
  xInputStream : TMemoryStream;
  xOutputStream : TMemoryStream;
begin
  xInputStream  := TMemoryStream.Create;
  try
    xOutputStream := TMemoryStream.Create;
    try
      xInputStream.LoadFromFile(AInputFile);
      ZipStream(xInputStream, xOutputStream);
      xOutputStream.SaveToFile(AOutputFile);
    finally
      xOutputStream.DisposeOf;
    end;
  finally
    xInputStream.DisposeOf;
  end;
end;

procedure UnizipFile(AInputFile, AOutputFile : string);
var
  xInputStream : TMemoryStream;
  xOutputStream : TMemoryStream;
begin
  xInputStream  := TMemoryStream.Create;
  try
    xOutputStream := TMemoryStream.Create;
    try
      xInputStream.LoadFromFile(AInputFile);
      UnzipStream(xInputStream, xOutputStream);
      xOutputStream.SaveToFile(AOutputFile);
    finally
      xOutputStream.DisposeOf;
    end;
  finally
    xInputStream.DisposeOf;
  end;
end;

function ZipFiles(AInputFiles : TStringList; AOutputFile : string) : Boolean;
var
  I       : integer;
  xZipper : TZipFile;
begin
  xZipper := TZipFile.Create;
  try
    xZipper.Open(AOutputFile, zmWrite);
    for i := 0 to Pred(AInputFiles.Count) do
    begin
      if FileExists(AInputFiles[i]) then
      begin
        xZipper.Add(AInputFiles[i]);
      end;
    end;
    xZipper.Close;
  finally
    xZipper.DisposeOf;
  end;
  Result := True;
end;

function UnzipFiles(AInputFile, AOutputDir : string) : Boolean;
var
  xUnZipper: TZipFile;
begin
  xUnZipper := TZipFile.Create;
  try
    xUnZipper.Open(AInputFile, zmRead);
    xUnZipper.ExtractAll(AOutputDir);
    xUnZipper.Close;
  finally
    xUnZipper.DisposeOf;
  end;
  Result := True;
end;

function ZipFiles(AZipStreams : TMyZipStreams) : TMemoryStream; overload;
var
   xZipper : TZipFile;
   i : integer;
begin
  Result := TMemoryStream.Create;

  if not Assigned(AZipStreams) then
  begin
    Exit;
  end;

  if (AZipStreams.Count <= 0) then
  begin
    Exit;
  end;

  xZipper := TZipFile.Create;
  try
    xZipper.Open(Result, zmWrite);
    for i := 0 to Pred(AZipStreams.Count) do
    begin
      xZipper.Add(AZipStreams[i].Stream, AZipStreams[i].FileName);
    end;
    xZipper.Close;
  finally
    xZipper.DisposeOf;
  end;
end;

function UnzipFiles(AFileStream : TMemoryStream; AOutputDir : string): Boolean;
var
  xUnZipper: TZipFile;
begin
  xUnZipper := TZipFile.Create;
  try
    xUnZipper.Open(AFileStream, zmRead);
    xUnZipper.ExtractAll(AOutputDir);
    xUnZipper.Close;
  finally
    xUnZipper.DisposeOf;
  end;
  Result := True;
end;

function EncodeText(ATextToEncode: string): string;
var
  vInput,
  vOutput: TStringStream;
begin
  vInput := TStringStream.Create(ATextToEncode);
  try
    vOutput := TStringStream.Create;
    try
      ZipStream(vInput, vOutput);
      Result := vOutput.DataString;
    finally
      FreeAndNil(vOutput);
    end;
  finally
    FreeAndNil(vInput);
  end;
end;

function DecodeText(ATextToDecode: string): string;
var
  vInput,
  vOutput: TStringStream;
begin
  vInput := TStringStream.Create(ATextToDecode);
  try
    vOutput := TStringStream.Create;
    try
      UnzipStream(vInput, vOutput);
      Result := vOutput.DataString;
    finally
      FreeAndNil(vOutput);
    end;
  finally
    FreeAndNil(vInput);
  end;
end;

end.

