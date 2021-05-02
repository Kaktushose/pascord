unit http;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, fpjson, logging, opensslsockets;

type
  RequestType = (GET, POST, PUT, Delete);

  THttp = class
  private
    url:   string;
    token: string;
    log:   TLogger;
  public
    procedure MakeRequest(aRequestType: RequestType; route: string);
    constructor Create(aToken: string);
    destructor Destroy; override;
  end;

implementation

procedure THttp.MakeRequest(aRequestType: RequestType; route: string);
var
  client: TFPHttpClient;
begin
  client := TFPHTTPClient.Create(nil);
  client.AddHeader('Authorization', 'Bot ' + token);
  client.AddHeader('Content-Type', 'application/json');
  client.RequestBody := TRawByteStringStream.Create('{"content": "Hello Pascord!", "tts": false}');
  try
    try
      WriteLn(client.Post(url + '/channels/533817194266755072/messages'));
    except
      on e: Exception do
        log.Error('Request Failed!', e);
    end;
  finally
    FreeAndNil(client);
  end;
end;

constructor THttp.Create(aToken: string);
begin
  inherited Create;
  url := 'https://discord.com/api/v9';
  token := aToken;
  log := TLogger.GetLogger(THttp);
end;

destructor THttp.Destroy;
begin
  inherited Destroy;
end;

end.
