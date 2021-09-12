{ This unit is used to send http requests to the discord api. 
  @author(Kaktushose (https://github.com/Kaktushose)) }

unit http;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fphttpclient, opensslsockets, fpjson, jsonparser, logging;

type
  { @abstract(An enum describing all available http request methods.)}
  RequestType = (GET, POST, PUT, DELETE);

  { @abstract(This class describes a http request. Instances of this class can be reused.)}
  THttpRequest = class
  private
    FMethod : RequestType;
    FRoute: string;
    FBody: string;
    procedure setRequestType(aRequestType : RequestType);
    procedure setRoute(aRoute : string);
    procedure setBody(aBody : string);
  public
    { The request method the use. }
    property Method: RequestType read FMethod write setRequestType;
    { The route of the request. }
    property Route: string read FRoute write setRoute;
    { An optional body to attach to the request. }
    property Body: string read FBody write setBody;
  end;

  { @abstract(A simple http client that can make requests to the discord api. }
  THttpClient = class
  private
    url:   string;
    token: string;
    log:   TLogger;
  public
    {Send a @link(THttpRequest) to the discord api. }
    function MakeRequest(aHttpRequest : THttpRequest) : TJSONObject; 
    { Create a new client with the given bot token. }
    constructor Create(aToken: string);
    { The destructor. }
    destructor Destroy; override;
  end;

implementation

function THttpClient.MakeRequest(aHttpRequest : THttpRequest) : TJSONObject;
var
  requestUrl, method : string;
  client: TFPHttpClient;
  json : TJSONData;
begin
  client := TFPHTTPClient.Create(nil);
  client.AddHeader('Authorization', 'Bot ' + token);
  client.AddHeader('Content-Type', 'application/json');
  client.RequestBody := TRawByteStringStream.Create(aHttpRequest.Body);

  WriteStr(method, aHttpRequest.Method);
  requestUrl := url + aHttpRequest.Route;
  log.Debug('Request: ' + method + ' ' + requestUrl);
  try
    try
      case aHttpRequest.Method of
        RequestType.GET:
          json := GetJson(client.Get(requestUrl));
        RequestType.POST:
          json := GetJson(client.Post(requestUrl));
        RequestType.PUT:
          json := GetJson(client.Put(requestUrl));
        RequestType.DELETE:
          json := GetJson(client.Delete(requestUrl));
      end; 
      log.Debug('Response code: ' + IntToStr(client.ResponseStatusCode));
      Result := TJSONObject(json);
    except
      on e: Exception do
        log.Error('Request failed!', e);
    end;
  finally
    FreeAndNil(client);
  end;
end;

constructor THttpClient.Create(aToken: string);
begin
  inherited Create;
  url := 'https://discord.com/api/v9/';
  token := aToken;
  log := TLogger.GetLogger(THttpClient);
end;

destructor THttpClient.Destroy;
begin
  FreeAndNil(log);
  inherited Destroy;
end;

procedure THttpRequest.setRequestType(aRequestType: RequestType);
begin
  FMethod := aRequestType;
end;

procedure THttpRequest.setRoute(aRoute: string);
begin
  FRoute := aRoute;
end;

procedure THttpRequest.setBody(aBody: string);
begin
  FBody := aBody;
end;

end.
