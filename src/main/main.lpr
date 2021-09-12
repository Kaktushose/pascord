program main;

{$mode objfpc}{$H+}

uses
  http, SysUtils;

var
  client: THttpClient;
  request : THttpRequest;
begin
  client := THttpClient.Create('token');

  request := THttpRequest.Create;
  request.Method := RequestType.GET;
  request.Route := 'channels/545967082253189121';
  
  WriteLn(client.MakeRequest(request).Get('id'));
end.
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
