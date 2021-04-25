program main;

{$mode objfpc}{$H+}

uses
  util, SysUtils;

var
  log: TLogger;

begin
  log := TLogger.GetLogger('main');
  log.Info('Hello World !');
  Readln;
end.
