program main;

{$mode objfpc}{$H+}

uses
  logging, SysUtils;

var
  log: TLogger;

begin
  log := TLogger.GetLogger('main');
  log.Info('Hello World!');
  Readln;
end.
