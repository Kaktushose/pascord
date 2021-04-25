unit util;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  LoggingLevel = (DEBUG, INFO, WARN, ERROR);

  { TLogger }

  TLogger = class
  private
    Name: string;
    level: LoggingLevel; static;
    procedure Print(message: string; loggingLevel: LoggingLevel);
  public
    class procedure SetLevel(loggingLevel: LoggingLevel);
    procedure Debug(message: string);
    procedure Info(message: string);
    procedure Warn(message: string);
    procedure Error(message: string);
    procedure Error(message: string; e: Exception);
    procedure Error(e: Exception);
    function CreateStackTrace(e: Exception): string;
    constructor GetLogger(clazz: TClass);
    constructor GetLogger(loggerName: string);
    destructor Destroy; override;
  end;

implementation

class procedure TLogger.SetLevel(loggingLevel: LoggingLevel);
begin
  level := loggingLevel;
end;

procedure TLogger.Debug(message: string);
begin
  Print(message, LoggingLevel.DEBUG);
end;

procedure TLogger.Info(message: string);
begin
  Print(message, LoggingLevel.INFO);
end;

procedure TLogger.Warn(message: string);
begin
  Print(message, LoggingLevel.WARN);
end;

procedure TLogger.Error(message: string);
begin
  Print(message, LoggingLevel.ERROR);
end;

procedure TLogger.Error(message: string; e: Exception);
begin
  if e = nil then
  begin
    Error(message);
    Exit;
  end;
  Print(message + sLineBreak + CreateStackTrace(e) , LoggingLevel.ERROR);
end;

procedure TLogger.Error(e: Exception);
begin
  if e = nil then
  begin
    Error('nil');
    Exit;
  end;
  Print(CreateStackTrace(e), LoggingLevel.ERROR);
end;

procedure TLogger.Print(message: string; loggingLevel: LoggingLevel);
var
  levelString, fmt: string;
begin
  if loggingLevel < level then
    Exit;

  WriteStr(levelString, loggingLevel);
  fmt := '%s [%s] %-5s %s';
  WriteLn(Format(fmt, [TimeToStr(Now), Name, LevelString, message]));
end;

function TLogger.CreateStackTrace(e: Exception): string;
begin
    Result := e.ClassName + ': ' + e.Message + sLineBreak + BackTraceStrFunc(e);
end;

constructor TLogger.GetLogger(clazz: TClass);
begin
  inherited Create;
  Name := clazz.ClassName;
end;

constructor TLogger.GetLogger(loggerName: string);
begin
  inherited Create;
  Name := loggerName;
end;

destructor TLogger.Destroy;
begin
  inherited Destroy;
end;

end.
