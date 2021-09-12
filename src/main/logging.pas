{ This unit provides a basic implementation of logging. 
  @author(Kaktushose (https://github.com/Kaktushose)) }
unit logging;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { @abstract(An enum describing all available logging levels.) }
  LoggingLevel = (DEBUG, INFO, WARN, ERROR);

  { @abstract(This class provides the actual logging functionality.) 
  Use @link(TLogger.SetLevel) to set the logging level. }
  TLogger = class
  private
    name: string;
    level: LoggingLevel; static;
    procedure Print(message: string; loggingLevel: LoggingLevel);
    function CreateStackTrace(e: Exception): string;
  public
    { Update the global logging level. }
    class procedure SetLevel(loggingLevel: LoggingLevel);
    { Log a debug message. }
    procedure Debug(message: string);
    { Log an info message. }
    procedure Info(message: string);
    { Log a warn message. }
    procedure Warn(message: string);
    { Log a error message. }
    procedure Error(message: string);
    { Log a error message and an exception. }
    procedure Error(message: string; e: Exception);
    { Log an exception. }
    procedure Error(e: Exception);
    { Create a new logger named after a class. }
    constructor GetLogger(clazz: TClass);
    { Create a new logger with a custom name. }
    constructor GetLogger(loggerName: string);
    { The destructor. }
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
  WriteLn(Format(fmt, [TimeToStr(Now), name, LevelString, message]));
end;

function TLogger.CreateStackTrace(e: Exception): string;
begin
    Result := e.ClassName + ': ' + e.Message + sLineBreak + BackTraceStrFunc(e);
end;

constructor TLogger.GetLogger(clazz: TClass);
begin
  inherited Create;
  name := clazz.ClassName;
end;

constructor TLogger.GetLogger(loggerName: string);
begin
  inherited Create;
  name := loggerName;
end;

destructor TLogger.Destroy;
begin
  inherited Destroy;
end;

end.
