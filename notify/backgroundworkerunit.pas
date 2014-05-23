unit BackgroundWorkerUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

Type
  TDoWorkEventArgs = record
    Result : Boolean;
    Argument : string;
    Message : string;
    Target : TObject;
  end;

  TRunWorkerCompletedEventArgs = record
    Result : Boolean;
    Message : string;
    Target : TObject;
  end;

  TReportStatusEventArgs = record
    Status : string;
    Progress: Real;
    Message: string;
    Target: TObject;
  end;

  TDoWorkEventHandler = function(e: TDoWorkEventArgs):TRunWorkerCompletedEventArgs of Object;
  TRunWorkerCompletedEventHandler = procedure(e: TRunWorkerCompletedEventArgs) of Object;
  TReportStatusEventHandler = procedure(e: TReportStatusEventArgs) of Object;
  
  TBackgroundWorker = class(TThread)
  private
    DoWorkEventArgs: TDoWorkEventArgs;
    DoWorkEventHandler: TDoWorkEventHandler;
    RunWorkerCompletedEventHandler: TRunWorkerCompletedEventHandler;
    ReportStatusEventHandler: TReportStatusEventHandler;
    ReportStatusEventArgs: TReportStatusEventArgs;
    RunWorkerCompletedEventArgs: TRunWorkerCompletedEventArgs;

  protected
    procedure Execute; override;
    procedure Completed(Sender: TObject);

  public
    Constructor Create(createSuspended : boolean = True); overload;
    constructor Create(doWork: TDoWorkEventHandler; workCompleted : TRunWorkerCompletedEventHandler; createSuspended: boolean = False); overload;
    
    property OnDoWork: TDoWorkEventHandler read DoWorkEventHandler write DoWorkEventHandler;
    property ReportStatus: TReportStatusEventHandler read ReportStatusEventHandler write ReportStatusEventHandler;
    property DoWorkArgs: TDoWorkEventArgs read DoWorkEventArgs write DoWorkEventArgs;
    property RunWorkerCompleted : TRunWorkerCompletedEventHandler read RunWorkerCompletedEventHandler  write RunWorkerCompletedEventHandler;

    procedure RunWorkerAsync(e: TDoWorkEventArgs); overload;
    procedure RunWorkerAsync(); overload;
    procedure OnReportStatus();
  end;

implementation

(* Constructor *)
constructor TBackgroundWorker.Create(createSuspended: boolean = True); overload;
begin
    FreeOnTerminate := True;
    inherited Create(createSuspended);
    OnTerminate := @Completed;
end;

constructor TBackgroundWorker.Create(doWork: TDoWorkEventHandler; workCompleted : TRunWorkerCompletedEventHandler; createSuspended: boolean = False); overload;
begin
    FreeOnTerminate := True;
    inherited Create(createSuspended);
    OnTerminate := @Completed;

    OnDoWork := doWork;
    RunWorkerCompleted := workCompleted;
end;

procedure TBackgroundWorker.OnReportStatus();
begin
    if Assigned(ReportStatus) then
    begin
        ReportStatus(ReportStatusEventArgs);
    end;
end;

procedure TBackgroundWorker.RunWorkerAsync(e: TDoWorkEventArgs); overload;
begin
    DoWorkArgs := e;
    Start;
end;

procedure TBackgroundWorker.RunWorkerAsync(); overload;
begin
    Start;
end;

procedure TBackgroundWorker.Completed(Sender: TObject);
begin
    if Assigned(RunWorkerCompletedEventHandler) then
    begin
        RunWorkerCompletedEventHandler(RunWorkerCompletedEventArgs);
    end;
end;

procedure TBackgroundWorker.Execute;
var r: TRunWorkerCompletedEventArgs;
begin
    ReportStatusEventArgs.Message := 'Execute begin!';
    Synchronize(@OnReportStatus);

    if Assigned(DoWorkEventHandler) then
    begin
        ReportStatusEventArgs.Message := 'Assigned DoWorkEventHandler! DoWorkEventArgs.Message='+DoWorkEventArgs.Message;
        Synchronize(@OnReportStatus);

        RunWorkerCompletedEventArgs := DoWorkEventHandler(DoWorkEventArgs);

        ReportStatusEventArgs.Message := 'Completed DoWorkEventHandler!';
        Synchronize(@OnReportStatus);
    end;

    ReportStatusEventArgs.Message := 'Excute complted! RunWorkerCompletedEventArgs.Message=' + DoWorkEventArgs.Message;
    Synchronize(@OnReportStatus);
end;

end.

