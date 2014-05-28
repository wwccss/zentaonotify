unit BackgroundWorkerUnit;

{$mode objfpc}{$H+}

interface

uses
    {$ifdef unix}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
    {$endif}
    Classes, SysUtils;

Type
  TRunWorkerCompletedEventArgs = record
    Result : Boolean;
    Message : string;
    Target : TObject;
    Tag: Integer;
  end;

  TReportStatusEventArgs = record
    Status : string;
    Progress: Real;
    Message: string;
    Target: TObject;
  end;

  TDoWorkEventHandler = function(arg: TObject):TRunWorkerCompletedEventArgs of Object;
  TRunWorkerCompletedEventHandler = procedure(e: TRunWorkerCompletedEventArgs) of Object;
  TReportStatusEventHandler = procedure(e: TReportStatusEventArgs) of Object;
  
  TBackgroundWorker = class(TThread)
  private
    DoWorkEventHandler: TDoWorkEventHandler;
    RunWorkerCompletedEventHandler: TRunWorkerCompletedEventHandler;
    DoWorkArgs: TObject;
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
    property RunWorkerCompleted : TRunWorkerCompletedEventHandler read RunWorkerCompletedEventHandler  write RunWorkerCompletedEventHandler;

    procedure RunWorkerAsync(arg: TObject); overload;
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

procedure TBackgroundWorker.RunWorkerAsync(arg: TObject); overload;
begin
    DoWorkArgs := arg;
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
        ReportStatusEventArgs.Message := 'Assigned DoWorkEventHandler! DoWorkEventArgs.Message=';
        Synchronize(@OnReportStatus);

        RunWorkerCompletedEventArgs := DoWorkEventHandler(DoWorkArgs);

        ReportStatusEventArgs.Message := 'Completed DoWorkEventHandler!';
        Synchronize(@OnReportStatus);
    end;

    ReportStatusEventArgs.Message := 'Excute complted! RunWorkerCompletedEventArgs.';
    Synchronize(@OnReportStatus);
end;

end.

