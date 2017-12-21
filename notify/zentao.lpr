program zentao;

{$mode objfpc}{$H+}

uses
    {$ifdef unix}
    cthreads,
    cmem, // the c memory manager is on some systems much faster for multi-threading
    {$endif}
    Interfaces, // this includes the LCL widgetset
    Forms,
    runtimetypeinfocontrols,
    LoginFormUnit,
    ZentaoAPIUnit,
    MainFormUnit,
    SysUtils,
    BackgroundWorkerUnit,
    PopWindowUnit,
    AboutUnit,
    localizedforms,
    StringsUnit,
    CloseConfirmFormUnit;

{$R *.res}

begin
    RequireDerivedFormResource := True;
    Application.Initialize;
    Application.CreateForm(TLoginForm, LoginForm);
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TPopWindow, PopWindow);
    Application.CreateForm(TAboutForm, AboutForm);
    Application.CreateForm(TCloseConfirmForm, CloseConfirmForm);
    Application.Run;
end.
