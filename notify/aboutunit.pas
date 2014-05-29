unit AboutUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    LCLIntf,
    LocalizedForms,
    StringsUnit,
    ZentaoAPIUnit,
    StdCtrls;

type

    { TAboutForm }

    TAboutForm = class(TLocalizedForm)
        Image1:        TImage;
        LabelVersion:  TLabel;
        LabelVersion1: TLabel;
        LabelVersion2: TLabel;
        LabelVersion3: TLabel;
        procedure FormCreate(Sender: TObject);
        procedure LabelMouseEnter(Sender: TObject);
        procedure LabelMouseLeave(Sender: TObject);
        procedure LabelVersion1MouseMove(Sender: TObject; Shift: TShiftState;
            X, Y: integer);
        procedure LabelVersion2Click(Sender: TObject);
        procedure LabelVersion3Click(Sender: TObject);
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    AboutForm: TAboutForm;

implementation

{$R *.lfm}

{ TAboutForm }

procedure TAboutForm.LabelMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := $00e6953e;
end;

procedure TAboutForm.FormCreate(Sender: TObject);
begin
    LabelVersion.Caption := GetBuildVersion('build %d.%d.%d ') + GetBuildVersion('%s');
end;

procedure TAboutForm.LabelMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := $00EEA251;
end;

procedure TAboutForm.LabelVersion1MouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: integer);
begin

end;

procedure TAboutForm.LabelVersion2Click(Sender: TObject);
begin
    OpenURL('http://www.zentao.net/');
end;

procedure TAboutForm.LabelVersion3Click(Sender: TObject);
begin
    OpenURL('https://github.com/easysoft/zentaonotify');
end;

end.

