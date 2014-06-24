unit CloseConfirmFormUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    LocalizedForms,
    Translations, DefaultTranslator,
    ZentaoAPIUnit;

type

    { TCloseConfirmForm }
    TCloseConfirmForm = class(TLocalizedForm)
        Button1: TButton;
        CheckBox1: TCheckBox;
        Label1: TLabel;
        RadioButton1: TRadioButton;
        RadioButton2: TRadioButton;
        procedure Button1Click(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure RadioButton1Click(Sender: TObject);
    private
        { private declarations }

        var slectionValue : Integer;
    public
        { public declarations }
    end;

var
    CloseConfirmForm: TCloseConfirmForm;

implementation

{$R *.lfm}

{ TCloseConfirmForm }

procedure TCloseConfirmForm.RadioButton1Click(Sender: TObject);
begin
    slectionValue := (Sender as TRadioButton).Tag;
end;

procedure TCloseConfirmForm.Button1Click(Sender: TObject);
begin
    if checkBox1.Checked then
    begin
        user.CloseOption := slectionValue;
    end
    else
    begin
        user.CloseOption := -1;
    end;

    Self.ModalResult := slectionValue;
end;

procedure TCloseConfirmForm.FormCreate(Sender: TObject);
begin
    slectionValue := 1;
end;

end.

