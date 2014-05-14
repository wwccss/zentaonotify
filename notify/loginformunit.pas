unit LoginFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons;

type

  { TLoginForm }

  TLoginForm = class(TForm)
    EditAddress: TEdit;
    EditUsername: TEdit;
    EditPassword: TEdit;
    ImageBackground: TImage;
    ShapeAddress: TShape;
    ShapeUsername: TShape;
    ShapePassword: TShape;
    BitBtnLogin: TBitBtn;
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditPasswordEnter(Sender: TObject);
    procedure EditPasswordExit(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  LoginForm: TLoginForm;

implementation

{$R *.lfm}

{ TLoginForm }

procedure TLoginForm.EditEnter(Sender: TObject);
var EditSender : TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = EditSender.Hint then
        EditSender.Text := '';
end;

procedure TLoginForm.EditExit(Sender: TObject);
var EditSender : TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = '' then
        EditSender.Text := EditSender.Hint;
end;

procedure TLoginForm.EditChange(Sender: TObject);
var EditSender : TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = EditSender.Hint then
        EditSender.Font.Color := clGray
    else
        EditSender.Font.Color := clBlack;
end;

procedure TLoginForm.EditPasswordEnter(Sender: TObject);
begin
    EditEnter(Sender);
    EditPassword.PasswordChar := '*';
end;

procedure TLoginForm.EditPasswordExit(Sender: TObject);
begin
    EditExit(Sender);
    if EditPassword.Text = EditPassword.Hint then
        EditPassword.PasswordChar := #0
end;

end.

