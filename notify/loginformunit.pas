unit LoginFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,
  md5,
  fpjson,jsonparser,
  ZentaoAPIUnit;

type

    { TLoginForm }
    TLoginForm = class(TForm)
        EditAddress: TEdit;
        EditUsername: TEdit;
        EditPassword: TEdit;
        ImageBackground: TImage;
        LabelResult: TLabel;
        ShapeAddress: TShape;
        ShapeUsername: TShape;
        ShapePassword: TShape;
        BitBtnLogin: TBitBtn;
        procedure BitBtnLoginClick(Sender: TObject);
        procedure EditEnter(Sender: TObject);
        procedure EditExit(Sender: TObject);
        procedure EditChange(Sender: TObject);
        procedure EditPasswordEnter(Sender: TObject);
        procedure EditPasswordExit(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure ShowResultMessage(Message: string);
        procedure HideResultMessage();
        function CheckInputs:Boolean;

    private
            { private declarations }
    public
            { public declarations }
    end;

var
    LoginForm    : TLoginForm;

implementation

uses MainFormUnit;

{$R *.lfm}

{ TLoginForm members }

(* Check inputs *)
function TLoginForm.CheckInputs():Boolean;
begin
    Result := true;
    if (EditAddress.Text = '') or (EditAddress.Text = EditAddress.Hint) then
        begin
            ShowResultMessage('请填写禅道地址。');
        end
    else if (EditUsername.Text = '') or (EditUsername.Text = EditUsername.Hint) then
        begin
            ShowResultMessage('请填写用户名。');
        end
    else if (EditPassword.Text = '') or (EditPassword.Text = EditPassword.Hint) then
        begin
            ShowResultMessage('请填写禅道密码。');
        end
    else
        begin
            LabelResult.Visible := false;
            Result := true;
        end;
end;

(* Handle event: on textbox focus *)
procedure TLoginForm.EditEnter(Sender: TObject);
var EditSender : TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = EditSender.Hint then
        EditSender.Text := '';
end;

(* Handle event: on click the login button *)
procedure TLoginForm.BitBtnLoginClick(Sender: TObject);
var r : HandleResult;
begin
    if CheckInputs() then
    begin
        BitBtnLogin.Caption := 'Logining';
        BitBtnLogin.Enabled := false;

        User.Account  := EditUsername.Text;
        User.Password := EditPassword.Text;
        User.PassMd5  := MD5Print(MD5String(User.Password));
        User.Url      := EditAddress.Text;

        InitZentaoAPI();
        r := TryLogin();

        if not r.Result then
        begin
            ShowResultMessage(r.Message);
            BitBtnLogin.Caption := '登录';
            BitBtnLogin.Enabled := true;
        end
        else
        begin
            ShowResultMessage('登录成功！');
            LoginForm.Hide;
            MainForm.Show;
        end;
    end;
end;

(* Handle event: on textbox blur *)
procedure TLoginForm.EditExit(Sender: TObject);
var EditSender : TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = '' then
        EditSender.Text := EditSender.Hint;
end;

(* Handle event: on textbox changed *)
procedure TLoginForm.EditChange(Sender: TObject);
var EditSender : TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = EditSender.Hint then
        EditSender.Font.Color := clGray
    else
        EditSender.Font.Color := clBlack;
    if (EditSender.Text <> '') and (EditSender.Text <> EditSender.Hint) then
        HideResultMessage();;
end;

(* Handle event: on password textbox focus *)
procedure TLoginForm.EditPasswordEnter(Sender: TObject);
begin
    EditEnter(Sender);
    EditPassword.PasswordChar := '*';
end;

(* Handle event: on password textbox blur *)
procedure TLoginForm.EditPasswordExit(Sender: TObject);
begin
    EditExit(Sender);
    if EditPassword.Text = EditPassword.Hint then
        EditPassword.PasswordChar := #0;
end;

procedure TLoginForm.FormCreate(Sender: TObject);
begin
    Session := TJSONObject.Create(['undefined', true]);

    // Test data
    EditAddress.Text := 'http://zentao.com/';
    EditUsername.Text := 'hello';
    EditPassword.Text := '123321';
end;

(* Hide result message *)
procedure TLoginForm.HideResultMessage();
begin
    LabelResult.Visible := false;
end;

procedure TLoginForm.ShowResultMessage(Message: string);
begin
    LabelResult.Caption := Message;
    LabelResult.Visible := true;
end;

end.

