unit LoginFormUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, Buttons,
    md5,
    fpjson, jsonparser,
    LCLIntf,
    ZentaoAPIUnit,
    AboutUnit,
    BackgroundWorkerUnit;

type
    { TLoginForm }
    TLoginForm = class(TForm)
        EditAddress:     TEdit;
        EditUsername:    TEdit;
        EditPassword:    TEdit;
        ImageBackground: TImage;
        LabelResult:     TLabel;
        LabelResult1: TLabel;
        LabelBtnAbout: TLabel;
        ShapeAddress:    TShape;
        ShapeUsername:   TShape;
        ShapePassword:   TShape;
        BitBtnLogin:     TBitBtn;
        
        procedure BitBtnLoginClick(Sender: TObject);
        procedure EditEnter(Sender: TObject);
        procedure EditExit(Sender: TObject);
        procedure EditChange(Sender: TObject);
        procedure EditPasswordEnter(Sender: TObject);
        procedure EditPasswordExit(Sender: TObject);
        procedure FormCreate(Sender: TObject);
        procedure LabelBtnAboutClick(Sender: TObject);
        procedure LabelResult1Click(Sender: TObject);
        procedure ShowResultMessage(message: string);
        procedure HideResultMessage();
        procedure LoginCompleted(e: TRunWorkerCompletedEventArgs);
        function Logining(arg: TObject):TRunWorkerCompletedEventArgs;
        function CheckInputs: boolean;

    private
        { private declarations }
    public
        { public declarations }
    end;

var
    LoginForm: TLoginForm;

implementation

uses MainFormUnit;

{$R *.lfm}

{ TLoginForm members }

(* Check inputs *)
function TLoginForm.CheckInputs(): boolean;
begin
    Result := False;
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
        LabelResult.Visible := False;
        Result := True;
    end;
end;

(* Handle event: on textbox focus *)
procedure TLoginForm.EditEnter(Sender: TObject);
var
    EditSender: TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = EditSender.Hint then
        EditSender.Text := '';
end;

(* Handle event: on click the login button *)
procedure TLoginForm.BitBtnLoginClick(Sender: TObject);
begin
    if CheckInputs() then
    begin
        BitBtnLogin.Caption := '登录中...';
        BitBtnLogin.Enabled := False;

        user.Account  := EditUsername.Text;
        user.Password := EditPassword.Text;
        user.PassMd5  := MD5Print(MD5String(user.Password));
        user.Url      := EditAddress.Text;

        InitZentaoAPI();

        TBackgroundWorker.Create(@Logining, @LoginCompleted);
    end;
end;

(* Handle event: on textbox blur *)
procedure TLoginForm.EditExit(Sender: TObject);
var
    EditSender: TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = '' then
        EditSender.Text := EditSender.Hint;
end;

(* Handle event: on textbox changed *)
procedure TLoginForm.EditChange(Sender: TObject);
var
    EditSender: TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = EditSender.Hint then
        EditSender.Font.Color := clGray
    else
        EditSender.Font.Color := clBlack;
    if (EditSender.Text <> '') and (EditSender.Text <> EditSender.Hint) then
        HideResultMessage();
    ;
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

procedure TLoginForm.LoginCompleted(e: TRunWorkerCompletedEventArgs);
var
    r: HandleResult;
begin
    BitBtnLogin.Caption := '登录';
    BitBtnLogin.Enabled := True;

    r.Result := e.Result;
    r.Message := e.Message;

    if not r.Result then
    begin
        ShowResultMessage(r.Message);
    end
    else
    begin
        ShowResultMessage('登录成功！');
        HideResultMessage;
        LoginForm.Hide;
        MainForm.Show;
    end;
end;

function TLoginForm.Logining(arg: TObject): TRunWorkerCompletedEventArgs;
var
    r: HandleResult;
begin
    r := TryLogin;
    Result.Result := r.Result;
    Result.Message := r.Message;
end;

procedure TLoginForm.FormCreate(Sender: TObject);
begin
    inherited;
    session := TJSONObject.Create(['undefined', True]);

    // Test data
    EditAddress.Text  := 'http://zentao.com/';
    EditUsername.Text := 'hello';
    EditPassword.Text := '123321';
end;

procedure TLoginForm.LabelBtnAboutClick(Sender: TObject);
begin
    AboutForm.ShowModal;
end;

procedure TLoginForm.LabelResult1Click(Sender: TObject);
begin
    OpenURL('http://www.zentao.net/');
end;

(* Hide result message *)
procedure TLoginForm.HideResultMessage();
begin
    LabelResult.Visible := False;
end;

procedure TLoginForm.ShowResultMessage(message: string);
begin
    LabelResult.Caption := message;
    LabelResult.Visible := True;
end;

end.
