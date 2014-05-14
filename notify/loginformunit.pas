unit LoginFormUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons,
  fpjson,jsonparser,
  fphttpclient;

type

  { TLoginForm }

  TLoginForm = class(TForm)
    Edit1: TEdit;
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
    function CheckInputs:Boolean;
    function GetConfig:Boolean;

  private
    { private declarations }
  public
    { public declarations }
  end;

var
  LoginForm: TLoginForm;
  ZentaoConfig: TJSONObject;

implementation

{$R *.lfm}

{ TLoginForm }

function TLoginForm.GetConfig():Boolean;
var configStr : string;
begin
    With TFPHTTPClient.Create(Nil) do
    try
        configStr := Get(EditAddress.Text + '/index.php?mode=getconfig');
        ZentaoConfig := TJSONObject(TJSONParser.Create(configStr).Parse);

        Edit1.Text := ZentaoConfig.Strings['version'];
    finally
        Free;
    end;
    Result := true;
end;

function TLoginForm.CheckInputs():Boolean;
begin
    if (EditAddress.Text = '') or (EditAddress.Text = EditAddress.Hint) then
        begin
            LabelResult.Caption := '请填写禅道地址。';
            LabelResult.Visible := true;
            Result := false;
        end
    else if (EditUsername.Text = '') or (EditUsername.Text = EditUsername.Hint) then
        begin
            LabelResult.Caption := '请填写用户名。';
            LabelResult.Visible := true;
            Result := false;
        end
    else if (EditPassword.Text = '') or (EditPassword.Text = EditPassword.Hint) then
        begin
            LabelResult.Caption := '请填写禅道密码。';
            LabelResult.Visible := true;
            Result := false;
        end
    else
        LabelResult.Visible := false;
end;

procedure TLoginForm.EditEnter(Sender: TObject);
var EditSender : TEdit;
begin
    EditSender := Sender as TEdit;
    if EditSender.Text = EditSender.Hint then
        EditSender.Text := '';
end;

procedure TLoginForm.BitBtnLoginClick(Sender: TObject);
begin
    if CheckInputs() then
        begin
            BitBtnLogin.Caption := 'Logining';
            GetConfig();
        end;
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
    if (EditSender.Text <> '') and (EditSender.Text <> EditSender.Hint) then
    LabelResult.Visible := false;
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

