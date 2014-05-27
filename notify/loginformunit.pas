unit LoginFormUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, Buttons,
    md5,
    fpjson, jsonparser,
    LCLIntf, Menus,
    ZentaoAPIUnit,
    AboutUnit,
    LocalizedForms,
    StringsUnit,
    BackgroundWorkerUnit;

type
    { TLoginForm }
    TLoginForm = class(TLocalizedForm)
        CheckBoxRememberMe: TCheckBox;
        CheckBoxAutoSignIn: TCheckBox;
        EditAddress:     TEdit;
        EditUsername:    TEdit;
        EditPassword:    TEdit;
        ImageBackground: TImage;
        LabelBtnAbout: TLabel;
        LabelResult:     TLabel;
        LabelResult1: TLabel;
        LabelBtnLanguage: TLabel;
        MenuItemLangZHTW: TMenuItem;
        MenuItemLangEN: TMenuItem;
        MenuItemLangZHCN: TMenuItem;
        Panel1: TPanel;
        PopupMenuLang: TPopupMenu;
        ShapeAddress:    TShape;
        ShapeUsername:   TShape;
        ShapePassword:   TShape;
        BitBtnLogin:     TBitBtn;
        
        procedure BitBtnLoginClick(Sender: TObject);
        procedure CheckBoxAutoSignInChange(Sender: TObject);
        procedure CheckBoxRememberMeChange(Sender: TObject);
        procedure EditEnter(Sender: TObject);
        procedure EditExit(Sender: TObject);
        procedure EditChange(Sender: TObject);
        procedure EditPasswordChange(Sender: TObject);
        procedure EditPasswordEnter(Sender: TObject);
        procedure EditPasswordExit(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure LabelBtnAboutClick(Sender: TObject);
        procedure LabelBtnLanguageClick(Sender: TObject);
        procedure LabelResult1Click(Sender: TObject);
        procedure MenuItemLangClick(Sender: TObject);
        procedure ShowResultMessage(message: string);
        procedure HideResultMessage();
        procedure TryAutoLogin();
        procedure LoginCompleted(e: TRunWorkerCompletedEventArgs);
        function Logining(arg: TObject):TRunWorkerCompletedEventArgs;
        function CheckInputs: boolean;

    protected
        procedure UpdateTranslation(ALang: String); override;

    private
        { private declarations }
    public
        { public declarations }
    end;

var
    LoginForm: TLoginForm;
    FirstShow: boolean;
    PassMd5 : string;

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
        ShowResultMessage(rsRequireZentaoAddress);
    end
    else if (EditUsername.Text = '') or (EditUsername.Text = EditUsername.Hint) then
    begin
        ShowResultMessage(rsRequireUsername);
    end
    else if (EditPassword.Text = '') or (EditPassword.Text = EditPassword.Hint) then
    begin
        ShowResultMessage(rsRequirePassword);
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
        BitBtnLogin.Caption := rsLoging;
        BitBtnLogin.Enabled := False;

        user.Account  := EditUsername.Text;
        user.PassMd5  := PassMd5;
        user.Url      := EditAddress.Text;

        TBackgroundWorker.Create(@Logining, @LoginCompleted);
    end;
end;

procedure TLoginForm.CheckBoxAutoSignInChange(Sender: TObject);
begin
    if CheckBoxAutoSignIn.Checked then
        CheckBoxRememberMe.Checked := True;
    user.AutoSignIn := CheckBoxAutoSignIn.Checked;
end;

procedure TLoginForm.CheckBoxRememberMeChange(Sender: TObject);
begin
    if not CheckBoxRememberMe.Checked then
        CheckBoxAutoSignIn.Checked := False;
    user.RememberMe := CheckBoxRememberMe.Checked;
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

procedure TLoginForm.EditPasswordChange(Sender: TObject);
begin
    EditChange(Sender);

    PassMd5 := MD5Print(MD5String(EditPassword.Text));
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

procedure TLoginForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    // save config changed
    SaveConfig;
end;

procedure TLoginForm.LoginCompleted(e: TRunWorkerCompletedEventArgs);
var
    r: HandleResult;
begin
    BitBtnLogin.Caption := rsLogin;
    BitBtnLogin.Enabled := True;

    r.Result := e.Result;
    r.Message := e.Message;

    if not r.Result then
    begin
        ShowResultMessage(r.Message);
    end
    else
    begin
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
    FirstShow := True;

    Caption := rsAppName + ' ' + GetBuildVersion('%d.%d');

    InitZentaoAPI();
end;

procedure TLoginForm.FormShow(Sender: TObject);
begin
    if FirstShow then
    begin
        FirstShow := False;

        EditAddress.Text := EditAddress.Hint;
        EditUsername.Text := EditUsername.Hint;
        EditPassword.Text := EditPassword.Hint;

        TryAutoLogin;
    end;

    case CurrentLang of
        'zh_cn': MenuItemLangZHCN.Checked := True;
        'zh_tw': MenuItemLangZHTW.Checked := True;
        'en': MenuItemLangEN.Checked := True;
    end;
end;

procedure TLoginForm.TryAutoLogin();
begin
    if LoadConfig() then
    begin
        EditPassword.Text := user.PassMd5;
        EditAddress.Text := user.Url;
        EditUsername.Text := user.Account;
        PassMd5 := user.PassMd5;
        CheckBoxRememberMe.Checked := True;

        if user.Lang <> '' then
            SelectLanguage(user.Lang);

        if user.AutoSignIn then
        begin
           TBackgroundWorker.Create(@Logining, @LoginCompleted);
           CheckBoxAutoSignIn.Checked := True;
        end;
    end;
end;

procedure TLoginForm.LabelBtnAboutClick(Sender: TObject);
begin
    AboutForm.ShowModal;
end;

procedure TLoginForm.LabelBtnLanguageClick(Sender: TObject);
begin
    PopupMenuLang.Popup;
end;

procedure TLoginForm.LabelResult1Click(Sender: TObject);
begin
    OpenURL('http://www.zentao.net/');
end;

procedure TLoginForm.MenuItemLangClick(Sender: TObject);
var senderMenuItem: TMenuItem;
begin
    senderMenuItem := Sender as TMenuItem;

    SelectLanguage(senderMenuItem.Hint);
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

procedure TLoginForm.UpdateTranslation(ALang: String);
begin
    inherited;

    case ALang of
        'zh_cn': MenuItemLangZHCN.Checked := True;
        'zh_tw': MenuItemLangZHTW.Checked := True;
        'en': MenuItemLangEN.Checked := True;
    end;

    user.Lang := ALang;
end;

end.
