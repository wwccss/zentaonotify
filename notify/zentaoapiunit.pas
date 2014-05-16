unit ZentaoAPIUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    md5,
    fphttpclient,
    fpjson,jsonparser;

type
    { Record }

    UserConfig = record
        Url      : string;
        Account  : string;
        Password : string;
        PassMd5  : string;
        Role     : string;
    end;

    HandleResult = record
        Result   : boolean;
        Message  : string;
        Sender   : TObject;
    end;


{ Function declarations }
function GetAPI(const Params: array of Const): string;
function CheckVersion(): HandleResult;
function GetConfig(): HandleResult;
function GetSession(): HandleResult;
function Login(): HandleResult;
function GetRole(): HandleResult;
function TryLogin(): HandleResult;

var
    User         : UserConfig;
    ZentaoConfig : TJSONObject;
    Session      : TJSONObject;
    Http         : TFPHTTPClient;

implementation

(* Get config *)
function GetConfig(): HandleResult;
var configStr : string;
begin
    Result.Result := true;
    try
        configStr := Http.Get(User.Url + '/index.php?mode=getconfig');
        if Length(configStr) > 0 then
        begin
            ZentaoConfig := TJSONObject(TJSONParser.Create(configStr).Parse);
        end
        else
            Result.Result := false;
    except
        Result.Result := false;
    end;
    if not Result.Result then
        Result.Message := '无法获取禅道配置信息。';
end;

(* Check version *)
function CheckVersion(): HandleResult;
var version : string;
var verNum  : Extended;
var isPro : Boolean;
begin
    version := zentaoConfig.Strings['version'];
    isPro   := false;
    if Pos('pro', LowerCase(version)) > 0 then
    begin
        isPro := true;
        version := StringReplace(version, 'pro', '', [rfReplaceAll]);
    end;

    verNum := StrToFloat(version);
    Result.Result := false;

    if isPro and (verNum <= 1.3) then
    begin
        Result.Message := Format('您当前版本是%s，请升级至%s以上版本', [version, 'pro1.3']);
    end
    else if not isPro and (verNum < 4) then
    begin
        Result.Message := Format('您当前版本是%s，请升级至%s以上版本', [version, '4.0']);
    end
    else
        Result.Result := true;
end;

(* Get API address *)
function GetAPI(const Params: array of Const): string;
var config : TJSONObject;
var viewType, moduleName, methodName, password, pageID : string;
var item : TJSONEnum;
var nameSet : TStringList;
begin
    config     := TJSONObject.Create(Params);
    viewType   := config.Get('viewType', 'json');
    moduleName := config.Get('module', '');
    methodName := config.Get('method', '');
    nameSet    := TStringList.Create;
    nameSet.CommaText := 'viewType,module,method,moduleName,methodName,pageID';

    if LowerCase(ZentaoConfig.Get('requestType', '')) = 'get' then
    begin
        Result := User.Url + '/index.php?';
        if (moduleName = 'user') and (methodName = 'login') then
        begin
            password := MD5Print(MD5String(User.Password + IntToStr(Session.Int64s['rand'])));
            Result := Result + 'm=user&f=login&account=' + User.Account + '&password=' + password + '&' + Session.Get('sessionName', '') + '=' + Session.Get('sessionID', '') + '&t=json';
            Exit;
        end;

        Result := Result + 'm=' + moduleName + '&f=' + methodName;

        if (moduleName = 'api') and (LowerCase(methodName) = 'getmodel') then
        begin
            Result := Result + '&moduleName=' + config.Get('moduleName', '') + '&methodName=' + config.Get('methodName', '') + '&params=';
            for item in config do
            begin
                if (nameSet.indexOf(item.Key) > 0) then
                    continue;
                Result := Result + item.Key + '=' + item.Value.AsString + '&';
            end;
        end;

        if moduleName = 'my' then Result := Result + '&type=' + config.Get('type','');
        if methodName = 'view' then Result := Result + '&' + moduleName + 'ID=' + config.Get('ID','');

        pageID := config.Get('pageID', '');
        if pageID <> '' then
        begin
            if methodName = 'todo' then
            begin
                Result := Result + '&account=&status=all&orderBy=date_desc,status,begin&';
            end
            else
            begin
                // Result := Result + '&orderBy=id_desc&recTotal=' + pager.recTotal + '&recPerPage=' + pager.recPerPage + '&pageID=' + pageID;
            end;
        end;

        Result := Result + '&t=' + viewType;

        if not Session.Get('undefined', false) then
        begin
            Result := Result + '&' + Session.Get('sessionName', '') + '=' + Session.Get('sessionID', '');
        end;
    end
    else
    begin
        Result := Result + User.Url + '/';
        if (moduleName = 'user') and (methodName = 'login') then
        begin
            password := MD5Print(MD5String(User.PassMd5 + IntToStr(Session.Int64s['rand'])));
            Result := Result + 'user-login.json?account=' + User.Account + '&password=' + password + '&' + Session.Get('sessionName', 'sid') + '=' + Session.Get('sessionID', '');
            Exit;
        end;

        Result := Result + moduleName + '-' + methodName + '-';

        if (moduleName = 'api') and (LowerCase(methodName) = 'getmodel') then
        begin
            Result := Result + config.Get('moduleName', '') + '-' + config.Get('methodName', '') + '-';
            for item in config do
            begin
                if (nameSet.indexOf(item.Key) > 0) then
                    continue;
                Result := Result + item.Key + '=' + item.Value.AsString + '-';
            end;
        end;

        pageID := config.Get('pageID', '');
        if pageID <> '' then
        begin
            if methodName = 'todo' then
            begin
                Result := Result + '--all-date_desc,status,begin-';
            end
            else
            begin
                // Result := Result + '-id_desc-' + pager.recTotal + '-' + pager.recPerPage + '-' + pageID;
            end;
        end;

        if Result[Length(Result)] = '-' then
            Result := Copy(Result, 1, Length(Result) - 1);

        Result := Result + '.' + viewType;

        if not Session.Get('undefined', false) then
        begin
            Result := Result + '?' + Session.Get('sessionName', '') + '=' + Session.Get('sessionID', '');
        end;
    end;
end;

(* Get session *)
function GetSession:HandleResult;
var sessionStr : string;
begin
    Result.Result := true;
    try
        sessionStr := Http.Get(GetAPI(['module', 'api', 'method', 'getSessionID']));
        if Length(sessionStr) > 0 then
        begin
            Session := TJSONObject(TJSONParser.Create(sessionStr).Parse);
            if Session.Get('status', '') = 'success' then
            begin
                sessionStr := Session.Get('data', '');
                Session := TJSONObject(TJSONParser.Create(sessionStr).Parse);
            end
            else
                Result.Result := false;
        end
        else
            Result.Result := false;
    except
        Result.Result := false;
    end;

    if not Result.Result then
        Result.Message := '无法获取Session。';
end;

(* Login *)
function Login(): HandleResult;
var response : string;
var status   : TJSONObject;
begin
    Result.Result := true;
    try
        response := Http.Get(GetAPI(['module', 'user', 'method', 'login']));
        if Length(response) > 0 then
        begin
            status := TJSONObject(TJSONParser.Create(response).Parse);
            if status.get('status', '') = 'failed' then
            begin
                Result.Result := false;
            end
        end
        else
            Result.Result := false;
    except
        Result.Result := false;
    end;
    if not Result.Result then
        Result.Message := '登录失败。请检查用户名和密码。';
end;

(* Get role *)
function GetRole(): HandleResult;
var response : string;
var role     : TJSONObject;
begin
    Result.Result := true;
    try
        response := Http.Get(GetAPI(['module', 'api', 'method', 'getmodel', 'moduleName', 'user', 'methodName', 'getById', 'account', User.Account]));
        if Length(response) > 0 then
        begin
            role := TJSONObject(TJSONParser.Create(response).Parse);
            if role.get('status', '') = 'failed' then
            begin
                Result.Result := false;
            end
            else
            begin
                role := role.Get('data', TJSONObject.Create(['role', '']));
                User.Role := role.Get('role', '');
            end;
        end
        else
            Result.Result := false;
    except
        Result.Result := false;
    end;

    if not Result.Result then
        Result.Message := '获取角色信息失败。';
end;

(* Try login in *)
function TryLogin(): HandleResult;
begin
    Session := TJSONObject.Create(['undefined', true]);

    Http := TFPHTTPClient.Create(Nil);

    Result := GetConfig();
    if not Result.Result then
    begin
        Exit;
        Http.Free;
    end;

    Result := CheckVersion();
    if not Result.Result then
    begin
       Exit;
       Http.Free;
    end;

    Result := GetSession();
    if not Result.Result then
    begin
        Exit;
        Http.Free;
    end;

    Result := Login();
    if not Result.Result then
    begin
        Exit;
        Http.Free;
    end;

    Result := GetRole();
    if not Result.Result then
    begin
        Exit;
        Http.Free;
    end;

    Http.Free;
end;

end.

