unit ZentaoAPIUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    md5,
    fphttpclient,
    fpjson, jsonparser;

type
    { Record }

    UserConfig = record
        Url:      string;
        Account:  string;
        Password: string;
        PassMd5:  string;
        Role:     string;
    end;

    HandleResult = record
        Result:  boolean;
        Message: string;
        Sender:  TObject;
    end;

    DataResult = record
        Result:  boolean;
        Message: string;
        Data:    TJSONObject;
    end;

    BrowseType = (btTodo = 0, btTask = 1, btBug = 2, btStory = 3);

{ Function declarations }
procedure InitZentaoAPI();
procedure Destroy();
function GetAPI(const Params: array of const): string;
function CheckVersion(): HandleResult;
function GetConfig(): HandleResult;
function GetSession(): HandleResult;
function Login(): HandleResult;
function GetRole(): HandleResult;
function TryLogin(): HandleResult;
function LoadDataList(obj: string; objType: string; pageID: string): DataResult;

var
    User:         UserConfig;
    ZentaoConfig: TJSONObject;
    Session:      TJSONObject;
    Http:         TFPHTTPClient;
    BrowseName:   array[BrowseType] of string;
    BrowseTypes:  array[0..3] of BrowseType;

implementation

(* Load Data from server with zentao api and return in a list *)
function LoadDataList(obj: string; objType: string; pageID: string): DataResult;
var
    response: string;
    Data:     TJSONObject;
    url:      string;
begin
    Result.Result := True;
    url           := GetAPI(['module', 'my', 'method', obj, 'type', objType, 'pageID', pageID]);
 
    try
        response := Http.Get(url);
        try
            (* prepare data *)
            Data     := TJSONObject(TJSONParser.Create(response).Parse);
            response := Data.Get('data', '');
            if response <> '' then
                Data := TJSONObject(TJSONParser.Create(response).Parse);

            Result.Data := Data;

        except
            Result.Result  := False;
            Result.Message := '服务器返回的数据不正确。 URL: ' + url;
        end;
    except
        Result.Result  := False;
        Result.Message := '无法连接到服务器。';
    end;
end;

(* Get config *)
function GetConfig(): HandleResult;
var
    configStr: string;
begin
    Result.Result := True;
    try
        configStr := Http.Get(User.Url + '/index.php?mode=getconfig');
        if Length(configStr) > 0 then
        begin
            ZentaoConfig := TJSONObject(TJSONParser.Create(configStr).Parse);
        end
        else
            Result.Result := False;
    except
        Result.Result := False;
    end;
    if not Result.Result then
        Result.Message := '无法获取禅道配置信息。';
end;

(* Check version *)
function CheckVersion(): HandleResult;
var
    version: string;
    verNum:  extended;
    isPro:   boolean;
begin
    version := zentaoConfig.Strings['version'];
    isPro   := False;

    if Pos('pro', LowerCase(version)) > 0 then
    begin
        isPro   := True;
        version := StringReplace(version, 'pro', '', [rfReplaceAll]);
    end;

    verNum        := StrToFloat(version);
    Result.Result := False;

    if isPro and (verNum <= 1.3) then
    begin
        Result.Message := Format('您当前版本是%s，请升级至%s以上版本', [version, 'pro1.3']);
    end
    else if not isPro and (verNum < 4) then
    begin
        Result.Message := Format('您当前版本是%s，请升级至%s以上版本', [version, '4.0']);
    end
    else
        Result.Result := True;
end;

(* Get API address *)
function GetAPI(const Params: array of const): string;
var
    config:  TJSONObject;
    viewType, moduleName, methodName, password, pageID: string;
    item:    TJSONEnum;
    nameSet: TStringList;
begin
    config     := TJSONObject.Create(Params);
    viewType   := config.Get('viewType', 'json');
    moduleName := config.Get('module', '');
    methodName := config.Get('method', '');
    nameSet    := TStringList.Create;
    nameSet.CommaText := 'viewType,module,method,moduleName,methodName,pageID,type';

    if LowerCase(ZentaoConfig.Get('requestType', '')) = 'get' then
    begin
        Result := User.Url + '/index.php?';
        if (moduleName = 'user') and (methodName = 'login') then
        begin
            password := MD5Print(MD5String(User.Password +
                IntToStr(Session.Int64s['rand'])));
            Result   := Result + 'm=user&f=login&account=' + User.Account +
                '&password=' + password + '&' + Session.Get('sessionName', '') +
                '=' + Session.Get('sessionID', '') + '&t=json';
            Exit;
        end;

        Result := Result + 'm=' + moduleName + '&f=' + methodName;

        if (moduleName = 'api') and (LowerCase(methodName) = 'getmodel') then
        begin
            Result := Result + '&moduleName=' + config.Get('moduleName', '') +
                '&methodName=' + config.Get('methodName', '') + '&params=';
            for item in config do
            begin
                if (nameSet.indexOf(item.Key) > 0) then
                    continue;
                Result := Result + item.Key + '=' + item.Value.AsString + '&';
            end;
        end;

        if moduleName = 'my' then
            Result := Result + '&type=' + config.Get('type', '');
        if methodName = 'view' then
            Result := Result + '&' + moduleName + 'ID=' + config.Get('ID', '');

        pageID := config.Get('pageID', '');
        if pageID <> '' then
        begin
            if methodName = 'todo' then
            begin
                Result := Result +
                    '&account=&status=all&orderBy=date_desc,status,begin&';
            end
            else
            begin
                // Result := Result + '&orderBy=id_desc&recTotal=' + pager.recTotal + '&recPerPage=' + pager.recPerPage + '&pageID=' + pageID;
            end;
        end;

        Result := Result + '&t=' + viewType;

        if not Session.Get('undefined', False) then
        begin
            Result := Result + '&' + Session.Get('sessionName', '') +
                '=' + Session.Get('sessionID', '');
        end;
    end
    else
    begin
        Result := Result + User.Url + '/';
        if (moduleName = 'user') and (methodName = 'login') then
        begin
            password := MD5Print(MD5String(User.PassMd5 +
                IntToStr(Session.Int64s['rand'])));
            Result   := Result + 'user-login.json?account=' +
                User.Account + '&password=' + password + '&' +
                Session.Get('sessionName', 'sid') + '=' + Session.Get('sessionID', '');
            Exit;
        end;

        Result := Result + moduleName + '-' + methodName + '-';

        if (moduleName = 'api') and (LowerCase(methodName) = 'getmodel') then
        begin
            Result := Result + config.Get('moduleName', '') + '-' +
                config.Get('methodName', '') + '-';
        end;

        if moduleName = 'my' then
            Result := Result + config.Get('type', '') + '-';

        for item in config do
        begin
            if (nameSet.indexOf(item.Key) > 0) then
                continue;
            Result := Result + item.Key + '=' + item.Value.AsString + '-';
        end;

        // pageID := config.Get('pageID', '');
        // if pageID <> '' then
        // begin
        //     if methodName = 'todo' then
        //     begin
        //         Result := Result + '--all-date_desc,status,begin-';
        //     end
        //     else
        //     begin
        //         // Result := Result + '-id_desc-' + pager.recTotal + '-' + pager.recPerPage + '-' + pageID;
        //     end;
        // end;

        if Result[Length(Result)] = '-' then
            Result := Copy(Result, 1, Length(Result) - 1);

        Result := Result + '.' + viewType;

        if not Session.Get('undefined', False) then
        begin
            Result := Result + '?' + Session.Get('sessionName', '') +
                '=' + Session.Get('sessionID', '');
        end;
    end;
end;

(* Get session *)
function GetSession: HandleResult;
var
    sessionStr: string;
begin
    Result.Result := True;
    try
        sessionStr := Http.Get(GetAPI(['module', 'api', 'method', 'getSessionID']));
        if Length(sessionStr) > 0 then
        begin
            Session := TJSONObject(TJSONParser.Create(sessionStr).Parse);
            if Session.Get('status', '') = 'success' then
            begin
                sessionStr := Session.Get('data', '');
                Session    := TJSONObject(TJSONParser.Create(sessionStr).Parse);
            end
            else
                Result.Result := False;
        end
        else
            Result.Result := False;
    except
        Result.Result := False;
    end;

    if not Result.Result then
        Result.Message := '无法获取Session。';
end;

(* Login *)
function Login(): HandleResult;
var
    response: string;
    status:   TJSONObject;
begin
    Result.Result := True;
    try
        response := Http.Get(GetAPI(['module', 'user', 'method', 'login']));
        if Length(response) > 0 then
        begin
            status := TJSONObject(TJSONParser.Create(response).Parse);
            if status.get('status', '') = 'failed' then
            begin
                Result.Result := False;
            end;
        end
        else
            Result.Result := False;
    except
        Result.Result := False;
    end;
    if not Result.Result then
        Result.Message := '登录失败。请检查用户名和密码。';
end;

(* Get role *)
function GetRole(): HandleResult;
var
    response: string;
    role:     TJSONObject;
begin
    Result.Result := True;
    try
        response := Http.Get(GetAPI(['module', 'api', 'method', 'getmodel',
            'moduleName', 'user', 'methodName', 'getById', 'account', User.Account]));
        if Length(response) > 0 then
        begin
            role := TJSONObject(TJSONParser.Create(response).Parse);
            if role.get('status', '') = 'failed' then
            begin
                Result.Result := False;
            end
            else
            begin
                role      := TJSONObject(TJSONParser.Create(role.Get('data', '')).Parse);
                User.Role := role.Get('role', '');
            end;
        end
        else
            Result.Result := False;
    except
        Result.Result := False;
    end;

    if not Result.Result then
        Result.Message := '获取角色信息失败。';
end;

(* Try login in *)
function TryLogin(): HandleResult;
begin
    Session := TJSONObject.Create(['undefined', True]);

    Result := GetConfig();
    if not Result.Result then
        Exit;

    //Result := CheckVersion();
    //if not Result.Result then
    //   Exit;

    Result := GetSession();
    if not Result.Result then
        Exit;

    Result := Login();
    if not Result.Result then
        Exit;

    Result := GetRole();
    if not Result.Result then
        Exit;
end;

procedure InitZentaoAPI();
begin
    Http := TFPHTTPClient.Create(nil);

    (* init browsename *)
    BrowseName[btTodo]  := 'todo';
    BrowseName[btTask]  := 'task';
    BrowseName[btBug]   := 'bug';
    BrowseName[btStory] := 'story';

    BrowseTypes[0] := btTodo;
    BrowseTypes[1] := btTask;
    BrowseTypes[2] := btBug;
    BrowseTypes[3] := btStory;
end;

procedure Destroy();
begin
    Http.Free;
end;

end.
