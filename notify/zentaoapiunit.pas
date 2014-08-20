unit ZentaoAPIUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils,
    {$ifdef WINDOWS}
    Windows,
    {$endif}
    md5,
    fphttpclient,
    LCLIntf,
    jsonconf,
    StringsUnit,
    Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList,
    fpjson, jsonparser;

type

    BrowseType        = (btTodo = 0, btTask = 1, btBug = 2, btStory = 3);
    BrowseSubType     = (today = 0, yesterday = 1, thisweek = 2, lastweek =
        3, assignedTo = 4, openedBy = 5, reviewedBy = 6, closedBy = 7, finishedBy =
        8, resolvedBy = 9);

    { User config }
    UserConfig = record
        Url:        string;
        Account:    string;
        PassMd5:    string;
        Role:       string;
        AutoSignIn: boolean;
        RememberMe: boolean;
        Lang:       string;
        CloseOption: Integer;
    end;

    { Handle result }
    HandleResult = record
        Result:  boolean;
        Message: string;
        OnlineHelp: string;
        Sender:  TObject;
    end;

    { Page record }
    PageRecord = record
        Status:     string;
        PageID:     integer;
        PageTotal:  integer;
        PerPage:    integer;
        Total:      integer;
        PageCookie: string;
    end;

    { Date result }
    TDataResult = class(TObject)
        Result:    boolean;
        Message:   string;
        Data:      TJSONObject;
        Pager:     PageRecord;
        IsNew:     boolean;
        FirstPage: boolean;
        Tab:       BrowseType;
        SubType:   BrowseSubType;

        constructor Create;
    end;

{ Function declarations }
procedure InitZentaoAPI();
procedure DestroyZentaoAPI();
function GetAPI(const Params: array of const): string;
function CheckVersion(): HandleResult;
function GetConfig(): HandleResult;
function GetSession(): HandleResult;
function Login(): HandleResult;
function GetRole(): HandleResult;
function TryLogin(): HandleResult;
function Logout(): HandleResult;
function LoadDataList(obj: BrowseType; objType: BrowseSubType;
    pageID: string = ''): TDataResult;
function Max(a: integer; b: integer): integer;
function Min(a: integer; b: integer): integer;
function ViewObject(objType: BrowseType; id: string): boolean;
procedure SaveConfig();
function LoadConfig(): boolean;
function GetBuildVersion(formatStr: string = '%d.%d.%d'): string;
function HttpGet(url: string): string;
procedure DInfo(textOrName: string; text: string = '`');

var
    user:           UserConfig;
    zentaoConfig:   TJSONObject;
    session:        TJSONObject;
    BrowseName:     array[BrowseType] of string;
    BrowseNames:    array[BrowseType] of string;
    BrowseTypes:    array[0..3] of BrowseType;
    BrowseSubName:  array[BrowseSubType] of string;
    BrowseSubTypes: array[0..9] of BrowseSubType;
    BrowsePagers:   array[BrowseType] of array[BrowseSubType] of PageRecord;
    BrowseMd5:      array[BrowseType] of string;
    PopWindowData:  TDataResult;
    MainFormWindow: TForm;

const
    ONEDAYMILLIONSECONDS = 24 * 60 * 60 * 1000;
    ONEDAYSECONDS        = 24 * 60 * 60;
    CONFIG_FILE          = 'config.json';
    DEBUG_MODE           = 0;

implementation

constructor TDataResult.Create;
begin
    IsNew     := False;
    Result    := True;
    FirstPage := False;
end;

{ Display debug info into console window. }
procedure DInfo(textOrName: string; text: string = '`');
begin
    if DEBUG_MODE > 0 then
    begin
        {$ifdef WINDOWS}
        if not IsConsole then
        begin
            AllocConsole;
            IsConsole := True; // in System unit
            SysInitStdIO;      // in System unit
        end;
        {$endif}

        if text = '`' then
        begin
            text := textOrName;
        end
        else
        begin
            text := textOrName + ': ' + text;
        end;

        writeln('> ' + text);
    end;
end;

(* View object in browser *)
function ViewObject(objType: BrowseType; id: string): boolean;
begin
    if Copy(id, 1, 1) = '#' then
        id := Copy(id, 2, Length(id) - 1);
    Result := OpenURL(GetAPI(['module', BrowseName[objType], 'method',
        'view', 'id', id, 'viewType', 'html']));
end;

(* Load Data from server with zentao api and return in a list *)
function LoadDataList(obj: BrowseType; objType: BrowseSubType;
    pageID: string = ''): TDataResult;
var
    response, md5: string;
    Data, pageData: TJSONObject;
    url:       string;
    pager:     PageRecord;
    firstPage: boolean;
begin
    Result         := TDataResult.Create;
    Result.Tab     := obj;
    Result.SubType := objType;
    DInfo('BEGIN: LoadDataList' + LineEnding);

    (* init page *)
    pager  := BrowsePagers[obj, objType];
    pageID := LowerCase(pageID);
    if pager.status = 'ok' then
    begin
        if (pageID = 'next') or (pageID = 'n') then
        begin
            pageID := IntToStr(Min(pager.PageID + 1, pager.PageTotal));
        end
        else if (pageID = 'prev') or (pageID = 'p') then
        begin
            pageID := IntToStr(Min(pager.PageID - 1, pager.PageTotal));
        end
        else if (pageID = 'last') or (pageID = 'l') then
        begin
            pageID := IntToStr(pager.PageTotal);
        end
        else if (pageID = 'first') or (pageID = 'f') then
        begin
            pageID := '';
        end
        else
        begin
            try
                pageID := IntToStr(StrToInt(pageID));
            except
                pageID := '';
            end;
        end;
    end
    else
    begin
        pageID := '';
    end;
    firstPage        := (pageID = '');
    Result.FirstPage := firstPage;

    (* get url *)
    url := GetAPI(['module', 'my', 'method', BrowseName[obj],
        'type', BrowseSubName[objType], 'pageID', pageID, 'recTotal',
        IntToStr(pager.Total), 'recPerPage', IntToStr(pager.PerPage)]);

    try
        response := HttpGet(url);
        try
            (* prepare data *)
            Data := TJSONObject(TJSONParser.Create(response).Parse);

            md5 := Data.Get('md5', '');
            if firstPage and (md5 <> '') and (md5 <> BrowseMd5[obj]) then
            begin
                Result.IsNew   := True;
                BrowseMd5[obj] := md5;
            end;

            response := Data.Get('data', '');
            if response <> '' then
            begin
                Data.Free;
                Data := TJSONObject(TJSONParser.Create(response).Parse);
            end;

            pageData := Data.Objects['pager'];
            pager.PageID := pageData.Get('pageID', 1);
            if pager.PageID = 1 then
                pager.PageID := StrToInt(pageData.Get('pageID', '1'));
            if firstPage then
            begin
                pager.status     := 'ok';
                pager.PerPage    := pageData.Get('recPerPage', 20);
                pager.PageTotal  := pageData.Get('pageTotal', 0);
                pager.PageCookie := pageData.Get('pageCookie', '');
                pager.Total      := pageData.Get('recTotal', 0);
            end;
            Result.Pager := pager;
            BrowsePagers[obj, objType] := pager;
            pageData.Free;

            Result.Data := Data;
        except
            Result.Result  := False;
            Result.Message := rsErrorDataReturned;
        end;
    except
        Result.Result  := False;
        Result.Message := rsErrorCannotConnect;
    end;

    DInfo('END: LoadDataList' + LineEnding );
end;

(* Get config *)
function GetConfig(): HandleResult;
var
    configStr: string;
begin
    Result.Result := True;
    try
        configStr := HttpGet(user.Url + '/index.php?mode=getconfig');
        if Length(configStr) > 0 then
        begin
            zentaoConfig := TJSONObject(TJSONParser.Create(configStr).Parse);
        end
        else
            Result.Result := False;
    except
        Result.Result := False;
    end;
    if not Result.Result then
        Result.Message := rsErrorCannotConnectZentao;
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
        Result.Message := Format(rsErrorNeedNewerVersion, [version, 'pro1.3']);
    end
    else if not isPro and (verNum < 4) then
    begin
        Result.Message := Format(rsErrorNeedNewerVersion, [version, '4.0']);
    end
    else
        Result.Result := True;
end;

(* Get API address *)
function GetAPI(const Params: array of const): string;
var
    config:  TJSONObject;
    viewType, requestType, moduleName, methodName, password, pageID: string;
    item:    TJSONData;
    i:       integer;
    key:     string;
    nameSet: TStringList;
begin
    config     := TJSONObject.Create(Params);
    viewType   := config.Get('viewType', 'json');
    moduleName := config.Get('module', '');
    methodName := config.Get('method', '');
    requestType:= LowerCase(zentaoConfig.Get('requestType', ''));

    if  requestType = 'get' then
    begin
        Result := user.Url + '/index.php?';
        if (moduleName = 'user') and (methodName = 'login') then
        begin
            password := MD5Print(MD5String(user.PassMd5 +
                IntToStr(session.Int64s['rand'])));
            Result   := Result + 'm=user&f=login&account=' + user.Account +
                '&password=' + password + '&' + session.Get('sessionName', '') +
                '=' + session.Get('sessionID', '') + '&t=json';
            config.Free;
            DInfo('GetAPI', Result);
            Exit;
        end;

        Result := Result + 'm=' + moduleName + '&f=' + methodName;

        if (moduleName = 'api') and (LowerCase(methodName) = 'getmodel') then
        begin
            Result  := Result + '&moduleName=' + config.Get('moduleName', '') +
                '&methodName=' + config.Get('methodName', '') + '&params=';
            nameSet := TStringList.Create;
            nameSet.CommaText := ',viewType,module,method,moduleName,methodName,pageID,type,recTotal,recPerPage,id,';
            for i := 0 to (config.Count - 1) do
            begin
                item := config.items[i];
                key  := config.Names[i];
                if (nameSet.indexOf(Key) > 0) then
                    continue;
                Result := Result + Key + '=' + item.AsString + '&';
            end;
            nameSet.Free;
        end;

        if moduleName = 'my' then
            Result := Result + '&type=' + config.Get('type', '');
        if methodName = 'view' then
            Result := Result + '&' + moduleName + 'ID=' + config.Get('id', '');

        pageID := config.Get('pageID', '');
        if pageID <> '' then
        begin
            if methodName = 'todo' then
            begin
                Result := Result + '&account=&status=all&orderBy=date_desc,status,begin&';
            end
            else
            begin
                Result := Result + '&&orderBy=id_desc&';
            end;
            
            Result := Result + 'recTotal=' + config.Get('recTotal', '') + '&recPerPage=' + config.Get('recPerPage', '') + '&pageID=' + pageID;
        end;

        Result := Result + '&t=' + viewType;

        if not session.Get('undefined', False) then
        begin
            Result := Result + '&' + session.Get('sessionName', '') +
                '=' + session.Get('sessionID', '');
        end;
    end
    else
    begin
        Result := Result + user.Url + '/';
        if (moduleName = 'user') and (methodName = 'login') then
        begin
            password := MD5Print(MD5String(user.PassMd5 +
                IntToStr(session.Int64s['rand'])));
            Result   := Result + 'user-login.json?account=' +
                user.Account + '&password=' + password + '&' +
                session.Get('sessionName', 'sid') + '=' + session.Get('sessionID', '');
            config.Free;
            DInfo('GetAPI', Result);
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

        nameSet           := TStringList.Create;
        nameSet.CommaText :=
            ',viewType,module,method,moduleName,methodName,pageID,type,recTotal,recPerPage,';
        for i := 0 to (config.Count - 1) do
        begin
            item := config.items[i];
            key  := config.Names[i];

            if (nameSet.indexOf(key) > 0) then
                continue;
            if (methodName <> 'view') or (key <> 'id') then
                Result := Result + key + '=';
            Result     := Result + item.AsString + '-';
        end;
        nameSet.Free;

        pageID := config.Get('pageID', '');
        if pageID <> '' then
        begin
            if methodName = 'todo' then
            begin
                Result := Result + '-all-date_desc,status,begin-';
            end
            else
            begin
                Result := Result + 'id_desc-';
            end;
            Result := Result + config.Get('recTotal', '') + '-' +
                config.Get('recPerPage', '') + '-' + pageID;
        end;

        if Result[Length(Result)] = '-' then
            Result := Copy(Result, 1, Length(Result) - 1);

        Result := Result + '.' + viewType;

        if not session.Get('undefined', False) then
        begin
            Result := Result + '?' + session.Get('sessionName', '') +
                '=' + session.Get('sessionID', '');
        end;
    end;
    config.Free;
    DInfo('GetAPI', Result);
end;

(* Get session *)
function GetSession: HandleResult;
var
    sessionStr: string;
begin
    Result.Result := True;
    try
        sessionStr := HttpGet(GetAPI(['module', 'api', 'method', 'getSessionID']));
        if Length(sessionStr) > 0 then
        begin
            if Assigned(session) then session.Free;
            session := TJSONObject(TJSONParser.Create(sessionStr).Parse);
            if session.Get('status', '') = 'success' then
            begin
                sessionStr := session.Get('data', '');
                session.Free;
                session    := TJSONObject(TJSONParser.Create(sessionStr).Parse);
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
        Result.Message := rsErrorCannotGetSession;
end;

(* Login *)
function Login(): HandleResult;
var
    response: string;
    status:   TJSONObject;
begin
    DInfo('BEGIN: Login');
    Result.Result := True;
    try
        response := HttpGet(GetAPI(['module', 'user', 'method', 'login']));
        if Length(response) > 0 then
        begin
            status := TJSONObject(TJSONParser.Create(response).Parse);
            if status.get('status', '') = 'failed' then
            begin
                Result.Result := False;
            end;
            status.Free;
        end
        else
            Result.Result := False;
    except
        Result.Result := False;
    end;
    if not Result.Result then
        Result.Message := rsErrorLoginFailed;
    DInfo('END: Login ||' + Result.Message);
end;

(* Get role *)
function GetRole(): HandleResult;
var
    response: string;
    role, roleValue:     TJSONObject;
begin
    Result.Result := True;
    try
        response := HttpGet(GetAPI(['module', 'api', 'method', 'getmodel',
            'moduleName', 'user', 'methodName', 'getById', 'account', user.Account]));
        if Length(response) > 0 then
        begin
            role := TJSONObject(TJSONParser.Create(response).Parse);
            if role.get('status', '') = 'failed' then
            begin
                Result.Result := False;
            end
            else
            begin
                roleValue      := TJSONObject(TJSONParser.Create(role.Get('data', '')).Parse);
                user.Role := roleValue.Get('role', '');
                roleValue.Free;
            end;
            role.Free;
        end
        else
            Result.Result := False;
    except
        Result.Result := False;
    end;

    if not Result.Result then
        Result.Message := rsErrorCannotGetRoleConfig;
end;

{ Logout }
function Logout(): HandleResult;
var
    url, response: string;
begin
    Result.Result := True;

    url := GetAPI(['module', 'user', 'method', 'logout']);

    try
        response := HttpGet(url);
    except
        Result.Message := 'Url: ' + url;
    end;
end;

(* Try login in *)
function TryLogin(): HandleResult;
begin
    if Assigned(session) then session.Free;
    session := TJSONObject.Create(['undefined', True]);

    Result := GetConfig();
    if not Result.Result then
        Exit;

    Result := GetSession();
    if not Result.Result then
        Exit;

    Result := Login();
    if not Result.Result then
        Exit;

    Result := GetRole();
    if not Result.Result then
        Exit;

    if Result.Result then
    begin
        SaveConfig;
    end;
end;

{ Load config }
function LoadConfig(): boolean;
var
    conf:          TJSONConfig;
    lastLoginTime: TDateTime;
begin
    Result := False;
    conf   := TJSONConfig.Create(nil);
    try
        conf.FileName := CONFIG_FILE;

        lastLoginTime := conf.GetValue('/LastLoginTime', 0);
        user.CloseOption:= conf.GetValue('/CloseOption', -1);

        if lastLoginTime > 0 then
        begin
            user.Account    := conf.GetValue('/User/Account', '');
            user.Url        := conf.GetValue('/User/Url', '');
            user.PassMd5    := conf.GetValue('/User/PassMd5', '');
            user.Role       := conf.GetValue('/User/Role', '');
            user.AutoSignIn := conf.GetValue('/User/AutoSignIn', False);
            user.Lang       := conf.GetValue('/User/Lang', 'zh_cn');
            user.RememberMe := True;
            Result          := True;
        end;
    finally
        conf.Free;
    end;
end;

{ Save config }
procedure SaveConfig();
var
    conf: TJSONConfig;
begin
    conf := TJSONConfig.Create(nil);
    try
        conf.FileName := CONFIG_FILE;

        if user.RememberMe then
        begin
            conf.SetValue('/User/Account', user.Account);
            conf.SetValue('/User/Url', user.Url);
            conf.SetValue('/User/PassMd5', user.PassMd5);
            conf.SetValue('/User/Role', user.Role);
            conf.SetValue('/LastLoginTime', Now);
            conf.SetValue('/User/AutoSignIn', user.AutoSignIn);
            conf.SetValue('/User/Lang', user.Lang);
            conf.SetValue('/CloseOption', user.CloseOption);
        end
        else
        begin
            conf.SetValue('/User/Account', '');
            conf.SetValue('/User/Url', '');
            conf.SetValue('/User/PassMd5', '');
            conf.SetValue('/User/Role', '');
            conf.SetValue('/LastLoginTime', 0);
            conf.SetValue('/User/AutoSignIn', False);
            conf.SetValue('/User/Lang', '');
            conf.SetValue('/CloseOption', -1);
        end;

        conf.Flush;
    finally
        conf.Free;
    end;
end;

function HttpGet(url: string): string;
begin
    With TFPHttpClient.Create(Nil) do
    try
      Result := Get(url);
    finally
      Free;
    end;
end;

{ Init zentao API }
procedure InitZentaoAPI();
begin
    DInfo('BEGIN: InitZentaoAPI');
    if Assigned(session) then session.Free;
    session := TJSONObject.Create(['undefined', True]);

    (* init browsename *)
    BrowseName[btTodo]  := 'todo';
    BrowseName[btTask]  := 'task';
    BrowseName[btBug]   := 'bug';
    BrowseName[btStory] := 'story';

    BrowseNames[btTodo]  := 'todos';
    BrowseNames[btTask]  := 'tasks';
    BrowseNames[btBug]   := 'bugs';
    BrowseNames[btStory] := 'stories';

    BrowseTypes[0] := btTodo;
    BrowseTypes[1] := btTask;
    BrowseTypes[2] := btBug;
    BrowseTypes[3] := btStory;

    BrowseSubName[today]      := 'today';
    BrowseSubName[yesterday]  := 'yesterday';
    BrowseSubName[thisweek]   := 'thisweek';
    BrowseSubName[lastweek]   := 'lastweek';
    BrowseSubName[assignedTo] := 'assignedTo';
    BrowseSubName[openedBy]   := 'openedBy';
    BrowseSubName[reviewedBy] := 'reviewedBy';
    BrowseSubName[closedBy]   := 'closedBy';
    BrowseSubName[finishedBy] := 'finishedBy';
    BrowseSubName[resolvedBy] := 'resolvedBy';

    BrowseSubTypes[0] := today;
    BrowseSubTypes[1] := yesterday;
    BrowseSubTypes[2] := thisweek;
    BrowseSubTypes[3] := lastweek;
    BrowseSubTypes[4] := assignedTo;
    BrowseSubTypes[5] := openedBy;
    BrowseSubTypes[6] := reviewedBy;
    BrowseSubTypes[7] := closedBy;
    BrowseSubTypes[8] := finishedBy;
    BrowseSubTypes[9] := resolvedBy;

    DInfo('END: InitZentaoAPI');
end;

{ Destory resources }
procedure DestroyZentaoAPI();
begin
    DInfo('BEGIN: DestroyZentaoAPI');

    zentaoConfig.Free;
    session.Free;
    PopWindowData.Free;

    DInfo('END: DestroyZentaoAPI');
end;

{ Get max one between two intergers }
function Max(a: integer; b: integer): integer;
begin
    if a > b then
    begin
        Result := a;
    end
    else
    begin
        Result := b;
    end;
end;

{ Get min one between two intergers }
function Min(a: integer; b: integer): integer;
begin
    if a < b then
    begin
        Result := a;
    end
    else
    begin
        Result := b;
    end;
end;

{ Get build version }
function GetBuildVersion(formatStr: string = '%d.%d.%d'): string;
begin
    if formatStr = '%s' then
    begin
        Result := 'Release';
    end
    else
    begin
        Result := Format(formatStr, [2, 1, 2, 0]);
    end;
    // todo: get the real version info with os api.

end;

end.
