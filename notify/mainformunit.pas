unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, synhighlighterunixshellscript, RTTICtrls,
    Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList,
    Grids, EditBtn, Menus,
    fpjson, jsonparser,
    Clipbrd, LResources,
    ZentaoAPIUnit,
    BackgroundWorkerUnit;

type

    { TMainForm }

    TMainForm = class(TForm)
        Image1: TImage;
        Image2: TImage;
        Image3: TImage;
        ImageListPopupMenu: TImageList;
        LabelTodoSepLine: TLabel;
        LabelLoadingProgressbar: TLabel;
        LabelMessageClose: TLabel;
        LabelMessage: TLabel;
        LabelPagerTaskInfo: TLabel;
        LabelPagerBugInfo: TLabel;
        LabelPagerStoryInfo: TLabel;
        LabelPagerTodoInfo: TLabel;
        LabelPagerTaskLast: TLabel;
        LabelPagerBugLast: TLabel;
        LabelPagerStoryLast: TLabel;
        LabelPagerTodoLast: TLabel;
        LabelPagerTaskNext: TLabel;
        LabelPagerBugNext: TLabel;
        LabelPagerStoryNext: TLabel;
        LabelPagerTodoNext: TLabel;
        LabelPagerTaskPrev: TLabel;
        LabelPagerBugPrev: TLabel;
        LabelPagerStoryPrev: TLabel;
        LabelPagerTodoPrev: TLabel;
        LabelPopupMenuBtnSync: TLabel;
        LabelPopupMenuBtnSep: TLabel;
        LabelMenu10: TLabel;
        LabelMenu11: TLabel;
        LabelMenu12: TLabel;
        LabelMenu13: TLabel;
        LabelMenu14: TLabel;
        LabelMenu15: TLabel;
        LabelMenu16: TLabel;
        LabelMenu2: TLabel;
        LabelMenu3: TLabel;
        LabelMenu4: TLabel;
        Label2:          TLabel;
        LabelMenu1: TLabel;
        LabelMenu5: TLabel;
        LabelMenu6: TLabel;
        LabelMenu7: TLabel;
        LabelMenu8: TLabel;
        LabelMenu9: TLabel;
        LabelPopupMenuBtnLogout: TLabel;
        LabelPopupMenuBtnExit: TLabel;
        LabelTab3:       TLabel;
        LabelTab1:       TLabel;
        LabelTab2:       TLabel;
        LabelMenuIcon: TLabel;
        Memo1:           TMemo;
        MenuItemCopy: TMenuItem;
        MenuItemReloadTab: TMenuItem;
        MenuItemSep: TMenuItem;
        MenuItemViewObject: TMenuItem;
        PanelMessage: TPanel;
        PanelPagerTask: TPanel;
        PanelPagerBug: TPanel;
        PanelPagerStory: TPanel;
        PanelPagerTodo: TPanel;
        PanelPopupMenu: TPanel;
        PanelMenuBug: TPanel;
        PanelMenuStory: TPanel;
        PanelMenuTodo: TPanel;
        PanelMenu:       TPanel;
        PanelMenuTask: TPanel;
        PanelNavTodo:    TPanel;
        PanelNavTask:    TPanel;
        PanelNavBug:     TPanel;
        PanelNavStory:   TPanel;
        PopupMenuStringGrid: TPopupMenu;
        ShapeMenuIcon1: TShape;
        ShapeMenuIcon2: TShape;
        ShapeMenuIcon3: TShape;
        StringGridBug:   TStringGrid;
        StringGridStory: TStringGrid;
        StringGridTodo:  TStringGrid;
        StringGridTask:  TStringGrid;
        TimerAutoSync: TTimer;
        TimerLoadingAnimate: TTimer;
        procedure FormClick(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure LabelBtnMouseLeave(Sender: TObject);
        procedure LabelBtnMouseEnter(Sender: TObject);
        procedure LabelMenuClick(Sender: TObject);
        procedure LabelMenuIconClick(Sender: TObject);
        procedure LabelMenuIconMouseEnter(Sender: TObject);
        procedure LabelMenuIconMouseLeave(Sender: TObject);
        procedure LabelMenuMouseEnter(Sender: TObject);
        procedure LabelMenuMouseLeave(Sender: TObject);
        procedure LabelMessageCloseClick(Sender: TObject);
        procedure LabelPagerBtnClick(Sender: TObject);
        procedure LabelPagerPrevMouseEnter(Sender: TObject);
        procedure LabelPagerPrevMouseLeave(Sender: TObject);
        procedure LabelPopupMenuBtnExitClick(Sender: TObject);
        procedure LabelPopupMenuBtnLogoutClick(Sender: TObject);
        procedure LabelPopupMenuBtnMouseEnter(Sender: TObject);
        procedure LabelPopupMenuBtnMouseLeave(Sender: TObject);
        procedure LabelPopupMenuBtnSyncClick(Sender: TObject);
        procedure LabelTabMouseEnter(Sender: TObject);
        procedure LabelTabMouseLeave(Sender: TObject);
        procedure LabelTabClick(Sender: TObject);
        procedure MenuItemCopyClick(Sender: TObject);
        procedure MenuItemReloadTabClick(Sender: TObject);
        procedure MenuItemViewObjectClick(Sender: TObject);
        procedure PanelMenuClick(Sender: TObject);
        procedure PanelMessageClick(Sender: TObject);
        procedure PanelPopupMenuClick(Sender: TObject);
        procedure ShowMessage(Message: string; msgType: string = 'danger');
        procedure HideMessage();
        procedure LoadTabData(tabName: BrowseType; pageID: string = '');
        procedure LoadTabDataCompleted(e: TRunWorkerCompletedEventArgs);
        function LoadingTabData(arg: TObject):TRunWorkerCompletedEventArgs;
        procedure StringGridDblClick(Sender: TObject);
        procedure StringGridMouseDown(Sender: TObject;
            Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure StringGridTodoSelectCell(Sender: TObject; aCol,
            aRow: Integer; var CanSelect: Boolean);
        procedure TimerAutoSyncTimer(Sender: TObject);
        procedure TimerLoadingAnimateStartTimer(Sender: TObject);
        procedure TimerLoadingAnimateStopTimer(Sender: TObject);
        procedure TimerLoadingAnimateTimer(Sender: TObject);
        procedure TryLoadTabData(tabName: BrowseType);
        procedure InitTabMenu();
        procedure InitSubMenu();
        procedure HidePopup();overload;
        procedure HidePopup(Sender: TObject);overload;
        procedure LoadAllTabsData();
        procedure ShowPager(pager: PageRecord; tab: BrowseType);
    
    private
        procedure LoadTab(dataResult: TDataResult; tab: BrowseType);

    end;

    TLoadDataListArgs = class(TObject)
        Tab : BrowseType;
        SubType : BrowseSubType;
        PageID : string;
    end;

const
    TryLoadTabInterval = 5.0 / (24 * 60);
    ONEDAYMILLIONSECONDS = 24 * 60 * 60 * 1000;

var
    MainForm:      TMainForm;
    CurrentTab:    BrowseType;
    CurrentItemId: string;
    FirstShow:     boolean;
    LastSyncTime:  array[BrowseType] of TDateTime;
    ActiveSubMenu: array[BrowseType] of BrowseSubType;
    StringGrids:   array[BrowseType] of TStringGrid;
    BrowseTrack:   array[BrowseType] of TStringList;
    TabGroup:      array[1..3] of BrowseType;
    IsTabLoading: boolean;
    AverageWaitingTime: Double; // days
    StartLoadingTime, StopLoadingTime: TDateTime;

implementation

uses LoginFormUnit;

{$R *.lfm}

{ TMainForm }
procedure TMainForm.LoadAllTabsData();
begin
    LastSyncTime[btStory] := 0;
    LastSyncTime[btBug] := 0;
    LastSyncTime[btStory] := 0;
    LastSyncTime[btTodo] := 0;
    LoadTabData(CurrentTab);
end;

procedure TMainForm.HidePopup(Sender: TObject);overload;
begin
    HidePopup;
end;

procedure TMainForm.HidePopup();overload;
begin
    PanelPopupMenu.Visible := False;
    HideMessage;
end;

procedure TMainForm.TryLoadTabData(tabName: BrowseType);
begin
    if (Now - LastSyncTime[tabName]) > TryLoadTabInterval then
    begin
        LoadTabData(tabName);
    end;
end;

(* Load tab data list with a given browse type *)
procedure TMainForm.LoadTabData(tabName: BrowseType; pageID: string = '');
var
    dataLoader: TBackgroundWorker;
    dataLoaderArgs : TLoadDataListArgs;
begin
    if IsTabLoading then
    begin
        ShowMessage('应用正忙，请稍后再试。', 'warning');
        Exit;
    end;

    dataLoaderArgs := TLoadDataListArgs.Create;
    dataLoaderArgs.Tab := tabName;
    dataLoaderArgs.SubType := ActiveSubMenu[tabName];
    dataLoaderArgs.PageID := pageID;

    IsTabLoading := True;
    TimerLoadingAnimate.Enabled:= true;

    dataLoader := TBackgroundWorker.Create(@LoadingTabData, @LoadTabDataCompleted, True);
    dataLoader.RunWorkerAsync(dataLoaderArgs);
end;

procedure TMainForm.LoadTabDataCompleted(e: TRunWorkerCompletedEventArgs);
var
    data: TDataResult;
    tabName: BrowseType;
begin
    data := e.Target as TDataResult;
    tabName := BrowseTypes[e.Tag];

    IsTabLoading := False;
    StopLoadingTime := Now;

    LoadTab(data, tabName);

    data.Free;
end;

function TMainForm.LoadingTabData(arg: TObject):TRunWorkerCompletedEventArgs;
var
    data: TDataResult;
    dataLoaderArgs : TLoadDataListArgs;
begin
    dataLoaderArgs := arg as TLoadDataListArgs;
    data := LoadDataList(dataLoaderArgs.Tab, dataLoaderArgs.SubType, dataLoaderArgs.PageID);

    Result.Tag := Ord(dataLoaderArgs.tab);
    Result.Result := data.Result;
    Result.Message := data.Message;
    Result.Target := data;

    dataLoaderArgs.Free;
end;

procedure TMainForm.StringGridDblClick(Sender: TObject);
var
    stringGridSender: TStringGrid;
    tab: BrowseType;
begin
    stringGridSender := Sender as TStringGrid;
    tab := BrowseTypes[stringGridSender.Tag];
    ViewObject(tab, stringGridSender.Cells[0, stringGridSender.Selection.Bottom]);
end;

procedure TMainForm.StringGridMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
    nACol, nARow: Integer;
    stringGridSender: TStringGrid;
begin
    stringGridSender := Sender as TStringGrid;
    if Button = mbRight then
    begin
        stringGridSender.MouseToCell(X, Y, nACol, nARow);
        stringGridSender.Col := nACol;
        stringGridSender.Row := nARow;

        if nARow >= 0 then
        begin
            CurrentItemId := stringGridSender.Cells[0, nARow];
            PopupMenuStringGrid.Popup;
        end;
    end;
end;

procedure TMainForm.StringGridTodoSelectCell(Sender: TObject; aCol,
    aRow: Integer; var CanSelect: Boolean);
begin
end;

procedure TMainForm.TimerAutoSyncTimer(Sender: TObject);
var i:Integer;
begin
    for i := 1 to High(TabGroup) do
    begin
        if (Now - LastSyncTime[TabGroup[i]]) > ((2 * 60 * 1000) / ONEDAYMILLIONSECONDS) then
        begin
            LoadTabData(TabGroup[i]);
            ShowMessage('自动同步于： ' + FormatDateTime('YYYY-MM-DD HH:NN:SS', Now));
            break;
        end;
    end;
end;

procedure TMainForm.TimerLoadingAnimateStartTimer(Sender: TObject);
begin
    StartLoadingTime := Now;
    IsTabLoading := True;
    LabelLoadingProgressbar.Width := 0;
    LabelLoadingProgressbar.Visible := True;
end;

procedure TMainForm.TimerLoadingAnimateStopTimer(Sender: TObject);
begin
    AverageWaitingTime := 0.8 * AverageWaitingTime + 0.2 * (StopLoadingTime - StartLoadingTime);
    LabelLoadingProgressbar.Visible := False;
end;

procedure TMainForm.TimerLoadingAnimateTimer(Sender: TObject);
var
    n : TDateTime;
    d : Double;
    w : Int64;
    nd : Double;
begin
    n := Now;
    d := n - StartLoadingTime;
    w := Trunc(Width * (d/AverageWaitingTime));

    if IsTabLoading then
    begin
        LabelLoadingProgressbar.Width := Min(Width - 100, w);
    end
    else
    begin
        LabelLoadingProgressbar.Width := Min(Width, LabelLoadingProgressbar.Width + Trunc((Width - LabelLoadingProgressbar.Width) / 9));
        nd := (n-StopLoadingTime);

        if nd > (420 / ONEDAYMILLIONSECONDS) then
            LabelLoadingProgressbar.Width := Width;

        if nd > (500 / ONEDAYMILLIONSECONDS) then
        begin
            TimerLoadingAnimate.Enabled := False;
        end;
    end;
end;

procedure TMainForm.LoadTab(dataResult: TDataResult; tab: BrowseType);
var
    Data, dataItem: TJSONObject;
    dataList: TJSONArray;
    dataRow:  TJSONEnum;
    index:    integer;
    track: TStringList;
    stringGrid: TStringGrid;
    noTrack: boolean;
    id: string;
begin
    if dataResult.Result then
    begin
        Data     := dataResult.Data;
        dataList := Data.Arrays[BrowseNames[tab]];
        stringGrid := StringGrids[tab];

        track    := BrowseTrack[tab];
        noTrack  := track.indexOf('#') < 0;
        if noTrack then track.Add('#');

        (* clean all cells *)
        stringGrid.Clean;
        stringGrid.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index    := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            id := dataItem.Get('id', '');
            stringGrid.RowCount := index;
            stringGrid.Cells[0, index - 1] := '#' + id;
            stringGrid.Cells[1, index - 1] := dataItem.Get('name', '-');
            stringGrid.Cells[2, index - 1] := dataItem.Get('status', '-');

            if noTrack then
            begin
                track.Add(id);
            end
            else if track.indexOf(id) < 0 then
            begin
                track.Add(id);
                dataItem.Booleans['new'] := True;
            end;

            if dataItem.Get('new', False) then
            begin
                stringGrid.Cells[0, index - 1] := '+' + id;
            end;
        end;

        ShowPager(dataResult.Pager, tab);

        LastSyncTime[btTodo] := Now;
        BrowseTrack[btTodo] := track;
    end
    else
    begin
        ShowMessage(dataResult.Message);
    end;
end;

procedure TMainForm.ShowPager(pager: PageRecord; tab: BrowseType);
var
    labelPagerInfo, labelPagerPrev, labelPagerNext, labelPagerLast: TLabel;
begin
    case tab of
        btTodo:
        begin
            labelPagerInfo := LabelPagerTodoInfo;
            labelPagerPrev := LabelPagerTodoPrev;
            labelPagerNext := LabelPagerTodoNext;
            labelPagerLast := LabelPagerTodoLast;
        end;
        btStory:
        begin
            labelPagerInfo := LabelPagerStoryInfo;
            labelPagerPrev := LabelPagerStoryPrev;
            labelPagerNext := LabelPagerStoryNext;
            labelPagerLast := LabelPagerStoryLast;
        end;
        btBug:
        begin
            labelPagerInfo := LabelPagerBugInfo;
            labelPagerPrev := LabelPagerBugPrev;
            labelPagerNext := LabelPagerBugNext;
            labelPagerLast := LabelPagerBugLast;
        end;
        btTask:
        begin
            labelPagerInfo := LabelPagerTaskInfo;
            labelPagerPrev := LabelPagerTaskPrev;
            labelPagerNext := LabelPagerTaskNext;
            labelPagerLast := LabelPagerTaskLast;
        end;
        else 
        begin
            Exit;
        end;
    end;

    labelPagerInfo.Visible := False;
    labelPagerPrev.Visible := False;
    labelPagerNext.Visible := False;
    labelPagerLast.Visible := False;
    if pager.Total > 0 then
    begin
        labelPagerInfo.Caption := Format('第 %d - %d 条，共 %d 条', [Min(pager.Total,(pager.PageID - 1) * pager.PerPage + 1), Min(pager.Total, pager.PageID * pager.PerPage), pager.Total]);
        labelPagerPrev.Visible := True;
        labelPagerInfo.Visible := True;
        labelPagerNext.Visible := True;
        labelPagerLast.Visible := True;
        labelPagerPrev.Enabled := pager.PageID > 1;
        labelPagerNext.Enabled := pager.PageID < pager.PageTotal;
        labelPagerLast.Enabled := pager.PageTotal > 1;
        if pager.PageID = pager.PageTotal then
        begin
            labelPagerLast.Caption := '首页';
            labelPagerLast.Hint := 'first';
        end
        else
        begin
            labelPagerLast.Caption := '末页';
            labelPagerLast.Hint := 'last';
        end;
    end
    else
    begin
        labelPagerInfo.Caption := '暂时没有数据。';
        labelPagerInfo.Visible := True;
    end;
end;


procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    // Destroy;
    Logout;
    BrowseTrack[TabGroup[0]].Free;
    BrowseTrack[TabGroup[1]].Free;
    BrowseTrack[TabGroup[2]].Free;
    LoginForm.Close;
end;

procedure TMainForm.FormClick(Sender: TObject);
begin
    HidePopup;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    FirstShow := True;
    AverageWaitingTime := 2000 / ONEDAYMILLIONSECONDS; // 2 seconds
end;

procedure TMainForm.InitTabMenu();
begin
    TabGroup[1] := btTodo;
    BrowseTrack[btTodo] := TStringList.Create;
    BrowseTrack[btBug] := TStringList.Create;
    if (user.Role = 'qa') or (user.Role = 'qd') then
    begin
        LabelTab2.Caption := 'Bug';
        LabelTab2.Hint    := 'bug';
        LabelTab2.Tag     := 2;
        LabelTab3.Caption := '任务';
        LabelTab3.Hint    := 'task';
        LabelTab3.Tag     := 1;

        TabGroup[2] := btBug;
        TabGroup[3] := btTask;
        BrowseTrack[btTask] := TStringList.Create;
    end
    else if (user.Role = 'po') or (user.Role = 'pd') then
    begin
        LabelTab2.Caption := '需求';
        LabelTab2.Hint    := 'story';
        LabelTab2.Tag     := 3;
        LabelTab3.Caption := 'Bug';
        LabelTab3.Hint    := 'bug';
        LabelTab3.Tag     := 2;

        TabGroup[2] := btStory;
        TabGroup[3] := btBug;
        BrowseTrack[btStory] := TStringList.Create;
    end;

    IsTabLoading := False;

    StringGrids[btTask] := StringGridTask;
    StringGrids[btTodo] := StringGridTodo;
    StringGrids[btStory] := StringGridStory;
    StringGrids[btBug] := StringGridBug;
end;

procedure TMainForm.InitSubMenu();
begin
    ActiveSubMenu[btTask] := assignedTo;
    ActiveSubMenu[btStory] := assignedTo;
    ActiveSubMenu[btTodo] := today;
    ActiveSubMenu[btBug] := assignedTo;
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
    if FirstShow then
    begin
        InitTabMenu;
        InitSubMenu;

        (* Load all data *)
        LoadAllTabsData;

        FirstShow := False;
    end;
end;

procedure TMainForm.LabelBtnMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := 16547890;
end;

procedure TMainForm.LabelBtnMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := 13392660;
end;

procedure TMainForm.LabelMenuClick(Sender: TObject);
var
    labelSender : TLabel;
    tab         : BrowseType;
    menuParent  : TPanel;
    subMenu     : BrowseSubType;
    i           : integer;
begin
    HidePopup;

    labelSender := Sender as TLabel;
    menuParent  := labelSender.Parent as TPanel;
    tab         := BrowseTypes[menuParent.Tag];
    subMenu     := BrowseSubTypes[labelSender.Tag];

    if tab <> CurrentTab then
    begin
        // todo: changed tab
    end;

    if ActiveSubMenu[tab] <> subMenu then
    begin
        ActiveSubMenu[tab] := subMenu;

        for i := 0 to menuParent.ControlCount - 1 do
        begin
            if (menuParent.Controls[i] is TLabel) then
                (menuParent.Controls[i] as TLabel).Font.Color := $005D5D5D;
        end;
        labelSender.Font.Color := 13392660;
    end;
    
    LoadTabData(tab);
end;

procedure TMainForm.LabelMenuIconClick(Sender: TObject);
begin
    PanelPopupMenu.Top := 50;
    PanelPopupMenu.Visible := (not PanelPopupMenu.Visible);
    HideMessage;
end;

procedure TMainForm.LabelMenuIconMouseEnter(Sender: TObject);
begin
    LabelMenuIcon.Color := 16547890;
    ShapeMenuIcon1.Brush.Color := clWhite;
    ShapeMenuIcon2.Brush.Color := clWhite;
    ShapeMenuIcon3.Brush.Color := clWhite;
end;

procedure TMainForm.LabelMenuIconMouseLeave(Sender: TObject);
begin
    LabelMenuIcon.Color := $00CC5B14;
    ShapeMenuIcon1.Brush.Color := clSilver;
    ShapeMenuIcon2.Brush.Color := clSilver;
    ShapeMenuIcon3.Brush.Color := clSilver;
end;

procedure TMainForm.LabelMenuMouseEnter(Sender: TObject);
var
    labelSender : TLabel;
    tab         : BrowseType;
    menuParent  : TPanel;
begin
    labelSender       := Sender as TLabel;
    menuParent        := labelSender.Parent as TPanel;
    tab               := BrowseTypes[menuParent.Tag];
    if BrowseSubTypes[labelSender.Tag] <> ActiveSubMenu[tab] then
    begin
        labelSender.Font.Color := $00141414;
    end;
end;

procedure TMainForm.LabelMenuMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
    tab         : BrowseType;
    menuParent  : TPanel;
begin
    labelSender       := Sender as TLabel;
    menuParent        := labelSender.Parent as TPanel;
    tab               := BrowseTypes[menuParent.Tag];
    if BrowseSubTypes[labelSender.Tag] <> ActiveSubMenu[tab] then
    begin
        labelSender.Font.Color := $005D5D5D;
    end;
end;

procedure TMainForm.LabelMessageCloseClick(Sender: TObject);
begin
    HideMessage;
end;

procedure TMainForm.LabelPagerBtnClick(Sender: TObject);
var
    labelSender: TLabel;
    tab        : BrowseType;
begin
    labelSender       := Sender as TLabel;
    tab := BrowseTypes[(labelSender.Parent as TPanel).Tag];
    LoadTabData(tab, labelSender.Hint);
end;

procedure TMainForm.LabelPagerPrevMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Font.Color := $00CC5B14;

end;

procedure TMainForm.LabelPagerPrevMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Font.Color := $00FC8032;

end;

procedure TMainForm.LabelPopupMenuBtnExitClick(Sender: TObject);
begin
    PanelPopupMenu.Visible := False;

    Close;
end;

procedure TMainForm.LabelPopupMenuBtnLogoutClick(Sender: TObject);
var
    r: HandleResult;
begin
    PanelPopupMenu.Visible := False;

    r := Logout;

    if r.Result then
    begin
        LoginForm.Show;
        MainForm.Hide;
        FirstShow := True;
    end
    else
    begin
        ShowMessage(r.Message, 'danger');
    end;
end;

procedure TMainForm.LabelPopupMenuBtnMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := 16547890;
    labelSender.Font.Color := clWhite;
end;

procedure TMainForm.LabelPopupMenuBtnMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := clNone;
    labelSender.Font.Color := clDefault;

end;

procedure TMainForm.LabelPopupMenuBtnSyncClick(Sender: TObject);
begin
    LoadAllTabsData;
    PanelPopupMenu.Visible := False;
end;

procedure TMainForm.LabelTabMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender := Sender as TLabel;
    if BrowseTypes[labelSender.Tag] <> CurrentTab then
    begin
        labelSender.Color := 16547890;
    end;
end;

procedure TMainForm.LabelTabMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender := Sender as TLabel;
    if BrowseTypes[labelSender.Tag] <> CurrentTab then
    begin
        labelSender.Color := 13392660;
    end;
end;

procedure TMainForm.LabelTabClick(Sender: TObject);
var
    labelSender: TLabel;
    tab:         BrowseType;
begin
    HidePopup;

    labelSender := Sender as TLabel;
    tab         := BrowseTypes[labelSender.Tag];

    if tab <> CurrentTab then
    begin
        (* reset tab label color *)
        LabelTab1.Color      := 13392660;
        LabelTab1.Font.Color := clWhite;
        LabelTab2.Color      := 13392660;
        LabelTab2.Font.Color := clWhite;
        LabelTab3.Color      := 13392660;
        LabelTab3.Font.Color := clWhite;

        (* changed current tab color *)
        labelSender.Color := 16380651;
        labelSender.Font.Color := 13392660;
        CurrentTab := tab;

        (* hide other panel *)
        PanelNavTodo.Visible  := False;
        PanelNavTask.Visible  := False;
        PanelNavBug.Visible   := False;
        PanelNavStory.Visible := False;

        case tab of
            btTodo:
            begin
                PanelNavTodo.Visible := True;
                PanelNavTodo.Left    := 0;
            end;
            btTask:
            begin
                PanelNavTask.Visible := True;
                PanelNavTask.Left    := 0;
            end;
            btBug:
            begin
                PanelNavBug.Visible := True;
                PanelNavBug.Left    := 0;
            end;
            btStory:
            begin
                PanelNavStory.Visible := True;
                PanelNavStory.Left    := 0;
            end;
        end;

        TryLoadTabData(tab);
    end
    else
    begin
        LoadTabData(tab);
    end;
end;

procedure TMainForm.MenuItemCopyClick(Sender: TObject);
begin
end;

procedure TMainForm.MenuItemReloadTabClick(Sender: TObject);
begin
    LoadTabData(CurrentTab);
end;

procedure TMainForm.MenuItemViewObjectClick(Sender: TObject);
begin
    ViewObject(CurrentTab, CurrentItemId);
end;

procedure TMainForm.PanelMenuClick(Sender: TObject);
begin
    HidePopup;
end;

procedure TMainForm.PanelMessageClick(Sender: TObject);
begin

end;

procedure TMainForm.PanelPopupMenuClick(Sender: TObject);
begin

end;

(* Hide result message *)
procedure TMainForm.HideMessage();
begin
    PanelMessage.Visible := False;
end;

procedure TMainForm.ShowMessage(Message: string; msgType: string = 'danger');
begin
    LabelMessage.Caption := Message;
    PanelMessage.Visible := True;
    PanelMessage.Top := 50;

    case Lowercase(msgType) of
        'success':
        begin
            PanelMessage.Color := $00E6FFE5;
            PanelMessage.Font.Color := $00249F23;
        end;
        'danger':
        begin
            PanelMessage.Color := $00e5E6ff;
            PanelMessage.Font.Color := $002D32D2;
        end;
        'warning':
        begin
            PanelMessage.Color := $00E5F4FF;
            PanelMessage.Font.Color := $000086E4;
        end;
        'info':
        begin
            PanelMessage.Color := $00FFF9E5;
            PanelMessage.Font.Color := $00D7B339;
        end;
        'important':
        begin
            PanelMessage.Color := $00E5F3FF;
            PanelMessage.Font.Color := $001C5181;
        end;
        'special':
        begin
            PanelMessage.Color := $00FFE5F7;
            PanelMessage.Font.Color := $00A15789;
        end;
        else 
        begin
            PanelMessage.Color := $00F1F1F1;
            PanelMessage.Font.Color := $00333333;
        end;
    end;

    Memo1.Top := 0;
    Memo1.Visible := True;
    Memo1.Lines.Text    := Memo1.Lines.Text + Message + LineEnding;
end;

end.
