unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, synhighlighterunixshellscript, RTTICtrls,
    Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList,
    Grids, EditBtn, Menus,
    fpjson, jsonparser,
    Clipbrd, LResources,
    LCLIntf,
    ZentaoAPIUnit,
    PopWindowUnit,
    AboutUnit,
    LocalizedForms,
    DefaultTranslator,
    StringsUnit,
    CloseConfirmFormUnit,
    BackgroundWorkerUnit, types;

type

    { TMainForm }

    TMainForm = class(TLocalizedForm)
        ImageListPopupMenu: TImageList;
        LabelMenu17: TLabel;
        LabelMenu18: TLabel;
        LabelMenu19: TLabel;
        LabelTab4: TLabel;
        LabelTodoSepLine: TLabel;
        LabelLoadingProgressbar: TLabel;
        LabelMessageClose: TLabel;
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
        LabelMenu10:     TLabel;
        LabelMenu11:     TLabel;
        LabelMenu12:     TLabel;
        LabelMenu13:     TLabel;
        LabelMenu14:     TLabel;
        LabelMenu15:     TLabel;
        LabelMenu16:     TLabel;
        LabelMenu2:      TLabel;
        LabelMenu3:      TLabel;
        LabelMenu4:      TLabel;
        Label2:          TLabel;
        LabelMenu1:      TLabel;
        LabelMenu5:      TLabel;
        LabelMenu6:      TLabel;
        LabelMenu7:      TLabel;
        LabelMenu8:      TLabel;
        LabelMenu9:      TLabel;
        LabelTab3:       TLabel;
        LabelTab1:       TLabel;
        LabelTab2:       TLabel;
        LabelMenuIcon:   TLabel;
        LabelTodoSepLine1: TLabel;
        LabelTodoSepLine2: TLabel;
        LabelTodoSepLine3: TLabel;
        Memo1:           TMemo;
        MemoMessager: TMemo;
        MenuItem1:       TMenuItem;
        MenuItem4:       TMenuItem;
        MenuItem5:       TMenuItem;
        MenuItem6:       TMenuItem;
        MenuItem7:       TMenuItem;
        MenuItemAbout:   TMenuItem;
        MenuItem3:       TMenuItem;
        MenuItemExit1:   TMenuItem;
        MenuItemLogout1: TMenuItem;
        MenuItemOpenWebsite: TMenuItem;
        MenuItemOpenWebsite1: TMenuItem;
        MenuItemSyncAll: TMenuItem;
        MenuItem2:       TMenuItem;
        MenuItemLogout:  TMenuItem;
        MenuItemExit:    TMenuItem;
        MenuItemOpen:    TMenuItem;
        MenuItemCopy:    TMenuItem;
        MenuItemReloadTab: TMenuItem;
        MenuItemSep:     TMenuItem;
        MenuItemSyncAll1: TMenuItem;
        MenuItemViewObject: TMenuItem;
        PanelMessage:    TPanel;
        PanelPagerTask:  TPanel;
        PanelPagerBug:   TPanel;
        PanelPagerStory: TPanel;
        PanelPagerTodo:  TPanel;
        PanelMenuBug:    TPanel;
        PanelMenuStory:  TPanel;
        PanelMenuTodo:   TPanel;
        PanelMenu:       TPanel;
        PanelMenuTask:   TPanel;
        PanelNavTodo:    TPanel;
        PanelNavTask:    TPanel;
        PanelNavBug:     TPanel;
        PanelNavStory:   TPanel;
        PopupMenuMain:   TPopupMenu;
        PopupMenuNav:    TPopupMenu;
        PopupMenuStringGrid: TPopupMenu;
        ShapeMenuIcon1:  TShape;
        ShapeMenuIcon2:  TShape;
        ShapeMenuIcon3:  TShape;
        StringGridBug:   TStringGrid;
        StringGridStory: TStringGrid;
        StringGridTodo:  TStringGrid;
        StringGridTask:  TStringGrid;
        TimerAutoSync:   TTimer;
        TimerLoadingAnimate: TTimer;
        TrayIconMain:    TTrayIcon;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure FormWindowStateChange(Sender: TObject);
        procedure LabelBtnMouseLeave(Sender: TObject);
        procedure LabelBtnMouseEnter(Sender: TObject);
        procedure LabelMenu18Click(Sender: TObject);
        procedure LabelMenu19Click(Sender: TObject);
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
        procedure MenuItemLangClick(Sender: TObject);
        procedure OnShowAbout(Sender: TObject);
        procedure OnExit(Sender: TObject);
        procedure LabelPopupMenuBtnLogoutClick(Sender: TObject);
        procedure LabelPopupMenuBtnMouseEnter(Sender: TObject);
        procedure LabelPopupMenuBtnMouseLeave(Sender: TObject);
        procedure LabelPopupMenuBtnOpenWebsiteClick(Sender: TObject);
        procedure LabelPopupMenuBtnSyncClick(Sender: TObject);
        procedure LabelTabMouseEnter(Sender: TObject);
        procedure LabelTabMouseLeave(Sender: TObject);
        procedure LabelTabClick(Sender: TObject);
        procedure MenuItemOpenClick(Sender: TObject);
        procedure MenuItemReloadTabClick(Sender: TObject);
        procedure MenuItemViewObjectClick(Sender: TObject);
        procedure DisplayMessage(Message: string; msgType: string = 'danger');
        procedure HideMessage();
        procedure LoadTabData(tabName: BrowseType; pageID: string = '';
            mute: boolean = False);
        procedure LoadTabDataCompleted(e: TRunWorkerCompletedEventArgs);
        function LoadingTabData(arg: TObject): TRunWorkerCompletedEventArgs;
        procedure StringGridDblClick(Sender: TObject);
        procedure StringGridMouseDown(Sender: TObject;
            Button: TMouseButton; Shift: TShiftState; X, Y: integer);
        procedure StringGridTodoSelectCell(Sender: TObject;
            aCol, aRow: integer; var CanSelect: boolean);
        procedure TimerAutoSyncTimer(Sender: TObject);
        procedure TimerLoadingAnimateStartTimer(Sender: TObject);
        procedure TimerLoadingAnimateStopTimer(Sender: TObject);
        procedure TimerLoadingAnimateTimer(Sender: TObject);
        procedure TrayIconMainDblClick(Sender: TObject);
        procedure TryLoadTabData(tabName: BrowseType);
        procedure InitTabMenu();
        procedure InitSubMenu();
        procedure LoadAllTabsData();
        procedure ShowPager(pager: PageRecord; tab: BrowseType);
        procedure ChangeTab(tab: BrowseType; tabLabel: TLabel);
        procedure ExitApp();

    protected
        procedure UpdateTranslation(ALang: string); override;

    private
        procedure LoadTab(dataResult: TDataResult; tab: BrowseType);

    end;

    TLoadDataListArgs = class(TObject)
        Tab:     BrowseType;
        SubType: BrowseSubType;
        PageID:  string;
    end;

const
    TryLoadTabInterval = 9.0 / (24 * 60);

var
    MainForm:           TMainForm;
    CurrentTab:         BrowseType;
    CurrentItemId:      string;
    NotReady:           boolean;
    LastSyncTime:       array[BrowseType] of TDateTime;
    ActiveSubMenu:      array[BrowseType] of BrowseSubType;
    StringGrids:        array[BrowseType] of TStringGrid;
    BrowseTrack:        array[BrowseType] of TStringList;
    TabGroup:           array[1..4] of BrowseType;
    IsTabLoading:       boolean;
    AverageWaitingTime: double; // days
    StartLoadingTime, StopLoadingTime: TDateTime;
    LastLoaderThread:   TBackgroundWorker;

implementation

uses LoginFormUnit;

{$R *.lfm}

{ TMainForm }

{ Load all tabs }
procedure TMainForm.LoadAllTabsData();
begin
    LastSyncTime[btStory] := 0;
    LastSyncTime[btBug]   := 0;
    LastSyncTime[btTask]  := 0;
    LastSyncTime[btTodo]  := 0;
    LoadTabData(CurrentTab, '', True);
end;

{ Try load tab}
procedure TMainForm.TryLoadTabData(tabName: BrowseType);
begin
    if (Now - LastSyncTime[tabName]) > TryLoadTabInterval then
    begin
        LoadTabData(tabName);
    end;
end;

{ Load tab data list with a given browse type }
procedure TMainForm.LoadTabData(tabName: BrowseType; pageID: string = '';
    mute: boolean = False);
var
    dataLoaderArgs: TLoadDataListArgs;
begin
    HideMessage;

    if NotReady then
    begin
        if (not mute) then
            DisplayMessage(rsNotReadyMessage);
        Exit;
    end;

    if IsTabLoading and (not mute) then
    begin
        DisplayMessage(rsIsBusy, 'warning');
        Exit;
    end;

    dataLoaderArgs         := TLoadDataListArgs.Create;
    dataLoaderArgs.Tab     := tabName;
    dataLoaderArgs.SubType := ActiveSubMenu[tabName];
    dataLoaderArgs.PageID  := pageID;

    IsTabLoading := True;
    TimerLoadingAnimate.Enabled := True;

    LastLoaderThread := TBackgroundWorker.Create(@LoadingTabData, @LoadTabDataCompleted, True);
    LastLoaderThread.RunWorkerAsync(dataLoaderArgs);
end;

{ Handle tab load completed }
procedure TMainForm.LoadTabDataCompleted(e: TRunWorkerCompletedEventArgs);
var
    Data:    TDataResult;
    tabName: BrowseType;
begin
    Data    := e.Target as TDataResult;
    tabName := BrowseTypes[e.Tag];

    IsTabLoading    := False;
    StopLoadingTime := Now;

    LoadTab(Data, tabName);

    Data.Free;
end;

{ Handle tab loading procedure }
function TMainForm.LoadingTabData(arg: TObject): TRunWorkerCompletedEventArgs;
var
    Data:           TDataResult;
    dataLoaderArgs: TLoadDataListArgs;
begin
    dataLoaderArgs := arg as TLoadDataListArgs;
    Data           := LoadDataList(dataLoaderArgs.Tab, dataLoaderArgs.SubType, dataLoaderArgs.PageID);

    Result.Tag     := Ord(dataLoaderArgs.tab);
    Result.Result  := Data.Result;
    Result.Message := Data.Message;
    Result.Target  := Data;

    dataLoaderArgs.Free;
end;

{ Handle double click event of data grid row}
procedure TMainForm.StringGridDblClick(Sender: TObject);
var
    stringGridSender: TStringGrid;
    tab: BrowseType;
begin
    stringGridSender := Sender as TStringGrid;
    tab := BrowseTypes[stringGridSender.Tag];
    ViewObject(tab, stringGridSender.Cells[0, stringGridSender.Selection.Bottom]);
end;

{ Handle mouse down event of data grid  }
procedure TMainForm.StringGridMouseDown(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: integer);
var
    nACol, nARow:     integer;
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

{ Handle select cell event of data grid }
procedure TMainForm.StringGridTodoSelectCell(Sender: TObject;
    aCol, aRow: integer; var CanSelect: boolean);
begin
end;

{ Hanlde sync procedure of timer }
procedure TMainForm.TimerAutoSyncTimer(Sender: TObject);
var
    i: integer;
begin
    for i := 1 to High(TabGroup) do
    begin
        if (Now - LastSyncTime[TabGroup[i]]) > ((2 * 60 * 1000) /
            ONEDAYMILLIONSECONDS) then
        begin
            LoadTabData(TabGroup[i], '', True);
            break;
        end;
    end;
end;

{ Handle event when animate timer start }
procedure TMainForm.TimerLoadingAnimateStartTimer(Sender: TObject);
begin
    StartLoadingTime := Now;
    IsTabLoading     := True;
    LabelLoadingProgressbar.Width := 0;
    LabelLoadingProgressbar.Visible := True;
end;

{ Handle event on animate timer stop }
procedure TMainForm.TimerLoadingAnimateStopTimer(Sender: TObject);
begin
    AverageWaitingTime := 0.8 * AverageWaitingTime + 0.2 *
        (StopLoadingTime - StartLoadingTime);
    LabelLoadingProgressbar.Visible := False;
end;

{ run animate with timer }
procedure TMainForm.TimerLoadingAnimateTimer(Sender: TObject);
var
    n:  TDateTime;
    d:  double;
    w:  int64;
    nd: double;
begin
    n := Now;
    d := n - StartLoadingTime;
    w := Trunc(Width * (d / AverageWaitingTime));

    if IsTabLoading then
    begin
        LabelLoadingProgressbar.Width := Min(Width - 100, w);
    end
    else
    begin
        LabelLoadingProgressbar.Width :=
            Min(Width, LabelLoadingProgressbar.Width + Trunc(
            (Width - LabelLoadingProgressbar.Width) / 9));
        nd := (n - StopLoadingTime);

        if nd > (420 / ONEDAYMILLIONSECONDS) then
            LabelLoadingProgressbar.Width := Width;

        if nd > (500 / ONEDAYMILLIONSECONDS) then
        begin
            TimerLoadingAnimate.Enabled := False;
        end;
    end;
end;

{ Handle double click event of tray icon }
procedure TMainForm.TrayIconMainDblClick(Sender: TObject);
begin
    MenuItemOpenClick(Sender);
end;

{ Load tab by an given tab name }
procedure TMainForm.LoadTab(dataResult: TDataResult; tab: BrowseType);
var
    Data, dataItem: TJSONObject;
    dataList:       TJSONArray;
    dataRow:        TJSONData;
    index, i:       integer;
    track:          TStringList;
    stringGrid:     TStringGrid;
    noTrack, isNew: boolean;
    id, title:      string;
begin
    if dataResult.Result then
    begin
        Data       := dataResult.Data;
        dataList   := Data.Arrays[BrowseNames[tab]];
        stringGrid := StringGrids[tab];
        isNew      := False;

        track := BrowseTrack[tab];

        if dataResult.FirstPage then
        begin
            noTrack := (track.indexOf('#') < 0);
            if noTrack then
                track.Add('#');
        end;

        { clean all cells }
        stringGrid.Clean;
        stringGrid.RowCount := 0;
        index := 0;

        { convert data }
        for i := 0 to (dataList.Count - 1) do
        begin
            dataRow  := dataList.Items[i];
            dataItem := TJSONObject(dataRow);
            index    := index + 1;
            id       := dataItem.Get('id', '');
            title    := dataItem.Get('name', '-');
            if title = '-' then
                title           := dataItem.Get('title', '-');
            stringGrid.RowCount := index;
            stringGrid.Cells[0, index - 1] := '#' + id;
            stringGrid.Cells[1, index - 1] := title;
            stringGrid.Cells[2, index - 1] := getStatusName(tab, dataItem.Get('status', ''));

            if dataResult.FirstPage then
            begin
                if noTrack then
                begin
                    track.Add(id);
                end
                else if (track.indexOf(id) < 0) then
                begin
                    track.Add(id);
                    dataItem.Booleans['new'] := True;
                    isNew := True;
                end;
            end;
        end;

        ShowPager(dataResult.Pager, tab);

        if isNew and (WindowState = wsMinimized) then
        begin
            PopWindowData := dataResult;
            PopWindow.Show();
        end;

        LastSyncTime[tab] := Now;
        BrowseTrack[tab]  := track;
    end
    else
    begin
        DisplayMessage(dataResult.Message);
    end;
end;

{ update pager content }
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

    labelPagerPrev.Caption := rsPrevPage;
    labelPagerNext.Caption := rsNextPage;

    if pager.Total > 0 then
    begin
        labelPagerInfo.Caption :=
            Format(rsPagerInfoFormat,
            [Min(pager.Total, (pager.PageID - 1) * pager.PerPage + 1),
            Min(pager.Total, pager.PageID * pager.PerPage), pager.Total]);
        labelPagerPrev.Visible := True;
        labelPagerInfo.Visible := True;
        labelPagerLast.Visible := True;
        labelPagerNext.Visible := True;
        labelPagerPrev.Enabled := pager.PageID > 1;
        labelPagerNext.Enabled := pager.PageID < pager.PageTotal;
        labelPagerLast.Enabled := pager.PageTotal > 1;
        if pager.PageID = pager.PageTotal then
        begin
            labelPagerLast.Caption := rsFirstPage;
            labelPagerLast.Hint    := 'first';
        end
        else
        begin
            labelPagerLast.Caption := rsLastPage;
            labelPagerLast.Hint    := 'last';
        end;
    end
    else
    begin
        labelPagerInfo.Caption := rsNoDataMessage;
        labelPagerInfo.Visible := True;
    end;
end;

{ Handle event before window form close }
procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
    closeOption: integer;
begin
    DInfo('FormClose', '关闭窗口');
    if user.CloseOption <= 0 then
    begin
        CloseConfirmForm.Left := Left + Trunc((Width - CloseConfirmForm.Width) / 2);
        CloseConfirmForm.Top  := Top;
        CloseConfirmForm.ShowModal;
        closeOption := CloseConfirmForm.ModalResult;
    end else begin
        closeOption := user.CloseOption;
    end;

    DInfo('FormClose', 'CloseOption=' + INTTOSTR(closeOption));

    if closeOption = 2 then
    begin
        CloseAction   := caMinimize;
        WindowState   := wsMinimized;
        ShowInTaskBar := stNever;
        DInfo('FormClose', '最小化');
        Exit;
    end;

    DInfo('FormClose', '准备直接退出');

    ExitApp;
end;

procedure TMainForm.ExitApp();
begin
    TrayIconMain.Visible := False;

    // Destroy;
    try
        begin
            Logout;
            BrowseTrack[btTodo].Free;
            BrowseTrack[btBug].Free;
            BrowseTrack[btTask].Free;
            BrowseTrack[btStory].Free;
        end;
    finally
    end;

    DInfo('ExitApp', '关闭登录窗口');

    LoginForm.Close;

    DInfo('ExitApp', '关闭弹出窗口');
    PopWindow.Close;
end;

{ Handle event on form create }
procedure TMainForm.FormCreate(Sender: TObject);
begin
    NotReady           := True;
    AverageWaitingTime := 2000 / ONEDAYMILLIONSECONDS; // 2 seconds
    MainFormWindow     := MainForm;

    Caption := rsAppName + ' ' + GetBuildVersion('%d.%d');
end;

{ Init tab menu }
procedure TMainForm.InitTabMenu();
begin
    TabGroup[1]          := btTodo;
    TabGroup[2]          := btTask;
    TabGroup[3]          := btBug;
    TabGroup[4]          := btStory;
    BrowseTrack[btTodo]  := TStringList.Create;
    BrowseTrack[btBug]   := TStringList.Create;
    BrowseTrack[btTask]  := TStringList.Create;
    BrowseTrack[btStory] := TStringList.Create;

    IsTabLoading := False;

    StringGrids[btTask]  := StringGridTask;
    StringGrids[btTodo]  := StringGridTodo;
    StringGrids[btStory] := StringGridStory;
    StringGrids[btBug]   := StringGridBug;
end;

{ Init sub menu }
procedure TMainForm.InitSubMenu();
begin
    ActiveSubMenu[btTask]  := assignedTo;
    ActiveSubMenu[btStory] := assignedTo;
    ActiveSubMenu[btTodo]  := today;
    ActiveSubMenu[btBug]   := assignedTo;
end;

{ Handle event on form show }
procedure TMainForm.FormShow(Sender: TObject);
begin
    if NotReady then
    begin
        NotReady := False;

        InitTabMenu;
        InitSubMenu;

        { Load all data }
        LoadAllTabsData;

        TimerAutoSync.Enabled := True;
    end;
    TrayIconMain.Visible := True;
end;

procedure TMainForm.FormWindowStateChange(Sender: TObject);
begin
    if (WindowState <> wsMinimized) then
    begin
        ShowInTaskBar := stAlways;
    end
    else
    begin
        ShowInTaskBar := stNever;
    end;
end;

{ Handle mouse leave event of lable: changed style }
procedure TMainForm.LabelBtnMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := 16547890;
end;

{ Handle mouse enter event of lable: changed style }
procedure TMainForm.LabelBtnMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := 13392660;
end;

procedure TMainForm.LabelMenu18Click(Sender: TObject);
begin

    OpenUrl('http://www.zentao.net/book/zentaopmshelp/71.html');
end;

procedure TMainForm.LabelMenu19Click(Sender: TObject);
begin
    OpenUrl('http://www.zentao.net/book/zentaopmshelp/43.html');
end;

{ Handle click event of lable: changed tab and load it }
procedure TMainForm.LabelMenuClick(Sender: TObject);
var
    labelSender: TLabel;
    tab:         BrowseType;
    menuParent:  TPanel;
    subMenu:     BrowseSubType;
    i:           integer;
begin
    labelSender := Sender as TLabel;
    menuParent  := labelSender.Parent as TPanel;
    tab         := BrowseTypes[menuParent.Tag];
    subMenu     := BrowseSubTypes[labelSender.Tag];

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

{ Handle click event of lable: open popup menu }
procedure TMainForm.LabelMenuIconClick(Sender: TObject);
begin
    HideMessage;
    PopupMenuNav.Popup;
end;

{ Handle mouse enter event of lable: changed style }
procedure TMainForm.LabelMenuIconMouseEnter(Sender: TObject);
begin
    LabelMenuIcon.Color        := 16547890;
    ShapeMenuIcon1.Brush.Color := clWhite;
    ShapeMenuIcon2.Brush.Color := clWhite;
    ShapeMenuIcon3.Brush.Color := clWhite;
end;

{ Handle mouse leave event of lable: changed style }
procedure TMainForm.LabelMenuIconMouseLeave(Sender: TObject);
begin
    LabelMenuIcon.Color        := $00CC5B14;
    ShapeMenuIcon1.Brush.Color := clSilver;
    ShapeMenuIcon2.Brush.Color := clSilver;
    ShapeMenuIcon3.Brush.Color := clSilver;
end;

{ Handle mouse enter event of lable: changed style }
procedure TMainForm.LabelMenuMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
    tab:         BrowseType;
    menuParent:  TPanel;
begin
    labelSender := Sender as TLabel;
    menuParent  := labelSender.Parent as TPanel;
    tab         := BrowseTypes[menuParent.Tag];
    if BrowseSubTypes[labelSender.Tag] <> ActiveSubMenu[tab] then
    begin
        labelSender.Font.Color := $00141414;
    end;
end;

{ Handle mouse leave event of lable: changed style }
procedure TMainForm.LabelMenuMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
    tab:         BrowseType;
    menuParent:  TPanel;
begin
    labelSender := Sender as TLabel;
    menuParent  := labelSender.Parent as TPanel;
    tab         := BrowseTypes[menuParent.Tag];
    if BrowseSubTypes[labelSender.Tag] <> ActiveSubMenu[tab] then
    begin
        labelSender.Font.Color := $005D5D5D;
    end;
end;

{ Handle click event of lable: close the message panel }
procedure TMainForm.LabelMessageCloseClick(Sender: TObject);
begin
    HideMessage;
end;

{ Handle click event of lable: reload data on page id changed }
procedure TMainForm.LabelPagerBtnClick(Sender: TObject);
var
    labelSender: TLabel;
    tab:         BrowseType;
begin
    labelSender := Sender as TLabel;
    tab         := BrowseTypes[(labelSender.Parent as TPanel).Tag];
    LoadTabData(tab, labelSender.Hint);
end;

{ Handle mouse enter event of lable: changed styel of label btn }
procedure TMainForm.LabelPagerPrevMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender := Sender as TLabel;
    labelSender.Font.Color := $00CC5B14;

end;

{ Handle mouse leave event of lable: changed styel of label btn }
procedure TMainForm.LabelPagerPrevMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender := Sender as TLabel;
    labelSender.Font.Color := $00FC8032;

end;

{ Change language }
procedure TMainForm.MenuItemLangClick(Sender: TObject);
var
    senderMenuItem: TMenuItem;
begin
    senderMenuItem := Sender as TMenuItem;
    SelectLanguage(senderMenuItem.Hint);
end;

{ Show about window }
procedure TMainForm.OnShowAbout(Sender: TObject);
begin
    AboutForm.ShowModal;
end;

{ Close window}
procedure TMainForm.OnExit(Sender: TObject);
begin
    ExitApp;
end;

{ Handle click event of label btn: logout }
procedure TMainForm.LabelPopupMenuBtnLogoutClick(Sender: TObject);
var
    r: HandleResult;
begin
    r := Logout;

    if r.Result then
    begin
        NotReady := True;
        TrayIconMain.Visible := False;
        MainForm.Hide;
        LoginForm.Show;
        LoginForm.WindowState := wsNormal;
    end
    else
    begin
        DisplayMessage(r.Message, 'danger');
    end;
end;

{ Handle mouse enter event of label: changed label style}
procedure TMainForm.LabelPopupMenuBtnMouseEnter(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := 16547890;
    labelSender.Font.Color := clWhite;
end;

{ Handle mouse leave event of label: changed label style}
procedure TMainForm.LabelPopupMenuBtnMouseLeave(Sender: TObject);
var
    labelSender: TLabel;
begin
    labelSender       := Sender as TLabel;
    labelSender.Color := clNone;
    labelSender.Font.Color := clDefault;

end;

{ Handle click event of label: open website with default browser }
procedure TMainForm.LabelPopupMenuBtnOpenWebsiteClick(Sender: TObject);
begin
    OpenURL(User.Url);
end;

{ Handle click event of lable: reload content of all tabs }
procedure TMainForm.LabelPopupMenuBtnSyncClick(Sender: TObject);
begin
    LoadAllTabsData;
end;

{ Handle mouse enter event of tab label }
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

{ Handle mouse leave event of tab label }
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

{ Changed current tab }
procedure TMainForm.ChangeTab(tab: BrowseType; tabLabel: TLabel);
begin
    { reset tab label color }
    LabelTab1.Color      := 13392660;
    LabelTab1.Font.Color := clWhite;
    LabelTab2.Color      := 13392660;
    LabelTab2.Font.Color := clWhite;
    LabelTab3.Color      := 13392660;
    LabelTab3.Font.Color := clWhite;
    LabelTab4.Color      := 13392660;
    LabelTab4.Font.Color := clWhite;

    { changed current tab color }
    tabLabel.Color      := 16380651;
    tabLabel.Font.Color := 13392660;
    CurrentTab          := tab;

    { hide other panel }
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
end;

{ Handle the click event of tab label }
procedure TMainForm.LabelTabClick(Sender: TObject);
var
    labelSender: TLabel;
    tab:         BrowseType;
begin
    labelSender := Sender as TLabel;
    tab         := BrowseTypes[labelSender.Tag];

    if tab <> CurrentTab then
    begin
        ChangeTab(tab, labelSender);

        TryLoadTabData(tab);
    end
    else
    begin
        LoadTabData(tab);
    end;
end;

{ Handle click event of menu item: open main window }
procedure TMainForm.MenuItemOpenClick(Sender: TObject);
begin
    Show;
    WindowState   := wsNormal;
    ShowInTaskBar := stAlways;
end;

{ Handle click event of menu item: reload current tab data }
procedure TMainForm.MenuItemReloadTabClick(Sender: TObject);
begin
    LoadTabData(CurrentTab);
end;

{ Handle click event of menu item: view object with default browser }
procedure TMainForm.MenuItemViewObjectClick(Sender: TObject);
begin
    ViewObject(CurrentTab, CurrentItemId);
end;

{ Hide result message }
procedure TMainForm.HideMessage();
begin
    PanelMessage.Visible := False;
end;

{ Show message }
procedure TMainForm.DisplayMessage(Message: string; msgType: string = 'danger');
begin
    MemoMessager.Caption := Message;
    PanelMessage.Visible := True;
    PanelMessage.Top     := 40;
    PanelMessage.Height  := 70;
    case Lowercase(msgType) of
        'success':
        begin
            PanelMessage.Color      := $00E6FFE5;
            MemoMessager.Color      := $00E6FFE5;
            PanelMessage.Font.Color := $00249F23;
        end;
        'danger':
        begin
            PanelMessage.Color      := $00e5E6ff;
            MemoMessager.Color      := $00e5E6ff;
            PanelMessage.Font.Color := $002D32D2;
            PanelMessage.Height     := 100;
        end;
        'warning':
        begin
            PanelMessage.Color      := $00E5F4FF;
            MemoMessager.Color      := $00E5F4FF;
            PanelMessage.Font.Color := $000086E4;
        end;
        'info':
        begin
            PanelMessage.Color      := $00FFF9E5;
            MemoMessager.Color      := $00FFF9E5;
            PanelMessage.Font.Color := $00D7B339;
        end;
        'important':
        begin
            MemoMessager.Color      := $00E5F3FF;
            PanelMessage.Color      := $00E5F3FF;
            PanelMessage.Font.Color := $001C5181;
        end;
        'special':
        begin
            PanelMessage.Color      := $00FFE5F7;
            MemoMessager.Color      := $00FFE5F7;
            PanelMessage.Font.Color := $00A15789;
        end;
        else
        begin
            PanelMessage.Color      := $00F1F1F1;
            MemoMessager.Color      := $00F1F1F1;
            PanelMessage.Font.Color := $00333333;
        end;
    end;

    Memo1.Top        := 0;
    Memo1.Visible    := True;
    Memo1.Lines.Text := Memo1.Lines.Text + Message + LineEnding;
end;

{ Update translation }
procedure TMainForm.UpdateTranslation(ALang: string);
begin
    inherited;

    // LabelTab2.Caption := rsTask;
    // LabelTab3.Caption := rsBug;
    // LabelTab4.Caption := rsStory;
    TrayIconMain.Hint := rsAppName;

    case ALang of
        'zh_cn': MenuItem5.Checked := True;
        'zh_tw': MenuItem6.Checked := True;
        'en': MenuItem7.Checked    := True;
    end;

    Caption := rsAppName + ' ' + GetBuildVersion('%d.%d');

    user.Lang := ALang;

    LoadAllTabsData;
end;

end.
