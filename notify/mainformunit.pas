unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, synhighlighterunixshellscript, RTTICtrls,
    Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList,
    Grids, EditBtn, Menus,
    fpjson, jsonparser,
    ZentaoAPIUnit;

type

    { TMainForm }

    TMainForm = class(TForm)
        Image1: TImage;
        Image2: TImage;
        Image3: TImage;
        ImageListPopupMenu: TImageList;
        Label1: TLabel;
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
        procedure MenuItem1Click(Sender: TObject);
        procedure MenuItemReloadTabClick(Sender: TObject);
        procedure MenuItemViewObjectClick(Sender: TObject);
        procedure PanelMenuClick(Sender: TObject);
        procedure PanelMessageClick(Sender: TObject);
        procedure PanelPopupMenuClick(Sender: TObject);
        procedure ShowMessage(Message: string; msgType: string = 'danger');
        procedure HideMessage();
        procedure LoadTodos(pageID: string = '');
        procedure LoadTasks(pageID: string = '');
        procedure LoadBugs(pageID: string = '');
        procedure LoadStories(pageID: string = '');
        procedure LoadTabData(tabName: BrowseType; pageID: string = '');
        procedure StringGridDblClick(Sender: TObject);
        procedure StringGridMouseDown(Sender: TObject;
            Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
        procedure StringGridTodoSelectCell(Sender: TObject; aCol,
            aRow: Integer; var CanSelect: Boolean);
        procedure TryLoadTabData(tabName: BrowseType);
        procedure InitTabMenu();
        procedure InitSubMenu();
        procedure HidePopup();overload;
        procedure HidePopup(Sender: TObject);overload;
        procedure LoadAllTabDatas();
        procedure ShowPager(pager: PageRecord; labelPagerInfo: TLabel; labelPagerPrev: TLabel; labelPagerNext: TLabel; labelPagerLast: TLabel);

    private
        { private declarations }
    public
        { public declarations }
    end;

const
    TryLoadTabInterval = 5.0 / (24 * 60);


var
    MainForm:      TMainForm;
    CurrentTab:    BrowseType;
    CurrentItemId: string;
    FirstShow:     boolean;
    LastSyncTime:  array[BrowseType] of TDateTime;
    ActiveSubMenu: array[BrowseType] of BrowseSubType;

implementation

uses LoginFormUnit;

{$R *.lfm}

{ TMainForm }
procedure TMainForm.LoadAllTabDatas();
begin
    LoadTodos;
    LoadBugs;
    if (user.Role = 'qa') or (user.Role = 'qd') then
    begin
        LoadTasks;
    end
    else if (user.Role = 'po') or (user.Role = 'pd') then
    begin
        LoadStories
    end;
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
begin
    case tabName of
        btStory: LoadStories(pageID);
        btBug: LoadBugs(pageID);
        btTask: LoadTasks(pageID);
        btTodo: LoadTodos(pageID);
        else
            ShowMessage('无法加载标签 "' + BrowseName[tabName] + '" 的数据。');
    end;
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
    // ShowMessage(IntToStr(aCol) + ',' + IntToStr(aRow));
end;

(* Load todos *)
procedure TMainForm.LoadTodos(pageID: string = '');
var
    Data, dataItem: TJSONObject;
    dataList: TJSONArray;
    r:        DataResult;
    dataRow:  TJSONEnum;
    index:    integer;
begin
    r := LoadDataList(btTodo, ActiveSubMenu[btTodo], pageID);
    if r.Result then
    begin
        Data     := r.Data;

        dataList := Data.Arrays['todos'];

        (* clean all cells *)
        StringGridTodo.Clean;
        StringGridTodo.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index    := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            StringGridTodo.RowCount := index;
            StringGridTodo.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridTodo.Cells[1, index - 1] := dataItem.Get('name', '-');
            StringGridTodo.Cells[2, index - 1] := dataItem.Get('status', '-');
        end;

        ShowPager(r.Pager, LabelPagerTodoInfo, LabelPagerTodoPrev, LabelPagerTodoNext, LabelPagerTodoLast);

        LastSyncTime[btTodo] := Now;
    end
    else
    begin
        ShowMessage(r.Message);
    end;
end;

(* Load tasks *)
procedure TMainForm.LoadTasks(pageID: string = '');
var
    Data, dataItem: TJSONObject;
    dataList: TJSONArray;
    r:        DataResult;
    dataRow:  TJSONEnum;
    index:    integer;
begin
    r := LoadDataList(btTask, ActiveSubMenu[btTask], pageID);
    if r.Result then
    begin
        Data     := r.Data;
        dataList := Data.Arrays['tasks'];

        (* clean all cells *)
        StringGridTask.Clean;
        StringGridTask.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index    := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            StringGridTask.RowCount := index;
            StringGridTask.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridTask.Cells[1, index - 1] := dataItem.Get('name', '-');
            StringGridTask.Cells[2, index - 1] := dataItem.Get('status', '-');
        end;

        ShowPager(r.Pager, LabelPagerTaskInfo, LabelPagerTaskPrev, LabelPagerTaskNext, LabelPagerTaskLast);

        LastSyncTime[btTask] := Now;
    end
    else
    begin
        ShowMessage(r.Message);
    end;
end;

(* Load bugs *)
procedure TMainForm.LoadBugs(pageID: string = '');
var
    Data, dataItem: TJSONObject;
    dataList: TJSONArray;
    r:        DataResult;
    dataRow:  TJSONEnum;
    index:    integer;
begin
    r := LoadDataList(btBug, ActiveSubMenu[btBug], pageID);
    if r.Result then
    begin
        Data     := r.Data;
        dataList := Data.Arrays['bugs'];

        (* clean all cells *)
        StringGridBug.Clean;
        StringGridBug.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index    := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            StringGridBug.RowCount := index;
            StringGridBug.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridBug.Cells[1, index - 1] := dataItem.Get('title', '-');
            StringGridBug.Cells[2, index - 1] := dataItem.Get('status', '-');
        end;

        ShowPager(r.Pager, LabelPagerBugInfo, LabelPagerBugPrev, LabelPagerBugNext, LabelPagerBugLast);

        LastSyncTime[btBug] := Now;
    end
    else
    begin
        ShowMessage(r.Message);
    end;
end;

(* Load stories *)
procedure TMainForm.LoadStories(pageID: string = '');
var
    Data, dataItem: TJSONObject;
    dataList: TJSONArray;
    r:        DataResult;
    dataRow:  TJSONEnum;
    index:    integer;
begin
    r := LoadDataList(btStory, ActiveSubMenu[btStory], pageID);
    if r.Result then
    begin
        Data     := r.Data;
        dataList := Data.Arrays['stories'];

        (* clean all cells *)
        StringGridStory.Clean;
        StringGridStory.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index    := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            StringGridStory.RowCount := index;
            StringGridStory.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridStory.Cells[1, index - 1] := dataItem.Get('title', '-');
            StringGridStory.Cells[2, index - 1] := dataItem.Get('status', '-');
        end;

        ShowPager(r.Pager, LabelPagerStoryInfo, LabelPagerStoryPrev, LabelPagerStoryNext, LabelPagerStoryLast);

        LastSyncTime[btStory] := Now;
    end
    else
    begin
        ShowMessage(r.Message);
    end;
end;

procedure TMainForm.ShowPager(pager: PageRecord; labelPagerInfo: TLabel; labelPagerPrev: TLabel; labelPagerNext: TLabel; labelPagerLast: TLabel);
begin
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
    LoginForm.Close;
end;

procedure TMainForm.FormClick(Sender: TObject);
begin
    HidePopup;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    FirstShow := True;
end;

procedure TMainForm.InitTabMenu();
begin
    if (user.Role = 'qa') or (user.Role = 'qd') then
    begin
        LabelTab2.Caption := 'Bug';
        LabelTab2.Hint    := 'bug';
        LabelTab2.Tag     := 2;
        LabelTab3.Caption := '任务';
        LabelTab3.Hint    := 'task';
        LabelTab3.Tag     := 1;
    end
    else if (user.Role = 'po') or (user.Role = 'pd') then
    begin
        LabelTab2.Caption := '需求';
        LabelTab2.Hint    := 'story';
        LabelTab2.Tag     := 3;
        LabelTab3.Caption := 'Bug';
        LabelTab3.Hint    := 'bug';
        LabelTab3.Tag     := 2;
    end;
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
        LoadAllTabDatas;

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
    childLabel  : TComponent;
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
    LoadAllTabDatas;
    ShowMessage('已成功同步.', 'success');

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

procedure TMainForm.MenuItem1Click(Sender: TObject);
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

    Memo1.Visible := True;
    Memo1.Lines.Text    := Memo1.Lines.Text + Message + LineEnding;
end;

end.
