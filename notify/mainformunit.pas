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
        Label1: TLabel;
        Label3: TLabel;
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
        LabelResult:     TLabel;
        LabelTab3:       TLabel;
        LabelTab1:       TLabel;
        LabelTab2:       TLabel;
        LabelMenuIcon: TLabel;
        Memo1:           TMemo;
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
        ShapeMenuIcon1: TShape;
        ShapeMenuIcon2: TShape;
        ShapeMenuIcon3: TShape;
        StringGridBug:   TStringGrid;
        StringGridStory: TStringGrid;
        StringGridTodo:  TStringGrid;
        StringGridTask:  TStringGrid;
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
        procedure LabelTabMouseEnter(Sender: TObject);
        procedure LabelTabMouseLeave(Sender: TObject);
        procedure LabelTabClick(Sender: TObject);
        procedure MenuItem1Click(Sender: TObject);
        procedure PanelMenuClick(Sender: TObject);
        procedure ShowResultMessage(Message: string);
        procedure HideResultMessage();
        procedure LoadTodos();
        procedure LoadTasks();
        procedure LoadBugs();
        procedure LoadStories();
        procedure LoadTabData(tabName: BrowseType);
        procedure TryLoadTabData(tabName: BrowseType);
        procedure InitTabMenu();
        procedure InitSubMenu();

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
    FirstShow:     boolean;
    LastSyncTime:  array[BrowseType] of TDateTime;
    ActiveSubMenu: array[BrowseType] of string;

implementation

uses LoginFormUnit;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.TryLoadTabData(tabName: BrowseType);
begin
    if (Now - LastSyncTime[tabName]) > TryLoadTabInterval then
    begin
        LoadTabData(tabName);
    end;
end;

procedure TMainForm.LoadTabData(tabName: BrowseType);
begin
    case tabName of
        btStory: LoadStories;
        btBug: LoadBugs;
        btTask: LoadTasks;
        btTodo: LoadTodos;
        else
            ShowResultMessage('无法加载标签 "' + BrowseName[tabName] + '" 的数据。');
    end;
end;

(* Load todos *)
procedure TMainForm.LoadTodos();
var
    Data, pager, dataItem: TJSONObject;
    dataList: TJSONArray;
    r:        DataResult;
    dataRow:  TJSONEnum;
    index:    integer;
begin
    r := LoadDataList('todo', ActiveSubMenu[btTodo], '0');
    if r.Result then
    begin
        Data     := r.Data;
        pager    := TJSONObject(TJSONParser.Create(Data.Get('pager', '')).Parse);
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

        LastSyncTime[btTodo] := Now;
    end
    else
    begin
        ShowResultMessage(r.Message);
    end;
end;

(* Load tasks *)
procedure TMainForm.LoadTasks();
var
    Data, pager, dataItem: TJSONObject;
    dataList: TJSONArray;
    r:        DataResult;
    dataRow:  TJSONEnum;
    index:    integer;
begin
    r := LoadDataList('task', ActiveSubMenu[btTask], '0');
    if r.Result then
    begin
        Data     := r.Data;
        pager    := TJSONObject(TJSONParser.Create(Data.Get('pager', '')).Parse);
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

        LastSyncTime[btTask] := Now;
    end
    else
    begin
        ShowResultMessage(r.Message);
    end;
end;

(* Load bugs *)
procedure TMainForm.LoadBugs();
var
    Data, pager, dataItem: TJSONObject;
    dataList: TJSONArray;
    r:        DataResult;
    dataRow:  TJSONEnum;
    index:    integer;
begin
    r := LoadDataList('bug', ActiveSubMenu[btBug], '0');
    if r.Result then
    begin
        Data     := r.Data;
        pager    := TJSONObject(TJSONParser.Create(Data.Get('pager', '')).Parse);
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

        LastSyncTime[btBug] := Now;
    end
    else
    begin
        ShowResultMessage(r.Message);
    end;
end;

(* Load stories *)
procedure TMainForm.LoadStories();
var
    Data, pager, dataItem: TJSONObject;
    dataList: TJSONArray;
    r:        DataResult;
    dataRow:  TJSONEnum;
    index:    integer;
begin
    r := LoadDataList('story', ActiveSubMenu[btStory], '0');
    if r.Result then
    begin
        Data     := r.Data;
        pager    := TJSONObject(TJSONParser.Create(Data.Get('pager', '')).Parse);
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

        LastSyncTime[btStory] := Now;
    end
    else
    begin
        ShowResultMessage(r.Message);
    end;
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    // Destroy;
    LoginForm.Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
    FirstShow := True;
end;

procedure TMainForm.InitTabMenu();
begin
    if (User.Role = 'qa') or (User.Role = 'qd') then
    begin
        LabelTab2.Caption := 'Bug';
        LabelTab2.Hint    := 'bug';
        LabelTab2.Tag     := 2;
        LabelTab3.Caption := '任务';
        LabelTab3.Hint    := 'task';
        LabelTab3.Tag     := 1;
    end
    else if (User.Role = 'po') or (User.Role = 'pd') then
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
    ActiveSubMenu[btTask] := 'assignedTo';
    ActiveSubMenu[btStory] := 'assignedTo';
    ActiveSubMenu[btTodo] := 'today';
    ActiveSubMenu[btBug] := 'assignedTo';
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
    if FirstShow then
    begin
        InitTabMenu;
        InitSubMenu;

        (* Load all data *)
        LoadTodos;
        LoadTasks;
        LoadBugs;
        LoadStories;

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
    subMenu     : string;
    childLabel  : TComponent;
    i           : integer;
begin
    labelSender := Sender as TLabel;
    menuParent  := labelSender.Parent as TPanel;
    tab         := BrowseTypes[menuParent.Tag];
    subMenu     := labelSender.Hint;

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
    if labelSender.Hint <> ActiveSubMenu[tab] then
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
    if labelSender.Hint <> ActiveSubMenu[tab] then
    begin
        labelSender.Font.Color := $005D5D5D;
    end;
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
    end
    else
    begin
        LoadTabData(tab);
    end;
end;

procedure TMainForm.MenuItem1Click(Sender: TObject);
begin

end;

procedure TMainForm.PanelMenuClick(Sender: TObject);
begin

end;

(* Hide result message *)
procedure TMainForm.HideResultMessage();
begin
    LabelResult.Visible := False;
end;

procedure TMainForm.ShowResultMessage(Message: string);
begin
    LabelResult.Caption := Message;
    LabelResult.Visible := True;
    Memo1.Visible       := True;
    Memo1.Lines.Text    := Memo1.Lines.Text + Message + LineEnding;
end;

end.
