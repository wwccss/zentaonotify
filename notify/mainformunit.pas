unit MainFormUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, synhighlighterunixshellscript, RTTICtrls,
    Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls, Buttons, ActnList,
    Grids, EditBtn, Menus,
    fpjson,jsonparser,
    ZentaoAPIUnit;

type

    { TMainForm }

    TMainForm = class(TForm)
        Label1: TLabel;
        Label2: TLabel;
        Label3: TLabel;
        Label4: TLabel;
        Label5: TLabel;
        Label6: TLabel;
        Label7: TLabel;
        Label8: TLabel;
        LabelResult: TLabel;
        LabelTab3: TLabel;
        LabelTab1: TLabel;
        LabelTab2: TLabel;
        Memo1: TMemo;
        PanelMenu: TPanel;
        PanelNav: TPanel;
        PanelNav1: TPanel;
        PanelNav2: TPanel;
        PanelNav3: TPanel;
        StringGridBug: TStringGrid;
        StringGridStory: TStringGrid;
        StringGridTodo: TStringGrid;
        StringGridTask: TStringGrid;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure LabelTabMouseEnter(Sender: TObject);
        procedure LabelTabMouseLeave(Sender: TObject);
        procedure LabelTabClick(Sender: TObject);
        procedure PanelMenuClick(Sender: TObject);
        procedure ShowResultMessage(Message: string);
        procedure HideResultMessage();
        procedure LoadTodos();
        procedure LoadTasks();
        procedure LoadBugs();
        procedure LoadStories();
        procedure LoadTabData(tabName: string);

    private
        { private declarations }
    public
        { public declarations }
    end;

var
    MainForm: TMainForm;
    TabName : string;

implementation

uses LoginFormUnit;

{$R *.lfm}

{ TMainForm }

procedure TMainForm.LoadTabData(tabName: string);
begin
    case tabName of
        'story': LoadStories;
        'bug': LoadBugs;
        'task': LoadTasks;
        'todo': LoadTodos;
        else 
            ShowResultMessage('无法加载标签 "' + tabName + '" 的数据。');
    end;
end;

(* Load todos *)
procedure TMainForm.LoadTodos();
var data, pager, dataItem : TJSONObject;
var dataList : TJSONArray;
var r : DataResult;
var dataRow : TJSONEnum;
var index : Integer;
begin
    r := LoadDataList('todo', 'today', '0');
    if r.Result then
    begin
        data := r.Data;
        pager := TJSONObject(TJSONParser.Create(data.Get('pager', '')).Parse);
        dataList := data.Arrays['todos'];
        
        (* clean all cells *)
        StringGridTodo.Clean;
        StringGridTodo.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            StringGridTodo.RowCount := index;
            StringGridTodo.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridTodo.Cells[1, index - 1] := dataItem.Get('name', '-');
            StringGridTodo.Cells[2, index - 1] := dataItem.Get('status', '-');
        end;
    end
    else
    begin
        ShowResultMessage(r.Message);
    end;
end;

(* Load tasks *)
procedure TMainForm.LoadTasks();
var data, pager, dataItem : TJSONObject;
var dataList : TJSONArray;
var r : DataResult;
var dataRow : TJSONEnum;
var index : Integer;
begin
    r := LoadDataList('task', 'assignedTo', '0');
    if r.Result then
    begin
        data := r.Data;
        pager := TJSONObject(TJSONParser.Create(data.Get('pager', '')).Parse);
        dataList := data.Arrays['tasks'];
        
        (* clean all cells *)
        StringGridTask.Clean;
        StringGridTask.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            StringGridTask.RowCount := index;
            StringGridTask.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridTask.Cells[1, index - 1] := dataItem.Get('name', '-');
            StringGridTask.Cells[2, index - 1] := dataItem.Get('status', '-');
        end;
    end
    else
    begin
        ShowResultMessage(r.Message);
    end;
end;

(* Load bugs *)
procedure TMainForm.LoadBugs();
var data, pager, dataItem : TJSONObject;
var dataList : TJSONArray;
var r : DataResult;
var dataRow : TJSONEnum;
var index : Integer;
begin
    r := LoadDataList('bug', 'assignedTo', '0');
    if r.Result then
    begin
        data := r.Data;
        pager := TJSONObject(TJSONParser.Create(data.Get('pager', '')).Parse);
        dataList := data.Arrays['bugs'];
        
        (* clean all cells *)
        StringGridBug.Clean;
        StringGridBug.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            StringGridBug.RowCount := index;
            StringGridBug.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridBug.Cells[1, index - 1] := dataItem.Get('title', '-');
            StringGridBug.Cells[2, index - 1] := dataItem.Get('status', '-');
        end;
    end
    else
    begin
        ShowResultMessage(r.Message);
    end;
end;

(* Load stories *)
procedure TMainForm.LoadStories();
var data, pager, dataItem : TJSONObject;
var dataList : TJSONArray;
var r : DataResult;
var dataRow : TJSONEnum;
var index : Integer;
begin
    r := LoadDataList('story', 'assignedTo', '0');
    if r.Result then
    begin
        data := r.Data;
        pager := TJSONObject(TJSONParser.Create(data.Get('pager', '')).Parse);
        dataList := data.Arrays['stories'];
        
        (* clean all cells *)
        StringGridStory.Clean;
        StringGridStory.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            index := index + 1;
            dataItem := TJSONObject(dataRow.Value);
            StringGridStory.RowCount := index;
            StringGridStory.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridStory.Cells[1, index - 1] := dataItem.Get('title', '-');
            StringGridStory.Cells[2, index - 1] := dataItem.Get('status', '-');
        end;
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
    (* Set menu *)

end;

procedure TMainForm.FormShow(Sender: TObject);
begin
    LoadTodos;
    LoadTasks;
    LoadBugs;
    LoadStories;
    ShowResultMessage(User.Role);
end;

procedure TMainForm.LabelTabMouseEnter(Sender: TObject);
var LabelSender : TLabel;
begin
    LabelSender := Sender as TLabel;
    if LabelSender.Tag = 0 then
    begin
        LabelSender.Color := 16547890;
    end;
end;

procedure TMainForm.LabelTabMouseLeave(Sender: TObject);
var LabelSender : TLabel;
begin
    LabelSender := Sender as TLabel;
    if LabelSender.Tag = 0 then
    begin
        LabelSender.Color := 13392660;
    end;
end;

procedure TMainForm.LabelTabClick(Sender: TObject);
var LabelSender : TLabel;
begin
    LabelSender := Sender as TLabel;
    if LabelSender.Tag = 0 then
    begin
        LabelTab1.Color      := 13392660;
        LabelTab1.Font.Color := clWhite;
        LabelTab1.Tag        := 0;
        LabelTab2.Color      := 13392660;
        LabelTab2.Font.Color := clWhite;
        LabelTab2.Tag        := 0;
        LabelTab3.Color       := 13392660;
        LabelTab3.Font.Color  := clWhite;
        LabelTab3.Tag         := 0;

        LabelSender.Color      := 16380651;
        LabelSender.Font.Color := 13392660;
        LabelSender.Tag        := 1;
        TabName                := LabelSender.Hint;
    end;
end;

procedure TMainForm.PanelMenuClick(Sender: TObject);
begin

end;

(* Hide result message *)
procedure TMainForm.HideResultMessage();
begin
    LabelResult.Visible := false;
end;

procedure TMainForm.ShowResultMessage(Message: string);
begin
    LabelResult.Caption := Message;
    LabelResult.Visible := true;
    Memo1.Lines.Text := Memo1.Lines.Text + Message + LineEnding;
end;

end.

