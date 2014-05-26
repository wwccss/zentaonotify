unit PopWindowUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Grids, Buttons, StdCtrls,
    fpjson, jsonparser,
    ZentaoAPIUnit;

type

    { TPopWindow }

    TPopWindow = class(TForm)
        Label1: TLabel;
        Panel1: TPanel;
        Panel2: TPanel;
        SpeedButton1: TSpeedButton;
        StringGridDataList: TStringGrid;
        Timer1: TTimer;
        procedure FormCreate(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure Label1lClick(Sender: TObject);
        procedure Label1MouseEnter(Sender: TObject);
        procedure Label1MouseLeave(Sender: TObject);
        procedure SpeedButton1Click(Sender: TObject);
        procedure ShowDataList();
        procedure HideWindow();
        procedure StringGridDataListClick(Sender: TObject);
        procedure Timer1StartTimer(Sender: TObject);
        procedure Timer1StopTimer(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    PopWindow: TPopWindow;
    PopTop: Integer;
    StartTime: TDateTime;

implementation

{$R *.lfm}

{ TPopWindow }

procedure TPopWindow.ShowDataList();
var
    Data, dataItem: TJSONObject;
    dataList: TJSONArray;
    dataRow:  TJSONEnum;
    index:    integer;
    title:    string;
begin
    if PopWindowData = nil then Exit;

    if PopWindowData.Result then
    begin
        Data     := PopWindowData.Data;
        dataList := Data.Arrays[BrowseNames[PopWindowData.Tab]];

        (* clean all cells *)
        StringGridDataList.Clean;
        StringGridDataList.RowCount := 0;
        index := 0;

        (* convert data *)
        for dataRow in dataList do
        begin
            dataItem := TJSONObject(dataRow.Value);
            if not dataItem.Get('new', False) then
                continue;

            index    := index + 1;

            if index > 6 then break;
            title := dataItem.Get('name', '-');
            if title = '-' then title := dataItem.Get('title', '-');

            StringGridDataList.RowCount := index;
            StringGridDataList.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridDataList.Cells[1, index - 1] := title;
        end;

        Caption := '新的条目: ' + BrowseNames[PopWindowData.Tab] + ' [' + IntToStr(index) + ']';
        Label1.Caption := Caption;
    end
end;

procedure TPopWindow.StringGridDataListClick(Sender: TObject);
begin
    ViewObject(PopWindowData.Tab, StringGridDataList.Cells[0, StringGridDataList.Selection.Bottom]);
end;

procedure TPopWindow.Timer1StartTimer(Sender: TObject);
begin
    StartTime := Now;
    AlphaBlendValue := 255;
end;

procedure TPopWindow.Timer1StopTimer(Sender: TObject);
begin
    HideWindow;
end;

procedure TPopWindow.Timer1Timer(Sender: TObject);
begin
    if PopTop > (Screen.PrimaryMonitor.WorkareaRect.bottom - Height) then
    begin
        PopTop := Max(Screen.PrimaryMonitor.WorkareaRect.bottom - Height, PopTop - Trunc(Height/10));
        Top := PopTop;
        StartTime := Now;
    end
    else
    begin
        if (Now - StartTime) > (6 / ONEDAYSECONDS) then
        begin
            AlphaBlendValue := Min(0,AlphaBlendValue - 25);
        end;

        if AlphaBlendValue = 0 then
        begin
            Timer1.Enabled := False;
        end;
    end;
end;

procedure TPopWindow.SpeedButton1Click(Sender: TObject);
begin
    HideWindow;
end;

procedure TPopWindow.FormShow(Sender: TObject);
begin
    ShowDataList;
    PopTop := Screen.PrimaryMonitor.Height;
    Left := Screen.PrimaryMonitor.WorkareaRect.right - Width;
    Timer1.Enabled := True;
end;

procedure TPopWindow.Label1lClick(Sender: TObject);
begin
    MainFormWindow.WindowState := wsNormal;
    MainFormWindow.Show;

    HideWindow;
end;

procedure TPopWindow.Label1MouseEnter(Sender: TObject);
begin
    Label1.Font.Color := $00FC8032;
end;

procedure TPopWindow.Label1MouseLeave(Sender: TObject);
begin
    Label1.Font.Color := $00CC5B14;
end;

procedure TPopWindow.FormCreate(Sender: TObject);
begin
    Top := Screen.PrimaryMonitor.Height;
end;

procedure TPopWindow.HideWindow();
begin
    Hide;
    Top := Screen.PrimaryMonitor.Height;
end;

end.

