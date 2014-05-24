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
        procedure SpeedButton1Click(Sender: TObject);
        procedure ShowDataList();
        procedure StringGridDataListClick(Sender: TObject);
        procedure Timer1Timer(Sender: TObject);
    private
        { private declarations }
    public
        { public declarations }
    end;

var
    PopWindow: TPopWindow;
    PopTop: Integer;

implementation

{$R *.lfm}

{ TPopWindow }

procedure TPopWindow.ShowDataList();
var
    Data, dataItem: TJSONObject;
    dataList: TJSONArray;
    dataRow:  TJSONEnum;
    index:    integer;
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

            if index > 6 then continue;
            StringGridDataList.RowCount := index;
            StringGridDataList.Cells[0, index - 1] := '+' + dataItem.Get('id', '');
            StringGridDataList.Cells[1, index - 1] := dataItem.Get('name', '-');
        end;

        Caption := '新的条目: ' + BrowseNames[PopWindowData.Tab] + ' [' + IntToStr(index) + ']';
        Label1.Caption := Caption;
    end
end;

procedure TPopWindow.StringGridDataListClick(Sender: TObject);
begin
    ViewObject(PopWindowData.Tab, StringGridDataList.Cells[0, StringGridDataList.Selection.Bottom]);
end;

procedure TPopWindow.Timer1Timer(Sender: TObject);
begin
    if PopTop > (Screen.Height - Height) then
    begin
        PopTop := PopTop - 10;
        Top := PopTop;
    end
    else
    begin
      Timer1.Enabled := False;
    end;
end;

procedure TPopWindow.SpeedButton1Click(Sender: TObject);
begin
    Hide;
    Top := Screen.Height;
end;

procedure TPopWindow.FormShow(Sender: TObject);
begin
    ShowDataList;
    PopTop := Screen.Height;
    Left := Screen.Width - Width;
    Timer1.Enabled := True;
end;

procedure TPopWindow.FormCreate(Sender: TObject);
begin
    Top := Screen.Height;
end;

end.

