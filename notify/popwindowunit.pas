unit PopWindowUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Grids, Buttons, StdCtrls,
    fpjson, jsonparser,
    LocalizedForms,
    StringsUnit,
    ZentaoAPIUnit;

type

    { TPopWindow }

    TPopWindow = class(TLocalizedForm)
        Label1:       TLabel;
        Panel1:       TPanel;
        Panel2:       TPanel;
        SpeedButton1: TSpeedButton;
        StringGridDataList: TStringGrid;
        Timer1:       TTimer;
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormHide(Sender: TObject);
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
    PopTop:    integer;
    StartTime: TDateTime;

implementation

uses LoginFormUnit;

{$R *.lfm}

{ TPopWindow }

{ Show data list }
procedure TPopWindow.ShowDataList();
var
    Data, dataItem: TJSONObject;
    dataList:       TJSONArray;
    dataRow:        TJSONData;
    index, i:       integer;
    title:          string;
begin
    if PopWindowData = nil then
        Exit;

    if PopWindowData.Result then
    begin
        Data     := PopWindowData.Data;
        dataList := Data.Arrays[BrowseNames[PopWindowData.Tab]];

        (* clean all cells *)
        StringGridDataList.Clean;
        StringGridDataList.RowCount := 0;
        index := 0;

        (* convert data *)
        for i := 0 to (dataList.Count - 1) do
        begin
            dataRow  := dataList.Items[i];
            dataItem := TJSONObject(dataRow);
            if not dataItem.Get('new', False) then
                continue;

            index := index + 1;

            if index > 6 then
                break;
            title := dataItem.Get('name', '-');
            if title = '-' then
                title := dataItem.Get('title', '-');

            StringGridDataList.RowCount := index;
            StringGridDataList.Cells[0, index - 1] := '#' + dataItem.Get('id', '');
            StringGridDataList.Cells[1, index - 1] := title;
        end;

        Caption        := Format(rsPopWindowTitle, [BrowseNames[PopWindowData.Tab], index]);
        Label1.Caption := Caption;
    end;
end;

{ Handle event of data grid: view object with default browser }
procedure TPopWindow.StringGridDataListClick(Sender: TObject);
var
    id: string;
begin
    if StringGridDataList.RowCount > 0 then begin
       id := StringGridDataList.Cells[0,
        StringGridDataList.Selection.Bottom];
       if id <> '' then begin
          DInfo('>>> row count', INTTOSTR(StringGridDataList.RowCount));
          DInfo('>>> row SELECT', id);
          ViewObject(PopWindowData.Tab, id);
       end;
    end;
end;

{ Handle event when animate timer start }
procedure TPopWindow.Timer1StartTimer(Sender: TObject);
begin
    StartTime       := Now;
    AlphaBlendValue := 255;
end;

{ Handle event when animate timer stop }
procedure TPopWindow.Timer1StopTimer(Sender: TObject);
begin
    HideWindow;
end;

{ Handle animate with timer }
procedure TPopWindow.Timer1Timer(Sender: TObject);
begin
    if PopTop > (Screen.PrimaryMonitor.WorkareaRect.bottom - Height) then
    begin
        PopTop    := Max(Screen.PrimaryMonitor.WorkareaRect.bottom -
            Height, PopTop - Trunc(Height / 10));
        Top       := PopTop;
        StartTime := Now;
    end
    else
    begin
        if (Now - StartTime) > (6 / ONEDAYSECONDS) then
        begin
            AlphaBlendValue := Min(0, AlphaBlendValue - 25);
        end;

        if AlphaBlendValue = 0 then
        begin
            Timer1.Enabled := False;
        end;
    end;
end;

{ Handle event of speed button: close popup window }
procedure TPopWindow.SpeedButton1Click(Sender: TObject);
begin
    HideWindow;
end;

{ Handle event on form show }
procedure TPopWindow.FormShow(Sender: TObject);
begin
    ShowDataList;
    PopTop         := Screen.PrimaryMonitor.Height;
    Left           := Screen.PrimaryMonitor.WorkareaRect.right - Width;
    Timer1.Enabled := True;
    ShowInTaskBar  := stNever;
    LoginForm.ShowInTaskBar  := stNever;
end;

{ Handle click event of label: open main window}
procedure TPopWindow.Label1lClick(Sender: TObject);
begin
    MainFormWindow.WindowState := wsNormal;
    MainFormWindow.Show;

    HideWindow;
end;

{ Handle mouse enter event of label: change style }
procedure TPopWindow.Label1MouseEnter(Sender: TObject);
begin
    Label1.Font.Color := $00FC8032;
end;

{ Handle mouse leave event of label: change style }
procedure TPopWindow.Label1MouseLeave(Sender: TObject);
begin
    Label1.Font.Color := $00CC5B14;
end;

{ Handle event on form create }
procedure TPopWindow.FormCreate(Sender: TObject);
begin
    Top := Screen.PrimaryMonitor.Height;
end;

procedure TPopWindow.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    LoginForm.ShowInTaskBar  := stNever;
end;

procedure TPopWindow.FormHide(Sender: TObject);
begin
    ShowInTaskBar  := stNever;
    LoginForm.ShowInTaskBar  := stNever;
end;

{ Hide popup window }
procedure TPopWindow.HideWindow();
begin
    Top := Screen.PrimaryMonitor.Height;
    Close();
    DInfo('PopWindow hided and close.');
end;

end.
