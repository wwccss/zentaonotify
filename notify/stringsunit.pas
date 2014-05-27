{ This unit collects all resource strings of the project }
unit StringsUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

resourcestring
  rsAppName = '禅道桌面提醒工具';
  
  rsRequireZentaoAddress = '请填写禅道地址。';
  rsRequireUsername = '请填写用户名。';
  rsRequirePassword = '请填写禅道密码。';
  rsLoging = '登录中...';
  rsLogin = '登录';
  rsNotReadyMessage = '您还没有登录，无法获取数据。';
  rsIsBusy = '应用正忙，请稍后再试。';
  rsPagerInfoFormat = '第 %d - %d 条，共 %d 条';
  rsFirstPage = '首页';
  rsPrevPage = '上一页';
  rsNextPage = '下一页';
  rsLastPage = '末页';
  rsNoDataMessage = '暂时没有数据。';
  rsBug = 'Bug';
  rsTask = '任务';
  rsStory = '需求';
  rsPopWindowTitle = '新的条目: %s [%d]';

implementation

end.


