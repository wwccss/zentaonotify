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
  rsErrorDataReturned = '服务器返回的数据不正确。 '
      +'请确保拥有权限读取数据。';
  rsErrorCannotConnect = '无法连接到服务器。请检查网络。';
  rsErrorCannotConnectZentao = '无法获取禅道配置信息。请检查禅'
      +'道地址并确认网络连接畅通。';
  rsErrorNeedNewerVersion = '您当前版本是%s，请升级至%s以上版本';
  rsErrorCannotGetSession = '无法获取Session。请确保当前账户拥有'
      +'超级model权限。参考：http://www.zentao.net/book/zentaopmshelp/'
      +'71.html';
  rsErrorLoginFailed = '登录失败。请检查用户名和密码。';
  rsErrorCannotGetRoleConfig = '获取角色信息失败。请确保当前账'
      +'户拥有超级model权限。参考：http://www.zentao.net/book/'
      +'zentaopmshelp/71.html';

implementation

end.


