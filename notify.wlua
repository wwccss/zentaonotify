require "iuplua"
require "iupluacontrols" 
require "json" 
require "md5" 
require "imlua" 
require "iupluaim" 
local http  = require "socket.http"

local init_encode = package.loadlib("encode.dll","luaopen_encode")
init_encode()


zentaoRoot    = "http://local.pms.net/" 
account       = "azhi"
password      = "chencongzhi@azhi"
sessionAPI    = zentaoRoot .. "api-getsessionid.json?a=1"
loginAPI      = zentaoRoot .. "user-login.json?a=1"
--superMyBugAPI = zentaoRoot .. "api-getmodel-bug-getUserBugPairs-account=$account.json?a=1"

session = {}
function getSession()
    http.request{url = sessionAPI, method = "GET", sink = ltn12.sink.table(session)}
    if table.getn(session) == 0 then return false end
    session      = json.decode(table.concat(session))
    session.data = json.decode(session.data)

    print("Get session success")
    return true
end

function login()
    local loginResponse = {}
    password = md5.sumhexa(md5.sumhexa(password) .. session.data.rand)
    loginAPI = loginAPI .. "&account=" .. account .. "&password=" .. password .. "&" .. session.data.sessionName .. "=" .. session.data.sessionID
    http.request{url = loginAPI, method = "GET", sink = ltn12.sink.table(loginResponse)}
    loginResponse = json.decode(table.concat(loginResponse))
    if loginResponse.status == 'success' then return true end
    return false
end

function getData(object, type)
    local data = {}
    local dataAPI
    if type then 
        dataAPI = zentaoRoot .. "my-" .. object .. "-" .. type .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID
    else
        dataAPI = zentaoRoot .. "my-" .. object .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID
    end

    http.request{url = dataAPI, method = "GET", sink = ltn12.sink.table(data)}
    if table.getn(data) == 0 then return {} end
    data = json.decode(table.concat(data))
    data = json.decode(data.data)
    data = data[object .. 's']
    return data
end

if not getSession() then print('Failed to get session') return end
if not login() then print('Failed to login') return end

dofile('zh-cn.lua')
function initTabs(source)
  local tabs = {}
  for k,v in pairs(source) do
    tabs[k] = iup.list{expand = 'YES', tip=k, dropdown='NO'}
    tabs[k].tabtitle = source[k]
  end
  return tabs
end

function createList(object, tab)
    local type = tab.tip
    local data = getData(object, type)

    for i, val in pairs(data) do 
        title   = object == 'bug' and val.title or val.name 
        tab[i]  = "#" .. val.id .. ' ' .. encode.u82a(title)
        tab.dblclick_cb = function(object, number, title) 
            local objectID = string.sub(title, string.find(title, '%d+'))
            local viewObjectAPI = zentaoRoot .. 'task-view-' .. objectID .. '.html?'  .. session.data.sessionName .. '=' .. session.data.sessionID 
            os.execute('start ' .. viewObjectAPI)
        end
    end
end

----------------------------------------------------------------
--- Todo tab.
----------------------------------------------------------------
todoTabs = initTabs(lang.todo)

todoBox = iup.tabs
{
    todoTabs.today,
    todoTabs.yesterday,
    todoTabs.thisweek,
    todoTabs.lastweek,
    todoTabs.thismonth,
    todoTabs.lastmonth,
    todoTabs.thisseason,
    todoTabs.thisyear,
    todoTabs.furture,
    todoTabs.all,
}
todoBox.tabtitle = lang.todoTab
function todoBox:tabchange_cb(newTab, oldTab)
    createList('todo', newTab)
end

----------------------------------------------------------------
--- Task tab.
----------------------------------------------------------------
taskTabs = initTabs(lang.task)
taskBox = iup.tabs
{
    taskTabs.assignedto,
    taskTabs.openedby,
    taskTabs.finishedby,
    taskTabs.closedby,
    taskTabs.canceledby,
}
taskBox.tabtitle = lang.taskTab
function taskBox:tabchange_cb(newTab, oldTab)
    createList('task', newTab)
end

----------------------------------------------------------------
--- Bug tab.
----------------------------------------------------------------
bugTabs = initTabs(lang.bug)
bugBox = iup.tabs
{
    bugTabs.assignedtome,
    bugTabs.openedbyme,
    bugTabs.resolvedbyme,
    bugTabs.closedbyme
}
bugBox.tabtitle = lang.bugTab

function bugBox:tabchange_cb(newTab, oldTab)
    createList('bug', newTab)
end

----------------------------------------------------------------
--- Notify dialog.
----------------------------------------------------------------
dialogIcon  = iup.LoadImage("favicon.ico")
mainDialog = iup.dialog
{
    iup.tabs{todoBox, taskBox, bugBox},
    title = notifyTitle.mainPanel,
    size  = "200x400",
    icon = dialogIcon,
    tray = 'YES',
    traytip =  notifyTitle.mainPanel,
    trayimage = dialogIcon
}

function mainDialog:close_cb()
    mainDialog.tray = "NO" 
end

function mainDialog:show_cb(state)
    if(state == iup.MINIMIZE) then mainDialog.hidetaskbar = "YES" end
end

function mainDialog:trayclick_cb(b, press)
    if b == 1 and press == 1 then mainDialog:show() end
    
    if b == 3 and press == 1 then 
        local rightMenu = iup.menu{iup.item {title = lang.item.exit, action = function() mainDialog.tray = "NO" mainDialog:hide() end}}
        rightMenu:popup(iup.MOUSEPOS, iup.MOUSEPOS)
    end
    return iup.DEFAULT
end

mainDialog:showxy(iup.RIGHT, iup.CENTER)

if iup.MainLoopLevel() == 0 then
  iup.MainLoop()
end

