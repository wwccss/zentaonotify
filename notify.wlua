require "iuplua"
require "iupluacontrols" 
require "json" 
require "md5" 
require "imlua" 
require "ex" 
require "iupluaim" 
local http  = require "socket.http"

local initEncode = package.loadlib("encode.dll","luaopen_encode")
initEncode()
dofile('zh-cn.lua')

---------------------------------------------------------------------
--- Show prompt dialog.
---------------------------------------------------------------------
function showPromptDlg(message, wrong, dialogType)
    dialogType = dialogtype or 'WARNING'
    local promptDlg = iup.messagedlg{dialogtype = dialogType, title = lang.prompt.warning, value = message}
    promptDlg:popup()
    promptDlg:destroy()
end

---------------------------------------------------------------------
--- Detect if the notify is running.
---------------------------------------------------------------------
PROCESS_EXIT = 'EXIT'
processIsRunning = io.open(PROCESS_EXIT, 'r')
if processIsRunning then showPromptDlg(lang.prompt.processIsRunning) processIsRunning:close() os.exit() end
processIsRunning = io.open(PROCESS_EXIT, 'w')
processIsRunning:close()

---------------------------------------------------------------------
--- Exit process.
---------------------------------------------------------------------
function exitProcess() 
    os.remove(PROCESS_EXIT) 
    os.exit() 
end

---------------------------------------------------------------------
--- Login. 
---------------------------------------------------------------------
LOGIN_FILE    = 'LOGININFO'  -- The file of login info of user.
ACTIONID_FILE = 'ACTIONID'   -- The file of the newest action id.
UPDATE_TIME   = 300000       -- Check the new action every five minute.
CLOSE_TIME    = 30000        -- Close the nofity window after show.
MOUSE_LEFT    = 49           -- The code represent left mouse.
MOUSE_PRESSED = 1            -- The code when mouse pressed.
KEY_ENTER     = 13           -- The code of key enter.
KEY_ESC       = 141          -- The code of key esc.
VERSION       = 'notify_0.1' -- The version of notify.
OS_VERSION    = '4.0'        -- The least supported version of open source.
PRO_VERSION   = '1.3'        -- The least supported version of pro.

isLogin     = false         -- The flag if login.
config      = {}            -- The config info.
session     = {}            -- The session info.
pagerInfo   = {}            -- The pager info, including recTotal,recPerPage,pageID,totalPage.
currentTab  = {}            -- Current tab.
dialogIcon  = iup.LoadImage("favicon.ico")

------------------------------------------
--- Get the infomation of login.
------------------------------------------
function getLoginInfo()
    local f = io.open(LOGIN_FILE, 'r')
    if not f then return nil end
    local info = f:read('*a')
    info = json.decode(info)
    return info.zentaoRoot, info.account, info.password
end
zentaoRoot, account, password = getLoginInfo()

------------------------------------------
--- Set login info.
------------------------------------------
function setLoginInfo()
    local data = {['zentaoRoot'] = zentaoRoot, ['account'] = account, ['password'] = password}
    local f = io.open(LOGIN_FILE, 'w')
    f:write(json.encode(data))
    f:close()
end

------------------------------------------
--- Check the version of pms.
------------------------------------------
function checkPmsVersion()
    local version = config.version
    local isPro   = false
    if string.find(string.lower(version), 'pro') then isPro = true version = string.sub(version, #'pro' + 1) end
    version = string.sub(version, string.find(version, '%d%.%d'))

    if isPro then
        if version < PRO_VERSION then showPromptDlg(string.format(lang.prompt.versionUnmatch, config.version, PRO_VERSION)) end
    else
        if version < OS_VERSION then showPromptDlg(string.format(lang.prompt.versionUnmatch, config.version, OS_VERSION)) end
    end
end

------------------------------------------
--- Set config info.
------------------------------------------
function getConfig()
    configAPI = zentaoRoot .. '/index.php?mode=getconfig'
    http.request{url = configAPI, method = "GET", sink = ltn12.sink.table(config)}

    if table.getn(config) == 0 then showPromptDlg(lang.prompt.wrongUrl) return false end
    if not pcall(json.decode, table.concat(config)) then showPromptDlg(lang.prompt.wrongUrl) return false end

    config = json.decode(table.concat(config))
    checkPmsVersion()
    return true
end

------------------------------------------
--- Get API.
------------------------------------------
function getAPI(params)
    local url = ''
    
    local moduleName = params[1] 
    local methodName = params[2] 

    local pagerParam = ''
    sessionParam = methodName ~= 'getsessionid' and session.data.sessionName .. '=' .. session.data.sessionID or ''
    viewType     = methodName == 'view' and 'html' or 'json'

    accountParam = ''
    if params.account then 
        accountParam = '&account=' .. params.account .. '&password=' .. params.password 
        params.account  = nil
        params.password = nil
    end
   
    if params.pageID then
       if pagerInfo[methodName] then
           local type = params.type      
           if config.requestType == 'GET' then
               pagerParam = 'recTotal=' .. pagerInfo[methodName][type].recTotal
               pagerParam = pagerParam .. '&recPerPage=' .. pagerInfo[methodName][type].recPerPage
               pagerParam = pagerParam .. '&pageID=' .. params.pageID
           else
               pagerParam = pagerInfo[methodName][type].recTotal .. config.requestFix .. pagerInfo[methodName][type].recPerPage .. config.requestFix .. params.pageID
           end
       end
    end

    for k, v in pairs(params) do
        if config.requestType == 'GET' then
            if k == 1 then k = config.moduleVar end
            if k == 2 then k = config.methodVar end
            if k ~= 'pageID' then url = url .. k .. '=' .. v .. '&' end
        else
            if k~= 'pageID' then url = url .. v .. '-' end
        end
    end

    if config.requestType == 'GET' then
        if pagerParam   ~= '' then pagerParam   = '&' .. pagerParam   end
        if sessionParam ~= '' then sessionParam = '&' .. sessionParam end
        url = url .. config.viewVar .. '=' .. viewType .. pagerParam .. sessionParam
        url = url .. '&client=' .. VERSION
    else
        if pagerParam  ~= ''  then pagerParam   = '-' .. pagerParam   end
        if sessionParam ~= '' then sessionParam = '?' .. sessionParam end
        if string.sub(url, #url) == '-' then url = string.sub(url, 1, #url - 1) end
        url = url .. pagerParam .. '.' .. viewType .. sessionParam
        url = sessionParam ~= '' and url .. '&client=' .. VERSION or url .. '?client=' .. VERSION
        print(url)
    end

    url = url .. accountParam
    url = config.requestType == 'GET' and zentaoRoot .. '/?' .. url or zentaoRoot .. '/' .. url
    return url
end

------------------------------------------
--- Get the session of login.
------------------------------------------
function getSession()
    if not getConfig() then return false end

    session = {}

    sessionAPI = getAPI{'api', 'getsessionid'}
    http.request{url = sessionAPI, method = "GET", sink = ltn12.sink.table(session)}

    if table.getn(session) == 0 then showPromptDlg(lang.prompt.wrongUrl) return false end
    if not pcall(json.decode, table.concat(session)) then showPromptDlg(lang.prompt.wrongUrl) return false end

    session = json.decode(table.concat(session))
    session.data = json.decode(session.data)
    return true
end

------------------------------------------
--- Login ZenTao.
------------------------------------------
loginDialog = nil
function login()
    if not getSession() then isLogin = false return end

    loginAPI = getAPI{'user' , 'login', ['account'] = account, ['password'] = md5.sumhexa(password .. session.data.rand)}

    local loginResponse = {}
    http.request{url = loginAPI, method = "GET", sink = ltn12.sink.table(loginResponse)}

    if not pcall(json.decode, table.concat(loginResponse)) then showPromptDlg(lang.prompt.wrongInfo) return false end

    loginResponse = json.decode(table.concat(loginResponse))
    if loginResponse.status == 'success' then  
       isLogin = true
    else
       showPromptDlg(lang.prompt.wrongInfo) 
    end
end

------------------------------------------
--- Show login Dialog.
------------------------------------------
function isLegalLoginInfo()
    if zentaoRoot == '' then showPromptDlg(string.format(lang.prompt.notEmpty, lang.login.url)) return false end
    if account    == '' then showPromptDlg(string.format(lang.prompt.notEmpty, lang.login.account)) return false end
    if password   == '' then showPromptDlg(string.format(lang.prompt.notEmpty, lang.login.password)) return false end
    return true
end

function clickLoginButton()
    if isLegalLoginInfo() then
        if not string.find(zentaoRoot, 'http:') then zentaoRoot = 'http://' .. zentaoRoot end
        password   = md5.sumhexa(password)

        loginDialog.cursor = 'busy'
        login()
        if isLogin then 
            setLoginInfo()
            loginDialog.visible = 'NO' 
        else
            loginDialog.cursor = 'arrow'
        end
    end
end


function showLoginDialog()
    urlLabel = iup.label{title = lang.login.url .. '��', size = lang.login.labelSize}
    urlInput = iup.text{size = lang.login.inputSize, value = zentaoRoot}
    urlBox   = iup.hbox{iup.fill{}, urlLabel, urlInput, iup.fill{}, margin='0x0', gap=0}

    accountLabel = iup.label{title = lang.login.account .. '��', size = lang.login.labelSize}
    accountInput = iup.text{size = lang.login.inputSize, value = account}
    accountBox   = iup.hbox{iup.fill{}, accountLabel, accountInput, iup.fill{}}

    passwordLabel = iup.label{title = lang.login.password .. '��', size = lang.login.labelSize}
    passwordInput = iup.text{size = lang.login.inputSize, password = 'YES'}
    passwordBox   = iup.hbox{iup.fill{}, passwordLabel, passwordInput, iup.fill{}}

    loginButton = iup.hbox{
        iup.fill{}, 
        iup.button
        {
            title = lang.login.loginButton,
            size = '50x25',
            action = function()
                zentaoRoot = urlInput.value
                account    = accountInput.value
                password   = passwordInput.value
                clickLoginButton()
            end
        }, 
        iup.fill{}
    }

    loginBox = iup.vbox{iup.fill{}, urlBox, accountBox, passwordBox, loginButton, iup.fill{}, margin = '5x1'}
    loginDialog = iup.dialog
    {
        loginBox,
        title = notifyTitle.loginPanel,
        size  = "200x400",
        resize = 'NO',
        icon = dialogIcon,
    }
    loginDialog:showxy(iup.RIGHT, iup.CENTER)

    function loginDialog:k_any(k)
         if k == KEY_ENTER then
             zentaoRoot = urlInput.value
             account    = accountInput.value
             password   = passwordInput.value

             clickLoginButton()
         elseif k == KEY_ESC then
              loginDialog.tray = 'NO'
              exitProcess()
         end
    end

    function loginDialog:close_cb() loginDialog.tray = 'NO' exitProcess() end

    if iup.MainLoopLevel() == 0 then
      iup.MainLoop()
    end
end

------------------------------------------
--- The logic of login.
------------------------------------------
if not zentaoRoot or not account or not password then 
    showLoginDialog() 
else
    login()
    if not isLogin then showLoginDialog() end
end

---------------------------------------------------------------------
--- Process data.
---------------------------------------------------------------------
------------------------------------------
--- Set pager info.
------------------------------------------
function setPagerInfo(pager, object, type)
    if not pager then return end
    pagerInfo[object] = {}
    pagerInfo[object][type] = {}
    pagerInfo[object][type]['recTotal']   = pager.recTotal
    pagerInfo[object][type]['recPerPage'] = pager.recPerPage
    pagerInfo[object][type]['pageID']     = pager.pageID
    pagerInfo[object][type]['totalPage']  = math.ceil(pager.recTotal / pager.recPerPage)
end

------------------------------------------
--- Set page summary.
------------------------------------------
function setPageSummary(object, type)
    if not pagerInfo[object] then return end
    pageSummary.title = string.format(
        lang.pager.summary, 
        pagerInfo[object][type].recTotal, 
        pagerInfo[object][type].recPerPage, 
        pagerInfo[object][type].pageID, 
        pagerInfo[object][type].totalPage
    )
end

------------------------------------------
--- Get the url of data list.
------------------------------------------
function getAPIa(object, type, pageID)
    if pagerInfo[object] and pagerInfo[object][type] then
        if config.requestType == 'GET' then
            return (zentaoRoot .. "/index.php?m=my&f=" .. object .. "&t=json&type=" .. type .. "&recTotal=" .. pagerInfo[object][type].recTotal .. "&recPerPage=" .. pagerInfo[object][type].recPerPage .. "&pageID=" .. pageID .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID)
        else
            return (zentaoRoot .. "/my-" .. object .. "-" .. pagerInfo[object][type].recTotal .. "-" .. pagerInfo[object][type].recPerPage .. "-" .. pageID .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID)
        end
    else
        if config.requestType == 'GET' then
            return (zentaoRoot .. "/index.php?m=my&f=" .. object .. "&t=json&type=" .. type .. "&" .. session.data.sessionName .. "=" .. session.data.sessionID)
        else
            return (zentaoRoot .. "/my-" .. object .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID)
        end
    end

--[[
    if pagerInfo[object] and pagerInfo[object][type] then
        if type then
            return (zentaoRoot .. "/my-" .. object .. "-" .. type .. "-" .. pagerInfo[object][type].recTotal .. "-" .. pagerInfo[object][type].recPerPage .. "-" .. pageID .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID)
        else
            return (zentaoRoot .. "/my-" .. object .. "-" .. pagerInfo[object][type].recTotal .. "-" .. pagerInfo[object][type].recPerPage .. "-" .. pageID .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID)
        end
    else
        if type then
            return (zentaoRoot .. "/my-" .. object .. "-" .. type .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID)
        else
            return (zentaoRoot .. "/my-" .. object .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID)
        end
    end
    ]]
end

------------------------------------------
--- Get data of object-type.
------------------------------------------
function getData(object, type, pageID)
    local data = {}
    local dataAPI

    dataAPI = getAPI{'my', object, ['type'] = type, ['pageID'] = pageID}

    http.request{url = dataAPI, method = "GET", sink = ltn12.sink.table(data)}
    if table.getn(data) == 0 then return {} end
    data = json.decode(table.concat(data))
    data = json.decode(data.data)

    setPagerInfo(data.pager, object, type)
    setPageSummary(object, type)

    data = data[object .. 's']
    return data
end

------------------------------------------
--- Create the data list when change tab.
------------------------------------------
data = {}
data.todo = {}
data.task = {}
data.bug  = {}
function createList(object, tab, pageID)
    mainDialog.cursor = 'busy'

    local type = tab.tip

    currentTab['parent'] = object
    currentTab['child']  = type

    data[object][type] = getData(object, type, pageID)
    for i, val in pairs(data[object][type]) do 
        title  = object == 'bug' and val.title or val.name 
        tab[i] = "#" .. val.id .. ' ' .. encode.u82a(title)
        tab.SPACING = 2
        tab.dblclick_cb = function(arg1, arg2, title) 
            local objectID = string.sub(title, string.find(title, '%d+'))
            local viewAPI = getAPI{object, 'view', [object] = objectID}          
            os.execute('start ' .. viewAPI)
        end
    end

    mainDialog.cursor = 'arrow'
end

---------------------------------------------------------------------
--- Main/data dialog. 
---------------------------------------------------------------------
------------------------------------------
--- Init tabs of object according to lang.
------------------------------------------
function initTabs(source)
  local tabs = {}
  for k,v in pairs(source) do
    tabs[k] = iup.list{expand = 'YES', tip=k, dropdown='NO', BGCOLOR = "234 234 234"}
    tabs[k].tabtitle = source[k]
  end
  return tabs
end

------------------------------------------
--- Todo tabs.
------------------------------------------
todoTabs = initTabs(lang.todo)

todoBox = iup.tabs
{
    todoTabs.today,
    todoTabs.yesterday,
    todoTabs.thisweek,
    todoTabs.lastweek,
}
todoBox.tabtitle = lang.todoTab
function todoBox:tabchange_cb(newTab, oldTab) createList('todo', newTab) end

------------------------------------------
--- Task tabs.
------------------------------------------
taskTabs = initTabs(lang.task)
taskBox = iup.tabs
{
    taskTabs.assignedto,
    taskTabs.openedby,
    taskTabs.finishedby,
    taskTabs.closedby,
}
taskBox.tabtitle = lang.taskTab
function taskBox:tabchange_cb(newTab, oldTab) createList('task', newTab) end

------------------------------------------
--- Bug tabs.
------------------------------------------
bugTabs = initTabs(lang.bug)
bugBox = iup.tabs
{
    bugTabs.assigntome,
    bugTabs.openedbyme,
    bugTabs.resolvedbyme,
    bugTabs.closedbyme
}
bugBox.tabtitle = lang.bugTab
function bugBox:tabchange_cb(newTab, oldTab) createList('bug', newTab) end

------------------------------------------
--- Page box.
------------------------------------------
prePage     = iup.label{title = lang.pager.pre}
nextPage    = iup.label{title = lang.pager.next}
lastPage    = iup.label{title = lang.pager.last}
pageSummary = iup.label{title = lang.pager.summary}
pagerBox    = iup.hbox{iup.fill{size = '10x1'}, pageSummary, iup.fill{}, prePage, nextPage, lastPage, iup.fill{size = '10x1'}, gap = 5}

function prePage:button_cb(btn, press)
    if(btn ~= MOUSE_LEFT or press ~= MOUSE_PRESSED) then return false end
    local object = currentTab.parent
    local type   = currentTab.child
    local tab    = object == 'todo' and todoTabs[type] or (object == 'task' and taskTabs[type] or bugTabs[type])
    if pagerInfo[object][type].pageID ~= 1 then
        pagerInfo[object][type].pageID = pagerInfo[object][type].pageID - 1 
        createList(object, tab, pagerInfo[object][type].pageID)
    end
end

function nextPage:button_cb(btn, press)
    if(btn ~= MOUSE_LEFT or press ~= MOUSE_PRESSED) then return false end
    local object = currentTab.parent
    local type   = currentTab.child
    local tab    = object == 'todo' and todoTabs[type] or (object == 'task' and taskTabs[type] or bugTabs[type])

    if tonumber(pagerInfo[object][type].pageID) < pagerInfo[object][type].totalPage then
        pagerInfo[object][type].pageID = pagerInfo[object][type].pageID + 1
        createList(object, tab, pagerInfo[object][type].pageID)
    end
end

function lastPage:button_cb(btn, press)
    if(btn ~= MOUSE_LEFT or press ~= MOUSE_PRESSED) then return false end
    local object = currentTab.parent
    local type   = currentTab.child
    local tab    = object == 'todo' and todoTabs[type] or (object == 'task' and taskTabs[type] or bugTabs[type])
    if tonumber(pagerInfo[object][type].pageID) ~= pagerInfo[object][type].totalPage then
        pagerInfo[object][type].pageID = pagerInfo[object][type].totalPage
        createList(object, tab, pagerInfo[object][type].pageID)
    end
end

------------------------------------------
--- Header box.
------------------------------------------
welcomeLabel = iup.label{title = string.format(lang.header.welcome, account)} 
logoutLabel  = iup.label{title = lang.header.logout}
function logoutLabel:button_cb(btn, press)
    if(btn ~= MOUSE_LEFT or press ~= MOUSE_PRESSED) then return false end
    os.remove(LOGIN_FILE)
    os.remove(PROCESS_EXIT)
    mainDialog.tray = 'NO'
    exitProcess()
end
headerBox = iup.hbox{iup.fill{size='10x1'}, welcomeLabel, iup.fill{}, logoutLabel, iup.fill{size='20x1'}}

------------------------------------------
--- Create and show main Dialog.
------------------------------------------
mainTabs = iup.tabs{todoBox, taskBox, bugBox}
mainDialog = iup.dialog
{
    iup.vbox
    {
        headerBox,
        mainTabs,
        pagerBox
    },
    title = notifyTitle.mainPanel,
    size  = "200x400",
    resize = 'NO',
    icon = dialogIcon,
    tray = 'YES',
    traytip =  notifyTitle.mainPanel,
    trayimage = dialogIcon,
    margin = '0x5',
    zorder = 'TOP'
}

function mainTabs:tabchange_cb(newTab, oldTab)
    if newTab.tabtitle == lang.taskTab then createList('task', taskTabs.assignedto) end
    if newTab.tabtitle == lang.bugTab then createList('bug', bugTabs.assigntome) end
end

function mainDialog:k_any(k)
    if k == KEY_ESC then mainDialog.tray = "NO" exitProcess() end
end

function mainDialog:close_cb()
    mainDialog.tray = "NO" 
    exitProcess()
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

if loginDialog then loginDialog.visible = 'NO' end
mainDialog:showxy(iup.RIGHT, iup.CENTER)
createList('todo', todoTabs.today)

---------------------------------------------------------------------
--- Timer.
---------------------------------------------------------------------
updateTimer = iup.timer{time = UPDATE_TIME, run = 'YES'}
closeTimer  = nil
actions = {}

------------------------------------------
--- Check the new action from ZenTaoPMS.
------------------------------------------
function updateTimer:action_cb()
    actions = {}
    local response = {}
    local actionAPI

    local actionID = 0
    local f = io.open(ACTIONID_FILE, 'r')
    if f then actionID = tonumber(f:read('*a')) or 0 f:close() end

    if config.requestType == 'GET' then
        actionAPI = zentaoRoot .. "/?m=api&f=getmodel&moduleName=action&methodName=getUnreadActions&" .. "actionID=" .. actionID .. "&t=json&" .. session.data.sessionName .. "=" .. session.data.sessionID
    else
        actionAPI = zentaoRoot .. "/api-getmodel-action-getUnreadActions-actionID=" .. actionID .. ".json?" .. session.data.sessionName .. "=" .. session.data.sessionID 
    end

    http.request{url = actionAPI, method = "GET", sink = ltn12.sink.table(response)}
    if pcall(json.decode, table.concat(response)) then 
        response = json.decode(table.concat(response))
        response.data = json.decode(json.decode(response.data))
        for object, list in pairs(response.data) do
            table.foreach(list, function(i, v) table.insert(actions, v) end)
        end
    end

    if table.getn(actions) > 0 then
        showNotifyDialog()
    end
end

------------------------------------------
--- Create and show nofity Dialog.
------------------------------------------
function showNotifyDialog()
    updateTimer.run = 'NO'
    local list = iup.list{expand = 'YES'}
    local link = nil
    local actionID = 0
    for i, v in pairs(actions) do
      if tonumber(v.actionID) > actionID then actionID = tonumber(v.actionID) end
       list[i] = encode.u82a(v.action)
       list.dblclick_cb = function(...)
           link = getAPI{v.objectType, 'view', [v.objectType] = v.objectID}
           os.execute('start ' .. link)
       end
    end

    local f = io.open(ACTIONID_FILE, 'w')
    if f then f:write(actionID) f:close() end

    local notifyDlg = iup.dialog
    {
        list,
        title = notifyTitle.notify,
        size = '200x150',
        icon = dialogIcon
    }
    notifyDlg:showxy(iup.RIGHT, iup.RIGHT)

    closeTimer = iup.timer{time = CLOSE_TIME, run = 'YES'}
    function closeTimer:action_cb()
        closeTimer.run = 'NO'
        if notifyDlg then  notifyDlg.visible = 'NO' updateTimer.run = 'YES' end
    end

    if iup.MainLoopLevel() == 0 then
      iup.MainLoop()
    end
end

if iup.MainLoopLevel() == 0 then
  iup.MainLoop()
end

