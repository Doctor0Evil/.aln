-- github.actions.bot.lua
-- Autonomous GitHub Actions Bot (LUA)
-- Requires: LuaSocket (for HTTP), JSON (dkjson or cjson)
local http = require("socket.http")
local ltn12 = require("ltn12")
local json = require("dkjson")

-- CONFIG (set via ENV for security)
local GITHUB_PAT = os.getenv("GITHUBPAT")
local GITHUB_REPO = os.getenv("GITHUBREPO") or "Doctor0Evil/ALNProgrammingLanguage"
local BRANCH = os.getenv("GITHUBBRANCH") or "main"

assert(GITHUB_PAT, "GitHub Personal Access Token must be set in env: GITHUBPAT")

-- Helper to perform authenticated GitHub API requests
local function github_api(endpoint, method, body)
  local headers = {
    ["Authorization"] = "token " .. GITHUB_PAT,
    ["Accept"] = "application/vnd.github.v3+json",
    ["User-Agent"] = "lua-github-actions-bot"
  }
  local response = {}
  local request_body = body and json.encode(body) or ""
  if request_body ~= "" then headers["Content-Type"] = "application/json" end

  local _, code = http.request{
    url = endpoint,
    method = method or "GET",
    headers = headers,
    source = ltn12.source.string(request_body),
    sink = ltn12.sink.table(response)
  }
  return code, table.concat(response)
end

-- List workflow runs (latest)
local function list_runs()
  local uri = "https://api.github.com/repos/"..GITHUB_REPO.."/actions/runs"
  local code, res = github_api(uri, "GET")
  if code == 200 then
    local data, _, err = json.decode(res)
    if not err then
      for _, run in ipairs(data.workflow_runs or {}) do
        print("[#"..run.id.."] "..run.name.." | "..run.status.." | "..run.head_branch)
      end
    end
  else
    print("API error: "..tostring(code))
  end
end

-- Trigger a workflow_dispatch manually
local function dispatch_workflow(workflow_file, ref)
  local uri = "https://api.github.com/repos/"..GITHUB_REPO.."/actions/workflows/"..workflow_file.."/dispatches"
  local code, res = github_api(uri, "POST", {ref=ref or BRANCH})
  if code == 204 then
    print("Workflow dispatched: "..workflow_file.." on ref "..(ref or BRANCH))
  else
    print("Dispatch failed: "..tostring(code).." "..res)
  end
end

-- Watch for failed runs and auto-rerun (self-healing)
local function auto_rerun_failures()
  local uri = "https://api.github.com/repos/"..GITHUB_REPO.."/actions/runs"
  local code, res = github_api(uri, "GET")
  if code == 200 then
    local data = json.decode(res)
    for _, run in ipairs(data.workflow_runs or {}) do
      if run.conclusion == "failure" then
        print("Auto-retrying failed workflow: "..run.id)
        local rerun_uri = "https://api.github.com/repos/"..GITHUB_REPO.."/actions/runs/"..run.id.."/rerun"
        github_api(rerun_uri, "POST")
      end
    end
  end
end

-- Entrypoint
local cmd = arg[1]
if cmd == "list" then
  list_runs()
elseif cmd == "dispatch" then
  assert(arg[2], "Usage: lua github.actions.bot.lua dispatch <workflow_file.yml>")
  dispatch_workflow(arg[2], arg[3])
elseif cmd == "rerun" then
  auto_rerun_failures()
else
  print("GitHub Actions Bot - LUA\nUsage:")
  print("  lua github.actions.bot.lua list")
  print("  lua github.actions.bot.lua dispatch <workflow_file> [branch]")
  print("  lua github.actions.bot.lua rerun")
end
