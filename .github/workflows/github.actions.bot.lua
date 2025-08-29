name: GitHub Actions Lua Bot

on:
  workflow_dispatch:
  push:

jobs:
  matrix-lua-bot:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, windows-latest, macos-latest]
        lua: [5.3, 5.4, 'luajit']
    steps:
      - name: Checkout code
        uses: actions/checkout@v4

      - name: Set up Lua
        uses: leafo/gh-actions-lua@v9
        with:
          lua-version: ${{ matrix.lua }}

      - name: Install LuaRocks dependencies (luasocket, dkjson)
        run: |
          luarocks install luasocket
          luarocks install dkjson

      - name: Run GitHub Actions Lua Bot
        env:
          GITHUBPAT: ${{ secrets.GITHUBPAT }}
          GITHUBREPO: ${{ github.repository }}
          GITHUBBRANCH: ${{ github.ref_name }}
        run: |
          lua .github/actions/github-actions-bot/github.actions.bot.lua list
