name: Bit.Hub Universal Compliance Audit
on: [push, pull_request, workflow_dispatch]
permissions:
  contents: read
  security-events: write
env:
  BITHUBREPOURL: https://github.com/Doctor0Evil/Bit.Hub.git
jobs:
  compliance-wall:
    runs-on: ubuntu-latest
    steps:
    - name: Checkout repository
      uses: actions/checkout@v4
    - name: Sync Bit.Hub Policy Modules
      run: |
        git clone --depth=1 $BITHUBREPOURL tmpbithub
        rsync -av --ignore-existing tmpbithub/.bithub/policies .bithub/policies
    - name: OPA Compliance Gate
      run: |
        curl -L -o opa https://openpolicyagent.org/downloads/latest/opa_linux_amd64
        chmod +x opa
        ./opa eval --format pretty --data .bithub/policies --input workflows/context.json data.bithub.personalitycore.allow
