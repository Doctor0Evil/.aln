Run PSModule/install-powershell@v1
  with:
    Version: latest
Run # Install-PowerShell
  # Install-PowerShell
  set -e
  echo "Requested version: [$REQUESTED_VERSION]"
  
  # Only resolve to latest version if explicitly set to 'latest' (case-insensitive)
  case "${REQUESTED_VERSION:-}" in
      [Ll][Aa][Tt][Ee][Ss][Tt])
          REQUESTED_VERSION=$(
            curl -s -f \
              -H "Accept: application/vnd.github+json" \
              -H "Authorization: ***" \
              -H "X-GitHub-Api-Version: 2022-11-28" \
              https://api.github.com/repos/PowerShell/PowerShell/releases/latest |
              jq -r '.tag_name' | sed 's/^v//'
          )
          echo "Latest stable PowerShell release detected: $REQUESTED_VERSION"
          ;;
      "")
          echo "Error: Version input is required (or use 'latest')"
          exit 1
          ;;
  esac
  
  DETECTED_VERSION=$(pwsh -NoLogo -NoProfile -Command '$PSVersionTable.PSVersion.ToString()' 2>/dev/null || true)
  if [[ -n "$DETECTED_VERSION" ]]; then
    echo "Currently installed PowerShell version: $DETECTED_VERSION"
  else
    echo "PowerShell is not currently installed"
  fi
  
  if [[ "$DETECTED_VERSION" == "$REQUESTED_VERSION" ]]; then
    echo "PowerShell $DETECTED_VERSION already installed. Skipping."
    exit 0
  fi
  
  # Determine Linux distribution type
  ARCH=$(dpkg --print-architecture 2>/dev/null || rpm --eval '%{_arch}' 2>/dev/null || echo "x86_64")
  
  if command -v apt-get >/dev/null || command -v dpkg >/dev/null; then
    # Debian/Ubuntu based
    echo "Detected Debian/Ubuntu based system..."
    DEB_NAME="powershell_${REQUESTED_VERSION}-1.deb_${ARCH}.deb"
    URL="https://github.com/PowerShell/PowerShell/releases/download/v${REQUESTED_VERSION}/${DEB_NAME}"
    echo "Downloading from: $URL"
    wget -q "$URL" -O "$DEB_NAME"
    echo "Starting installation of PowerShell [$REQUESTED_VERSION]..."
    sudo dpkg -i "$DEB_NAME" || sudo apt-get -f install -y
  elif command -v rpm >/dev/null; then
    # RHEL/Fedora/CentOS based
    echo "Detected RHEL/Fedora/CentOS based system..."
    if [[ "$ARCH" == "aarch64" ]]; then
      RPM_NAME="powershell-${REQUESTED_VERSION}-1.rh.${ARCH}.rpm"
    else
      RPM_NAME="powershell-${REQUESTED_VERSION}-1.rh.x86_64.rpm"
    fi
    URL="https://github.com/PowerShell/PowerShell/releases/download/v${REQUESTED_VERSION}/${RPM_NAME}"
    echo "Downloading from: $URL"
    wget -q "$URL" -O "$RPM_NAME"
    echo "Starting installation of PowerShell [$REQUESTED_VERSION]..."
    sudo rpm -i "$RPM_NAME" || sudo yum install -y "$RPM_NAME"
  else
    echo "Unsupported Linux distribution. Cannot determine package format."
    exit 1
  fi
  echo "Installation complete. PowerShell [$REQUESTED_VERSION] is now available."
  shell: /usr/bin/bash --noprofile --norc -e -o pipefail {0}
  env:
    REQUESTED_VERSION: latest
    GITHUB_TOKEN: ***
Requested version: [latest]
Latest stable PowerShell release detected: 7.5.2
Currently installed PowerShell version: 7.4.11
Detected Debian/Ubuntu based system...
Downloading from: https://github.com/PowerShell/PowerShell/releases/download/v7.5.2/powershell_7.5.2-1.deb_amd64.deb
Starting installation of PowerShell [7.5.2]...
(Reading database ... 219877 files and directories currently installed.)
Preparing to unpack powershell_7.5.2-1.deb_amd64.deb ...
Unpacking powershell (7.5.2-1.deb) over (7.4.11-1.deb) ...
Setting up powershell (7.5.2-1.deb) ...
Processing triggers for man-db (2.12.0-4build2) ...
Installation complete. PowerShell [7.5.2] is now available.
2s
Run pwsh -NoProfile -ExecutionPolicy Bypass -File scripts/run-all-ai-tests.ps1
  pwsh -NoProfile -ExecutionPolicy Bypass -File scripts/run-all-ai-tests.ps1
  shell: /usr/bin/pwsh -command ". '{0}'"
The argument 'scripts/run-all-ai-tests.ps1' is not recognized as the name of a script file. Check the spelling of the name, or if a path was included, verify that the path is correct and try again.

Usage: pwsh[.exe] [-Login] [[-File] <filePath> [args]]
                  [-Command { - | <script-block> [-args <arg-array>]
                                | <string> [<CommandParameters>] } ]
                  [-CommandWithArgs <string> [<CommandParameters>]
                  [-ConfigurationName <string>] [-ConfigurationFile <filePath>]
                  [-CustomPipeName <string>] [-EncodedCommand <Base64EncodedCommand>]
                  [-ExecutionPolicy <ExecutionPolicy>] [-InputFormat {Text | XML}]
                  [-Interactive] [-MTA] [-NoExit] [-NoLogo] [-NonInteractive] [-NoProfile]
                  [-NoProfileLoadTime] [-OutputFormat {Text | XML}] 
                  [-SettingsFile <filePath>] [-SSHServerMode] [-STA] 
                  [-Version] [-WindowStyle <style>] 
                  [-WorkingDirectory <directoryPath>]

       pwsh[.exe] -h | -Help | -? | /?

PowerShell Online Help https://aka.ms/powershell-docs

All parameters are case-insensitive.
Error: Process completed with exit code 1.
0s
0s
Post job cleanup.
/usr/bin/git version
git version 2.51.0
Temporarily overriding HOME='/home/runner/work/_temp/aa611172-ce18-43d8-b342-43ea47aa95bd' before making global git config changes
Adding repository directory to the temporary git global config as a safe directory
/usr/bin/git config --global --add safe.directory /home/runner/work/ALN_Programming_Language/ALN_Programming_Language
/usr/bin/git config --local --name-only --get-regexp core\.sshCommand
/usr/bin/git submodule foreach --recursive sh -c "git config --local --name-only --get-regexp 'core\.sshCommand' && git config --local --unset-all 'core.sshCommand' || :"
/usr/bin/git config --local --name-only --get-regexp http\.https\:\/\/github\.com\/\.extraheader
http.https://github.com/.extraheader
/usr/bin/git config --local --unset-all http.https://github.com/.extraheader
/usr/bin/git submodule foreach --recursive sh -c "git config --local --name-only --get-regexp 'http\.https\:\/\/github\.com\/\.extraheader' && git config --local --unset-all 'http.https://github.com/.extraheader' || :"
0s
Cleaning up orphan processes
