# Microsoft.PowerShell_profile.ps1
# Ported from .zshrc for consistent user experience on Windows

# --- PSReadLine (Autosuggestions, Syntax Highlighting, History) ---
Import-Module PSReadLine
Set-PSReadLineOption -EditMode Emacs
Set-PSReadLineOption -BellStyle None

# 予測補完機能（Prediction）は PSReadLine 2.1.0 以上が必要
$psrVersion = (Get-Module PSReadLine).Version
if ($psrVersion -ge [Version]"2.1.0") {
    Set-PSReadLineOption -PredictionSource History
    if ($psrVersion -ge [Version]"2.2.0") {
        Set-PSReadLineOption -PredictionViewStyle InlineView
    }
}

# --- Environment Variables ---
$env:EDITOR = "code -w"
$env:PAGER = "less"
$env:BAT_PAGER = "less -R"

# --- Aliases & Functions ---

# 強力な保護がかかっている標準エイリアスを事前に削除する関数
function Remove-ProtectedAlias {
    param ([string]$AliasName)
    if (Get-Alias $AliasName -ErrorAction SilentlyContinue) {
        Remove-Item "Alias:$AliasName" -Force -ErrorAction SilentlyContinue
    }
}

# rm -i equivalent
function Remove-Item-Interactive { Remove-Item -Confirm $args }
Remove-ProtectedAlias -AliasName "rm"
Set-Alias rm Remove-Item-Interactive -Option AllScope -Force

# eza aliases
function l { eza -lbF --git $args }
function ll { eza -lbGF --git $args }
function llm { eza -lbGd --git --sort=modified $args }
function la { eza -lbhHigUmuSa --time-style=long-iso --git --color-scale $args }
function lx { eza -lbhHigUmuSa@ --time-style=long-iso --git --color-scale $args }

if (Get-Command eza -ErrorAction SilentlyContinue) {
    Remove-ProtectedAlias -AliasName "ls"
    Set-Alias ls eza
}
if (Get-Command bat -ErrorAction SilentlyContinue) {
    Remove-ProtectedAlias -AliasName "cat"
    Set-Alias cat bat
}
if (Get-Command btm -ErrorAction SilentlyContinue) {
    Remove-ProtectedAlias -AliasName "top"
    Set-Alias top btm
}

function rg { ripgrep.exe -p $args }
$env:RIPGREP_CONFIG_PATH = "$HOME\.ripgreprc"

# --- Tool Initializations ---

# Starship
if (Get-Command starship -ErrorAction SilentlyContinue) {
    Invoke-Expression (&starship init powershell)
}

# Zoxide
if (Get-Command zoxide -ErrorAction SilentlyContinue) {
    Invoke-Expression (& { (zoxide init powershell | Out-String) })
}

# Fzf
if (Get-Command fzf -ErrorAction SilentlyContinue) {
    # fzf 0.48.0 以上なら --powershell が使えるかチェック
    $fzfVersionOutput = (fzf --version).Split(" ")
    if ($fzfVersionOutput.Length -ge 1) {
        $fzfVersionStr = $fzfVersionOutput[0]
        if ([Version]$fzfVersionStr -ge [Version]"0.48.0") {
            Invoke-Expression (& { (fzf --powershell | Out-String) })
        } else {
            # 古いバージョンの場合は手動で最小限の設定
            function fzf-completion {
                $line = $null
                $cursor = $null
                [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)
                $res = fzf --query=$line
                if ($res) {
                    [Microsoft.PowerShell.PSConsoleReadLine]::Replace(0, $line.Length, $res)
                }
            }
        }
    }
    $env:FZF_DEFAULT_COMMAND = 'fd -HL --exclude ".git"'
    $env:FZF_CTRL_T_COMMAND = 'fd -HL --exclude ".git" --type f'
    $env:FZF_ALT_C_COMMAND = 'fd -HL --exclude ".git" --type d'
    $env:FZF_DEFAULT_OPTS = '--layout=reverse --ansi --border --bind ctrl-v:page-down,alt-v:page-up,ctrl-k:kill-line'
}

# --- Custom Keybindings & Functions ---

# ghq + fzf (Ctrl+])
Set-PSReadLineKeyHandler -Key 'Ctrl+]' -ScriptBlock {
    $line = $null
    $cursor = $null
    [Microsoft.PowerShell.PSConsoleReadLine]::GetBufferState([ref]$line, [ref]$cursor)
    $target_dir = ghq list -p | fzf --query=$line
    if ($target_dir) {
        [Microsoft.PowerShell.PSConsoleReadLine]::Replace(0, $line.Length, "cd `"$target_dir`"")
        [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine()
    }
}

# zoxide interactive (Ctrl+U)
Set-PSReadLineKeyHandler -Key 'Ctrl+u' -ScriptBlock {
    [Microsoft.PowerShell.PSConsoleReadLine]::Replace(0, 0, "zi ")
    [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine()
}

# yazi (Ctrl+O)
function y {
    $tmp = New-TemporaryFile
    yazi --cwd-file="$tmp"
    if (Test-Path $tmp) {
        $cwd = Get-Content $tmp
        if ($cwd -and $cwd -ne $PWD) {
            Set-Location $cwd
        }
        Remove-Item $tmp
    }
}

Set-PSReadLineKeyHandler -Key 'Ctrl+o' -ScriptBlock {
    [Microsoft.PowerShell.PSConsoleReadLine]::Replace(0, 0, "y")
    [Microsoft.PowerShell.PSConsoleReadLine]::AcceptLine()
}

# lazygit
function lg {
    $env:LAZYGIT_NEW_DIR_FILE = "$HOME\.lazygit\newdir"
    lazygit $args
    if (Test-Path $env:LAZYGIT_NEW_DIR_FILE) {
        Set-Location (Get-Content $env:LAZYGIT_NEW_DIR_FILE)
        Remove-Item $env:LAZYGIT_NEW_DIR_FILE
    }
}
