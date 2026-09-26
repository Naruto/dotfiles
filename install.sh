#!/usr/bin/env bash

set -e

DOTFILES_PATH=$(cd "$(dirname "$0")" && pwd)

echo "Setting up dotfiles..."

mkdir -p ~/projects
mkdir -p ~/.config
mkdir -p ~/.claude

link() {
  local src="$1"
  local dest="$2"
  if [[ -L "$dest" && "$(readlink "$dest")" == "$src" ]]; then
    echo "  skip (already linked): $dest"
  else
    ln -sfn "$src" "$dest"
    echo "  linked: $dest -> $src"
  fi
}

# Keys in src win; keys only in dest (written by the app itself) are kept.
# Arrays keep the entries added in dest. The src merged last time is kept next
# to dest, so entries removed from src are removed from dest too.
merge_json() {
  local src="$1"
  local dest="$2"
  local last
  last="$(dirname "$dest")/.$(basename "$src")"
  local prev=null
  if [[ -f "$last" ]]; then
    prev=$(cat "$last")
  fi
  local merged
  if [[ -f "$dest" ]]; then
    merged=$(jq --argjson prev "$prev" --slurpfile src "$src" '
      def merge($prev; $src):
        if ($src | type) == "object" and type == "object" then
          reduce ($src | keys_unsorted[]) as $k (.;
            .[$k] |= merge($prev | if type == "object" then .[$k] else null end; $src[$k]))
        elif ($src | type) == "array" and type == "array" then
          ($src + ($prev | if type == "array" then . else [] end)) as $known
          | $src + map(select(. as $x | $known | any(.[]; . == $x) | not))
        else
          $src
        end;
      merge($prev; $src[0])' "$dest")
  else
    merged=$(jq . "$src")
  fi
  if [[ -f "$dest" && "$merged" == "$(jq . "$dest")" ]]; then
    echo "  skip (already merged): $dest"
  else
    printf '%s\n' "$merged" > "$dest"
    echo "  merged: $src -> $dest"
  fi
  cp "$src" "$last"
}

echo "Creating symlinks..."
link "${DOTFILES_PATH}/.emacs.d" ~/".emacs.d"
link "${DOTFILES_PATH}/.zshrc" ~/".zshrc"
link "${DOTFILES_PATH}/.zsh" ~/".zsh"
link "${DOTFILES_PATH}/.zfunc" ~/".zfunc"
link "${DOTFILES_PATH}/.tmux" ~/".tmux"
link "${DOTFILES_PATH}/.tmux/.tmux.conf" ~/".tmux.conf"
link "${DOTFILES_PATH}/.tmux.conf.local" ~/".tmux.conf.local"
link "${DOTFILES_PATH}/.starship" ~/".starship"
link "${DOTFILES_PATH}/.config/lazygit" ~/.config/"lazygit"
link "${DOTFILES_PATH}/.config/yazi" ~/.config/"yazi"
link "${DOTFILES_PATH}/.config/bat" ~/.config/"bat"
link "${DOTFILES_PATH}/.config/zsh-abbr" ~/.config/"zsh-abbr"
link "${DOTFILES_PATH}/.config/ghostty" ~/.config/"ghostty"
link "${DOTFILES_PATH}/.ripgreprc" ~/".ripgreprc"

echo "Merging settings..."
merge_json "${DOTFILES_PATH}/.claude/settings.common.json" ~/.claude/"settings.json"

echo "Dotfiles setup complete!"
