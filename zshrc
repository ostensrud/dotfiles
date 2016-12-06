# Set up the prompt

autoload -Uz promptinit
autoload -U colors && colors

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:git*' formats "(%{$fg[yellow]%}%b%{$reset_color%}%{$fg[red]%}%u%{$fg[green]%}%c%{$reset_color%})"
precmd() { vcs_info }
setopt prompt_subst
PROMPT='%{%F{069}%}[%{$reset_color%}%{%F{051}%}%D{%H:%M:%S}%{$reset_color%} %{$fg[blue]%}%n%{$reset_color%}@%{$fg[cyan]%}%m%{$reset_color%}%{%F{069}%}]%{$reset_color%} %{%F{070}%}%2~%{$reset_color%}${vcs_info_msg_0_} $ '

setopt histignorealldups sharehistory
setopt CORRECT

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

export PATH=$PATH:/usr/local/src/nodejs/bin

# Use modern completion system
autoload -Uz compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

alias ls="ls --color=auto"
alias ll="ls -l"
alias lh="ls -lh"
alias la="ls -lha"

#alias syi="sudo yum install"
alias syu="sudo yum update"

export VISUAL=vim
export EDITOR="$VISUAL"
