# Set up the prompt

autoload -Uz promptinit
autoload -U colors && colors

autoload -Uz vcs_info
zstyle ':vcs_info:*' enable git
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:git*' formats "(%{$fg[yellow]%}%b%{$reset_color%}%{$fg[red]%}%u%{$fg[green]%}%c%{$reset_color%})"
precmd() { vcs_info }
setopt prompt_subst
PROMPT='[%* %{$fg[blue]%}%n%{$reset_color%}@%{$fg[cyan]%}%m%{$reset_color%} %~]${vcs_info_msg_0_}$ '
#PROMPT='${vcs_info_msg_0_} '


setopt histignorealldups sharehistory

# Use emacs keybindings even if our EDITOR is set to vi
bindkey -e

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

alias ll="ls -l"
alias tmapp="cd ~/dev/ergotmapp"
alias tmlog="tail -fn 0 ~/dev/ergotmapp/var/log/httpd-ws/error_log"
alias dsfdb="cd ~/dev/dsfdb"
alias ergobo="cd ~/dev/ergobo"
alias iftorg="cd ~/dev/infotorg"
alias syi="sudo yum install"
alias syu="sudo yum update"
alias tunnels-start='bash -c "~/dev/autotunnel/autoSSH/ssh-tunnels.sh ytensrud"'
alias tunnels-stop='sudo killall -9 autossh;sudo killall -9 ssh'
alias tunnels-restart='sudo killall -9 autossh;sudo killall -9 ssh; sleep 5;bash -c "~/dev/autotunnel/autoSSH/ssh-tunnels.sh ytensrud"'
