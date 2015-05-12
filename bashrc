# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi


[ -z "$PS1" ] && return
export PS1='[\t \[\e[0;34m\]\u\[\e[0m\]@\[\e[0;36m\]\h\[\e[0m\] \W]\[\e[0;33m\]$(__git_ps1)\[\e[0m\]$ '
# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups

# User specific aliases and functions
export EDITOR=vim
export GIT_PS1_SHOWDIRTYSTATE=1

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
