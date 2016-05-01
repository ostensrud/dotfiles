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
export VISUAL=vim
export EDITOR="$VISUAL"
export GIT_PS1_SHOWDIRTYSTATE=1

alias syi="sudo yum install"
alias syu="sudo yum update"

alias ll="ls -l"
