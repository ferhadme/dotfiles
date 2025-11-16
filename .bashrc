#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

### Aliases
alias ls='ls --color=auto'
alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF --color=auto'

alias grep='grep --color=auto'
alias ls='ls -G'
alias diff='colordiff'

alias em='emacs -nw -q'
alias ed="ed -p ':'"

alias setclip='xclip -selection c'

alias src='cd ~/Programming'
### Aliases_End

### History
HISTCONTROL=ignoreboth
shopt -s histappend
HISTSIZE=1000
HISTFILESIZE=2000
### History_End

### PS1
parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ git:(\1)/'
}

export PS1="$(whoami)\[\e[31m\] \[\e[m\]\[\e[31m\]:\[\e[m\]\[\e[31m\]: \[\e[m\]\[\e[32m\]\w\[\e[m\] \[\e[34m\]»\[\e[m\]\[\e[35m\]\$(parse_git_branch)\[\e[0m\] $ "

intro_em() {
    echo "¯\_(ツ)_/¯"
    echo
}


### PS1_End

### Exports
export LSCOLORS=cxgxfxexbxegedabagacad
export CLICOLOR=1
export EDITOR='vim'
### Exports_End



