parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/ git:(\1)/'
}

export CLICOLOR=1
export LSCOLORS=cxgxfxexbxegedabagacad

# Enable prompt substitution
setopt PROMPT_SUBST

export PS1='%F{green}ferhadme%f%F{red} :: %f%F{green}%~%f %F{blue}»%f%F{magenta}$(type parse_git_branch >/dev/null 2>&1 && parse_git_branch)%f $ '

intro_em() {
    echo "¯\_(ツ)_/¯"
    echo
}

intro_em

alias ll='ls -alF'
alias la='ls -A'
alias l='ls -CF --color=auto'
alias ls='ls -l'
alias src='cd ~/Programming/'
alias rg='rg --hidden'

setopt noautomenu
setopt nomenucomplete
autoload -Uz compinit && compinit

export JAVA_HOME=/Library/Java/JavaVirtualMachines/amazon-corretto-21.jdk/Contents/Home
export GRADLE_HOME=/opt/gradle/gradle-9.1.0/bin
export PATH=$PATH:$GRADLE_HOME

export NVM_DIR="$HOME/.nvm"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"
[ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"

if [ -f '/opt/google-cloud-sdk/path.zsh.inc' ]; then . '/opt/google-cloud-sdk/path.zsh.inc'; fi

if [ -f '/opt/google-cloud-sdk/completion.zsh.inc' ]; then . '/opt/google-cloud-sdk/completion.zsh.inc'; fi

export PATH="$HOME/.local/bin:$PATH"
export MYVIMRC=~/.vimrc

