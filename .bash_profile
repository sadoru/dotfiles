export PATH=/usr/local/bin:$PATH
export EDITOR="emacsclient"

#Cpputest
export CPPUTEST_HOME=/usr/local/Cellar/cpputest/3.6

alias ls="ls -G"
alias ll="la -l"
alias la="ls -a"

alias cd.="cd .."
alias cd..="cd ../../"

alias javac='javac -J-Dfile.encoding=UTF-8'
alias java='java -Dfile.encoding=UTF-8'

alias jenkins="java -jar /usr/local/Cellar/jenkins/1.569/libexec/jenkins.war"

alias htdocs="cd /Applications/MAMP/htdocs"

alias Emacs="open -a /Applications/Emacs.app"
#alias emacs="open -a Emacs"

# Bash completion script for brew(1)n
. $(brew --repository)/Library/Contributions/brew_bash_completion.sh

## git
if [ -f /opt/local/share/doc/git-core/contrib/completion/git-completion.bash ]; then
    source /opt/local/share/doc/git-core/contrib/completion/git-completion.bash
fi

## create emacs env file
perl -wle \
    'do {print qq/(setenv "$_" "$ENV{$_}")/if exists $ENV{$_}} for @ARGV' \
    PATH > ~/.emacs.d/conf/shellenv.el
eval "$(rbenv init -)"
