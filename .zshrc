# SSH
eval `keychain -q --eval id_ed25519`


# If you come from bash you might have to change your $PATH.
# export PATH=$HOME/bin:/usr/local/bin:$PATH
# Path to your oh-my-zsh installation.
export ZSH=/home/abbreviatedman/.oh-my-zsh

export ALTERNATE_EDITOR=""

VIM_MODE_VICMD_KEY='^Q'

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="dangerroom"

eval `dircolors /home/abbreviatedman/.dir_colors`

# Uncomment the following line to use case-sensitive completion.
# CASE_SENSITIVE="true"

# Uncomment the following line to use hyphen-insensitive completion. Case
# sensitive completion must be off. _ and - will be interchangeable.
# HYPHEN_INSENSITIVE="true"

# Uncomment the following line to disable bi-weekly auto-update checks.
# DISABLE_AUTO_UPDATE="true"

# Uncomment the following line to change how often to auto-update (in days).
# export UPDATE_ZSH_DAYS=13

# Uncomment the following line to disable colors in ls.
# DISABLE_LS_COLORS="true"

# Uncomment the following line to disable auto-setting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment the following line to enable command auto-correction.
# ENABLE_CORRECTION="true"

# Uncomment the following line to display red dots whilst waiting for completion.
# COMPLETION_WAITING_DOTS="true"

# Uncomment the following line if you want to disable marking untracked files
# under VCS as dirty. This makes repository status check for large repositories
# much, much faster.
# DISABLE_UNTRACKED_FILES_DIRTY="true"

# Uncomment the following line if you want to change the command execution time
# stamp shown in the history command output.
# The optional three formats: "mm/dd/yyyy"|"dd.mm.yyyy"|"yyyy-mm-dd"
# HIST_STAMPS="mm/dd/yyyy"

# Would you like to use another custom folder than $ZSH/custom?
# ZSH_CUSTOM=/path/to/new-custom-folder

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
# Add wisely, as too many plugins slow down shell startup.
plugins=(
  git
  npm
  asdf web-search
  command-not-found
  zsh-syntax-highlighting
  zsh-autosuggestions
  zsh-vim-mode
)
source $ZSH/oh-my-zsh.sh

# User configuration

# zsh autusuggest color
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=#586e75'

# Different cursors based on vim modes
MODE_CURSOR_VIINS="#00ff00 blinking bar"
MODE_CURSOR_REPLACE="$MODE_CURSOR_VIINS #ff0000"
MODE_CURSOR_VICMD="green block"
MODE_CURSOR_SEARCH="#ff00ff steady underline"
MODE_CURSOR_VISUAL="$MODE_CURSOR_VICMD steady bar"
MODE_CURSOR_VLINE="$MODE_CURSOR_VISUAL #00ffff"
# export MANPATH="/usr/local/man:$MANPATH"

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

export EDITOR=/usr/sbin/emacsclient
export VISUAL=$EDITOR

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# ssh
# export SSH_KEY_PATH="~/.ssh/rsa_id"


# Aliases!

# doom emacs
alias ds='doom sync'
alias ddoc='doom doctor'

# pacman and yay
alias yay='yay --color=always'
alias pacman='pacman --color=always'

# kitty
alias icat='kitty icat'

# yadm
alias yst='yadm status'
alias ya='yadm add'
alias yc='yadm commit -v'
alias ycp='yadm commit -p'
alias yp='yadm push'
alias ypu='yadm pull'
alias yd='yadm difftool'

# open config files
alias zshconfig="e /home/abbreviatedman/.zshrc"
alias i3config="e /home/abbreviatedman/.i3/config"
alias vimconfig="e /home/abbreviatedman/.vimrc"
alias kittyconfig="e /home/abbreviatedman/.config/kitty/kitty.conf"
alias qtileconfig="e /home/abbreviatedman/.config/qtile/config.py"

# open scratchpad files
alias scratchjs="vim ~/Documents/scratchpad/scratchpad.js"
alias notesmd="vim ~/Sync/gtd/notes.md"
alias quickmd="vim ~/Sync/org/quick-note.org"
alias tweetdeckmd="vim ~/Documents/scratchpad/tweetdeck.md"
alias inbox='vim ~/Sync/gtd/inbox.md'
alias na='vim ~/Sync/gtd/next-actions.md'

# common directories
alias cdt1='cd ~/Documents/code-immersives/term-1'
alias cdjp='cd ~/Documents/code-immersives/term-1/javascript-playground/'
alias cdsc='cd ~/Documents/scratchpad'
alias cdsn='cd ~/Documents/snippets'
alias cdsp='cd ~/Documents/side-projects'
alias cdtc='cd ~/Documents/side-projects/techniconsole'
alias cdgtd='cd ~/Sync/gtd'

# open emacs
# open file(s) in emacs
alias emacs='emacsclient --no-wait -c -a ""'
alias e='emacsclient --no-wait -c -a ""'
# open an emacs terminal in this location
alias th='emacsclient --no-wait -c -a "" -e "(+vterm/here 1)"&'

# open current directory in a stand-alone terminal
alias kh='kitty --detach&'

# open vim
alias v='vim'

# open vim fuzzy finder
alias vf='vim $(fzf)'

# open current directory with file explorer
alias fh='nautilus -w $PWD &'


# Add techniconsole
alias tca='npm init -y && sudo npm link technicolor'

# Laptop keyboard on/off
alias kboff="xinput disable \"AT Translated Set 2 keyboard\""
alias kbon="xinput enable \"AT Translated Set 2 keyboard\""

# file manipulation
mkcd ()
{
    mkdir -p -- "$1" &&
      cd -P -- "$1"
}

# Make each directory list contents upon entering.
# chpwd() ls -a


# # Show all color codes.

# ZSH_SPECTRUM_TEXT=${ZSH_SPECTRUM_TEXT:-Arma virumque cano Troiae qui primus ab oris}

# function spectrum_ls() {
#   for code in {000..255}; do
#     print -P -- "$code: %{$FG[$code]%}$ZSH_SPECTRUM_TEXT%{$reset_color%}"
#   done
# }

# apt/pkcon
alias sai='sudo apt install -y'
alias saa='sudo apt autoremove -y'
alias sar='sudo apt remove -y'
alias sauu='sudo apt update && sudo apt upgrade'
alias update='sudo pkcon refresh && sudo pkcon update'

# npm aliases
alias ni='npm install'
alias nid='npm install --save-dev'
alias ng='npm install --global'
alias nt='npm test'
alias nr='npm run'
alias ns='npm start'
alias nf='npm cache clean && rm -rf node_modules && npm install'
alias nlg='npm list --global --depth=0'

# git aliases I do/should use
alias g='git'
alias gn='rm -rf .git && git init'
alias gd='git difftool'
alias ga='git add'
alias gaa='git add --all'
alias gb='git branch'
alias gbd='git branch -d'
alias gc='git commit -v'
alias gcp='git commit -p'
alias gcamsg='git commit -am'
alias gcb='git checkout -b'
alias gcd='git checkout develop'
alias gco='git checkout'
alias gcm='git checkout master'
alias gd='git diff'
alias gf='git fetch'
alias ggpull='git pull origin $(git_current_branch)'
alias gl='git pull'
alias gm='git merge'
alias gpsup='git push --set-upstream origin $(git_current_branch)'
alias gp='git push'
alias gst='git status'

# git aliases I don't really use
alias gap='git apply'
alias gapa='git add --patch'
alias gau='git add --update'
alias gba='git branch -a'
alias gbda='git branch --no-color --merged | command grep -vE "^(\*|\s*(master|develop |dev)\s*$)" | command xargs -n 1 git branch -d'
alias gbl='git blame -b -w'
alias gbnm='git branch --no-merged'
alias gbr='git branch --remote'
alias gbs='git bisect'
alias gbsb='git bisect bad'
alias gbsg='git bisect good'
alias gbsr='git bisect reset'
alias gbss='git bisect start'
# 'gc!'='git commit -v --amend'
alias gca='git commit -v -a'
# 'gca!'='git commit -v -a --amend'
alias gcam='git commit -a -m'
# 'gcan!'='git commit -v -a --no-edit --amend'
# 'gcans!'='git commit -v -a -s --no-edit --amend'
alias gcf='git config --list'
alias gcl='git clone --recursive'
alias gclean='git clean -fd'
# 'gcn!'='git commit -v --no-edit --amend'
alias gcount='git shortlog -sn'
# alias gcp='git cherry-pick'
alias gcpa='git cherry-pick --abort'
alias gcpc='git cherry-pick --continue'
alias gcs='git commit -S'
alias gcsm='git commit -s -m'
alias gdca='git diff --cached'
alias gdct='git describe --tags `git rev-list --tags --max-count=1`'
alias gdcw='git diff --cached --word-diff'
alias gdt='git diff-tree --no-commit-id --name-only -r'
alias gdw='git diff --word-diff'
alias gfa='git fetch --all --prune'
alias gfo='git fetch origin'
alias gg='git gui citool'
alias gga='git gui citool --amend'
alias ggsup='git branch --set-upstream-to=origin/$(git_current_branch)'
alias ggpur=ggu
alias ggpush='git push origin $(git_current_branch)'
alias ghh='git help'
alias gignore='git update-index --assume-unchanged'
alias gignored='git ls-files -v | grep "^[[:lower:]]"'
# git-svn-dcommit-push='git svn dcommit && git push github master:svntrunk'
alias gk='\gitk --all --branches'
alias gke='\gitk --all $(git log -g --pretty=%h)'
alias glg='git log --stat'
alias glgg='git log --graph'
alias glgga='git log --graph --decorate --all'
alias glgm='git log --graph --max-count=10'
alias glgp='git log --stat -p'
alias glo='git log --oneline --decorate'
alias glog='git log --oneline --decorate --graph'
alias gloga='git log --oneline --decorate --graph --all'
alias ggsup='git branch --set-upstream-to=origin/$(git_current_branch)'
alias glol='git log --graph --pretty='\''%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgree n(%cr) %C(bold blue)<%an>%Creset'\'' --abbrev-commit'
alias glola='git log --graph --pretty='\''%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgre en(%cr) %C(bold blue)<%an>%Creset'\'' --abbrev-commit --all'
alias glp=_git_log_prettily
alias glum='git pull upstream master'
alias gma='git merge --abort'
alias gmom='git merge origin/master'
alias gmt='git mergetool --no-prompt'
alias gmtvim='git mergetool --no-prompt --tool=vimdiff'
alias gmum='git merge upstream/master'
alias gpd='git push --dry-run'
alias gpoat='git push origin --all && git push origin --tags'
alias gpristine='git reset --hard && git clean -dfx'
alias gpu='git push upstream'
alias gpv='git push -v'
alias gr='git remote'
alias gra='git remote add'
alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase -i'
alias grbm='git rebase master'
alias grbs='git rebase --skip'
alias grep='grep  --color=auto --exclude-dir={.bzr,CVS,.git,.hg,.svn}'
alias grh='git reset HEAD'
alias grhh='git reset HEAD --hard'
alias grmv='git remote rename'
alias grrm='git remote remove'
alias grset='git remote set-url'
alias grt='cd $(git rev-parse --show-toplevel || echo ".")'
alias gru='git reset --'
alias grup='git remote update'
alias grv='git remote -v'
alias gsb='git status -sb'
alias gsd='git svn dcommit'
alias gsi='git submodule init'
alias gsps='git show --pretty=short --show-signature'
alias gsr='git svn rebase'
alias gss='git status -s'
alias gsta='git stash save'
alias gstaa='git stash apply'
alias gstc='git stash clear'
alias gstd='git stash drop'
alias gstl='git stash list'
alias gstp='git stash pop'
alias gsts='git stash show --text'
alias gsu='git submodule update'
alias gts='git tag -s'
alias gtv='git tag | sort -V'
alias gunignore='git update-index --no-assume-unchanged'
alias gunwip='git log -n 1 | grep -q -c "\-\-wip\-\-" && git reset HEAD~1'
alias gup='git pull --rebase'
alias gupv='git pull --rebase -v'
alias gwch='git whatchanged -p --abbrev-commit --pretty=medium'
alias gwip='git add -A; git rm $(git ls-files --deleted) 2> /dev/null; git commit --no -verify -m "--wip-- [skip ci]"'


export ANDROID_HOME=$HOME/Android/Sdk
export PATH=$PATH:$ANDROID_HOME/tools
export PATH=$PATH:$ANDROID_HOME/tools/bin
export PATH=$PATH:$ANDROID_HOME/platform-tools

export PATH=$PATH:/usr/bin

export PATH=$PATH:/snap/bin

export BROWSER=firefox

export FZF_DEFAULT_COMMAND="fd --type file --color=always"
export FZF_DEFAULT_OPTS="--ansi"export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--ansi"

export N_PREFIX="$HOME/n"; [[ :$PATH: == *":$N_PREFIX/bin:"* ]] || PATH+=":$N_PREFIX/bin"  # Added by n-install (see http://git.io/n-install-repo).

export PATH=$PATH:~/.cargo/bin
export PATH=$PATH:~/Applications
export PATH=$PATH:~/.local/

export HOMEBREW_NO_ENV_FILTERING=1

[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

export PATH=/usr/local/anaconda/bin:/usr/local/bin:/home/abbreviatedman/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:/snap/bin:/home/abbreviatedman/Android/Sdk/tools:/home/abbreviatedman/Android/Sdk/tools/bin:/home/abbreviatedman/Android/Sdk/platform-tools:/usr/bin:/snap/bin:/home/abbreviatedman/.fzf/bin:/home/abbreviatedman/Android/Sdk/tools:/home/abbreviatedman/Android/Sdk/tools/bin:/home/abbreviatedman/Android/Sdk/platform-tools:/usr/bin:/snap/bin:/home/abbreviatedman/.emacs.d/bin:/home/abbreviatedman/.local/kitty.app/bin



# eval $(/home/linuxbrew/.linuxbrew/bin/brew shellenv)

# use asdf
# . $HOME/.asdf/asdf.sh

# To customize prompt, run `p10k configure` or edit ~/.p10k.zsh.
[[ ! -f ~/.p10k.zsh ]] || source ~/.p10k.zsh

export NVM_DIR="$([ -z "${XDG_CONFIG_HOME-}" ] && printf %s "${HOME}/.nvm" || printf %s "${XDG_CONFIG_HOME}/nvm")"
[ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh" # This loads nvm
