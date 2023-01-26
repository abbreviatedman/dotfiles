# SSH
# linux keychain
# eval `keychain -q --eval id_ed25519`

# open vim
alias v='nvim'
alias vi='nvim'
alias vim='nvim'


# If you come from bash you might have to change your $PATH.
# Path to your oh-my-zsh installation.
export ZSH=~/.oh-my-zsh

export PATH=$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:/usr/local/bin:/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:~/Applications:~/.cargo/bin/snap/bin:/Android/Sdk/tools:/Android/Sdk/tools/bin:/Android/Sdk/platform-tools:/usr/bin:/snap/bin:/.fzf/bin:/Android/Sdk/tools:/Android/Sdk/tools/bin:/Android/Sdk/platform-tools:/usr/bin:/snap/bin:~/.emacs.d/bin:/.local/kitty.app/bin:~/.local:$HOME/go/bin:$HOME/.local/bin:$HOME/.npm-global:/opt/anaconda/condabin

VIM_MODE_VICMD_KEY='^Q'

# Set name of the theme to load. Optionally, if you set this to "random"
# it'll load a random theme each time that oh-my-zsh is loaded.
# See https://github.com/robbyrussell/oh-my-zsh/wiki/Themes
ZSH_THEME="dangerroom"

export LS_COLORS='rs=0:di=01;34:ln=01;34:pi=40;33:so=01;35:do=01;35:bd=40;33;01:cd=40;33;01:or=01;05;37;41:mi=01;05;37;41:su=37;41:sg=30;43:ca=30;41:tw=01;34:ow=01;34:st=01;34:ex=00:*.tar=01;31:*.tgz=01;31:*.arc=01;31:*.arj=01;31:*.taz=01;31:*.lha=01;31:*.lz4=01;31:*.lzh=01;31:*.lzma=01;31:*.tlz=01;31:*.txz=01;31:*.tzo=01;31:*.t7z=01;31:*.zip=01;31:*.z=01;31:*.Z=01;31:*.dz=01;31:*.gz=01;31:*.lrz=01;31:*.lz=01;31:*.lzo=01;31:*.xz=01;31:*.bz2=01;31:*.bz=01;31:*.tbz=01;31:*.tbz2=01;31:*.tz=01;31:*.deb=01;31:*.rpm=01;31:*.jar=01;31:*.war=01;31:*.ear=01;31:*.sar=01;31:*.rar=01;31:*.alz=01;31:*.ace=01;31:*.zoo=01;31:*.cpio=01;31:*.7z=01;31:*.rz=01;31:*.cab=01;31:*.jpg=01;35:*.jpeg=01;35:*.gif=01;35:*.bmp=01;35:*.pbm=01;35:*.pgm=01;35:*.ppm=01;35:*.tga=01;35:*.xbm=01;35:*.xpm=01;35:*.tif=01;35:*.tiff=01;35:*.png=01;34:*.svg=01;35:*.svgz=01;35:*.mng=01;35:*.pcx=01;35:*.mov=01;35:*.mpg=01;35:*.mpeg=01;35:*.m2v=01;35:*.mkv=01;35:*.webm=01;35:*.ogm=01;35:*.mp4=01;35:*.m4v=01;35:*.mp4v=01;35:*.vob=01;35:*.qt=01;35:*.nuv=01;35:*.wmv=01;35:*.asf=01;35:*.rm=01;35:*.rmvb=01;35:*.flc=01;35:*.avi=01;35:*.fli=01;35:*.flv=01;35:*.gl=01;35:*.dl=01;35:*.xcf=01;35:*.xwd=01;35:*.yuv=01;35:*.cgm=01;35:*.emf=01;35:*.axv=01;35:*.anx=01;35:*.ogv=01;35:*.ogx=01;35:*.pdf=00;32:*.ps=00;32:*.txt=00;32:*.patch=00;32:*.diff=00;32:*.log=00;32:*.tex=00;32:*.doc=00;32:*.aac=00;36:*.au=00;36:*.flac=00;36:*.m4a=00;36:*.mid=00;36:*.midi=00;36:*.mka=00;36:*.mp3=00;36:*.mpc=00;36:*.ogg=00;36:*.ra=00;36:*.wav=00;36:*.axa=00;36:*.oga=00;36:*.spx=00;36:*.xspf=00;36:';
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
  z
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

# You may need to manually set your language environment
# export LANG=en_US.UTF-8

export EDITOR='neovim'
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=$EDITOR

# Compilation flags
# export ARCHFLAGS="-arch x86_64"

# Aliases!

# doom emacs
alias ds='doom sync'
alias dmd='doom doctor'

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
alias scratchjs="nvim ~/Documents/scratchpad/scratchpad.js"
alias notesmd="nvim ~/Sync/gtd/notes.md"
alias quickmd="nvim ~/Sync/org/quick-note.org"
alias tweetdeckmd="nvim ~/Documents/scratchpad/tweetdeck.md"
alias inbox='nvim ~/Sync/gtd/inbox.md'
alias na='nvim ~/Sync/gtd/next-actions.md'

# common directories
alias cdga='cd ~/Documents/ga'
alias cdt1='cd ~/Documents/code-immersives/term-1'
alias cdjp='cd ~/Documents/javascript-playground/'
alias cdsc='cd ~/Documents/scratchpad'
alias cdsp='cd ~/Documents/side-projects'
alias cdtc='cd ~/Documents/side-projects/techniconsole'
alias cdgtd='cd ~/Sync/gtd'

# database management
alias smg='sequelize model:generate'

# open emacs
# open file(s) in emacs
# alias emacs='emacsclient --no-wait -c -a ""'

alias e='emacsclient --no-wait -c -a ""'
# open an emacs terminal in this location
alias th='emacsclient --no-wait -c -a "" -e "(+vterm/here 1)"&'

# open current directory in a stand-alone terminal
alias kh='kitty --detach&'

# open vim
# alias vim='nvim'
alias v='nvim'
alias ovim='\vim'

# open vim fuzzy finder
alias vf='nvim $(fzf)'

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

# apt/pkcon
alias sai='sudo apt install -y'
alias saa='sudo apt autoremove -y'
alias sar='sudo apt remove -y'
alias sauu='sudo apt update && sudo apt upgrade'
alias update='sudo pkcon refresh && sudo pkcon update'

# React
alias cra='create-react-app'

# npm aliases
alias ni='npm install'
alias niy='npm init -y'
alias nid='npm install --save-dev'
alias ng='npm install --global'
alias nt='npm test'
alias nr='npm run'
alias ns='npm start'
alias nf='npm cache clean && rm -rf node_modules && npm install'
alias nlg='npm list --global --depth=0'

# nodemon
alias nm='nodemon'

# hub
alias hcl='hub clone'
alias hcr='hub create'

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
alias gra='git remote add origin'
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


# ssh
eval `keychain --eval --agents ssh id_ed25519`
[ "$TERM" = "xterm-kitty" ] && alias ssh="kitty +kitten ssh"

export FZF_DEFAULT_COMMAND="fd --type file --color=always"
export FZF_DEFAULT_OPTS="--ansi"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export FZF_DEFAULT_OPTS="--ansi"


[ -f ~/.fzf.zsh ] && source ~/.fzf.zsh

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/opt/anaconda/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/opt/anaconda/etc/profile.d/conda.sh" ]; then
        . "/opt/anaconda/etc/profile.d/conda.sh"
    else
        export PATH="/opt/anaconda/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

