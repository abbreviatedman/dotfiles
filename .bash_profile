#
# ~/.bash_profile
#
export PATH=$ANDROID_HOME/tools:$ANDROID_HOME/tools/bin:$ANDROID_HOME/platform-tools:/usr/local/bin:/.local/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/games:/usr/local/games:~/Applications:~/.cargo/bin/snap/bin:/Android/Sdk/tools:/Android/Sdk/tools/bin:/Android/Sdk/platform-tools:/usr/bin:/snap/bin:/.fzf/bin:/Android/Sdk/tools:/Android/Sdk/tools/bin:/Android/Sdk/platform-tools:/usr/bin:/snap/bin:~/.emacs.d/bin:/.local/kitty.app/bin:~/.local:$HOME/go/bin:$HOME/.local/bin

[[ -f ~/.bashrc ]] && . ~/.bashrc
export PYENV_ROOT="$HOME/.pyenv"
command -v pyenv >/dev/null || export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
