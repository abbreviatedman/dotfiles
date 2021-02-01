# Manjaro

- install Manjaro
- restart and update (`sudo pacman -Syyu`) (this takes a while)

# Improve The Terminal

- get zsh (`yay -S zsh`) (Manjaro might already come with it)
- install oh-my-zsh (https://ohmyz.sh/)
- get dangerroom from the repo (http://www.github.com/abbreviatedman/dangerroom)
- enter "dangerroom" into the .zshrc for the variable `plugins`
- enter `bindkey -v` somewhere in `.zshrc`
- run `source ~/.zshrc`

# Install Apps From Repos

This takes a while, but can be done in one line that also includes all the items from the next section.

`yay -S`:

- firefox
- google-chrome
- pulse-sms
- slack-desktop
- emacs
- pavucontrol
- zoom
- signal-desktop
- code

# Install CLI Tools From Repos

`yay -S`:

- hub
- curl
- yadm
- pulseaudio
- keychain
- ripgrep
- fd
- aspell
- aspell-en
- cmake

# Github Integration

- generate ssh key and add it to the agent (https://docs.github.com/en/github-ae@latest/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
- add the key to the GitHub account (https://docs.github.com/en/github-ae@latest/articles/adding-a-new-ssh-key-to-your-github-account)
- add a personal access token with `gist`, `repo`, and `workflow` access (https://github.com/settings/tokens)
- copy the token to the clipboard
- use `hub` to clone something (maybe dangerroom)
- enter the access token as the password and say yes to adding it permanently

# Add Node

- install `asdf` using `git` (https://asdf-vm.com/#/core-manage-asdf)
- add the `nodejs` plugin with `asdf plugin add nodejs`
- install the latest version with `asdf install nodejs latest`
- set it as the global version with `asdf global nodejs [the version number installed with previous step]`

# Add Global NPM Packages

- `npm i -g prettier` (this can take a while)
- optional (if in a hurry, takes a while and you can do it later): `npm i -g jest surge typescript nodemon`

# Install Doom Emacs

- `git clone https://github.com/hlissner/doom-emacs ~/.emacs.d`
- `~/.emacs.d/bin/doom install` (this takes a while)
- `~/.emacs.d/bin/doom sync`
- `~/.emacs.d/bin/doom doctor`

# Clone Down And Activate Dotfiles

- `yadm clone https://www.github.com/abbreviatedman/dotfiles`
- `source ~/.zshrc`
- `doom sync && doom doctor` (this can take a WHILE)
- launch `vim` and run a `:PlugInstall`
