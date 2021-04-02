# Manjaro

- install Manjaro
- restart and update with `sudo pacman -Syyu` (this takes a while)

# Get Better Basic Tools

- `yay -S firefox kitty`
- install firefox extensions:
  - bitwarden
    - log in
    - make it dark mode
    - set it to auto-fill
    - change generator to passphrase with caps and a number
  - vimium-c
    - bind moveWindowToNewTab to W
    - pages may not steal focus
  - Dark Reader
  - set all extensions to work in private/incognito windows
- preferences/customize firefox

# Improve The Terminal

- get zsh (`yay -S zsh`) (Manjaro might already come with it)
- install oh-my-zsh (https://ohmyz.sh/)
- get dangerroom on the vim-indicator branch (https://github.com/abbreviatedman/dangerroom/blob/vim-indicator/dangerroom.zsh-theme)
- enter "dangerroom" into the .zshrc for the variable `plugins`
- enter `bindkey -v` somewhere in `.zshrc`
- run `source ~/.zshrc`
- `sudo visudo`

# Install Apps From Repos

This takes a while, but can be done in one line that ALSO includes all the items from the next section.

`yay -S`:

- google-chrome
- gvim (for clipboard-enabled vim)
- pulse-sms
- slack-desktop
- emacs
- pavucontrol
- zoom
- signal-desktop
- visual-studio-code-bin
- chromium
- xclip
- ttf-fira-code

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
- pandoc

# Github Integration

- generate ssh key and add it to the agent (https://docs.github.com/en/github-ae@latest/github/authenticating-to-github/generating-a-new-ssh-key-and-adding-it-to-the-ssh-agent)
- add the key to the GitHub account (https://docs.github.com/en/github-ae@latest/articles/adding-a-new-ssh-key-to-your-github-account)
- add a personal access token with `gist`, `repo`, and `workflow` access (https://github.com/settings/tokens)
- copy the token to the clipboard
- use `hub` to clone something (maybe dangerroom)
- enter the access token as the password and say yes to adding it permanently

# Add Global NPM Packages

- `npm i -g prettier`
- optional (if in a hurry, takes a while and you can do it later): `npm i -g jest surge typescript nodemon live-server`

# Install Doom Emacs

- `git clone https://github.com/hlissner/doom-emacs ~/.emacs.d`
- `~/.emacs.d/bin/doom install` (this takes a while)
- `~/.emacs.d/bin/doom sync`
- `~/.emacs.d/bin/doom doctor`

# Clone Down And Activate Dotfiles

- `yadm clone git@github.com:abbreviatedman/dotfiles`
- `source ~/.zshrc`
- `doom sync && doom doctor` (this can take a WHILE)
- launch `vim` and run a `:PlugInstall`
- check `.config/i3` for anything irrelevant and then reload i3 (or log out and back in)

# Add Node

- install `asdf` using `git` (https://asdf-vm.com/#/core-manage-asdf)
- add the `nodejs` plugin with `asdf plugin add nodejs`
- install gpg keyring for asdf-nodejs (BEFORE adding nodejs plugin?) with `bash -c '${ASDF_DATA_DIR:=$HOME/.asdf}/plugins/nodejs/bin/import-release-team-keyring'`
- install the latest version with `asdf install nodejs latest`
- set it as the global version with `asdf global nodejs [the version number installed with previous step]`

# Extras

- highlight-pointer https://github.com/swillner/highlight-pointer
