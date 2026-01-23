FROM alpine

RUN apk update && apk add git zsh bash neovim exa ripgrep fzf tzdata ncurses nodejs npm python3 py3-pip curl
RUN addgroup demo \
      && adduser --system --shell /bin/zsh -g demo demo \
      && cp /usr/share/zoneinfo/UTC /etc/localtime

WORKDIR /home/demo
USER demo
ADD --chown=demo ./ .dotfiles/
RUN .dotfiles/install.zsh

# Fix an error with lua-language-server and Alpine Linux
# https://github.com/mason-org/mason.nvim/discussions/1406
RUN nvim --headless -c "MasonInstall --target=linux_arm64_gnu lua-language-server" -c "qa" || true

RUN { echo 'cat .dotfiles/welcome.txt' >>.zshlocal; }

CMD ["zsh", "-l"]
