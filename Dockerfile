FROM alpine

RUN apk update && apk add git zsh bash neovim exa ripgrep fzf tzdata ncurses nodejs
RUN addgroup demo \
      && adduser --system --shell /bin/zsh -g demo demo \
      && cp /usr/share/zoneinfo/UTC /etc/localtime

WORKDIR /home/demo
USER demo
ADD --chown=demo ./ .dotfiles/
RUN .dotfiles/install.zsh

RUN { echo 'cat .dotfiles/welcome.txt' >>.zshlocal; }

CMD ["zsh", "-l"]
