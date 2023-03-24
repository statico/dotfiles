FROM alpine

RUN apk update && apk add git zsh bash vim exa ripgrep fzf tzdata
RUN addgroup demouser \
      && adduser --system --shell /bin/zsh -g demouser demouser \
      && cp /usr/share/zoneinfo/UTC /etc/localtime

WORKDIR /home/demouser
USER demouser
ADD --chown=demouser ./ .dotfiles/
RUN .dotfiles/install.zsh

RUN echo 'echo "Welcome to Ian’s dotfiles. This host has been given a random prompt color in .zshlocal. Further host-specific customizations would go in there. Read more: https://statico.link/dotfiles"' >>.zshlocal

CMD zsh -l
