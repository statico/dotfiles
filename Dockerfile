FROM alpine

RUN apk update && apk add git zsh bash vim exa ripgrep fzf
RUN adduser --system --shell /bin/zsh demouser

WORKDIR /home/demouser
USER demouser
ADD --chown=demouser ./ .dotfiles/
RUN .dotfiles/install.zsh

CMD zsh -l
