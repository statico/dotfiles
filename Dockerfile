FROM alpine
RUN apk update && apk add git zsh
ADD ./ /dotfiles
RUN zsh </dotfiles/install.zsh
CMD zsh -l
