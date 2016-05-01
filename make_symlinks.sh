#!/bin/bash


dir=~/dotfiles
olddir=~/old_dotfiles
files="bashrc vimrc Xdefaults Xresources conkyrc zshrc"
xmonad_files=".xmonad/xmonad.hs .xmonad/conky.rc_xmonad"

echo "Creating backup folder for existing dotfiles in ~"

mkdir -p $olddir
echo "...done"

# change to dotfiles directory
cd $dir

for file in $files; do
  echo "Backing up $file"
  mv ~/.$file ~/old_dotfiles/
  echo "Creating symlink to $file"
  ln -s $dir/$file ~/.$file
done

for file in $xmonad_files; do

if [ ! -d "~/.xmonad" ]; then
  echo "Creating xmonad-folder"
  mkdir ~/.xmonad
fi

  echo "Backing up $file"
  mv ~/$file ~/old_dotfiles/
  echo "Creating symlink to $file"
  ln -s $dir/$file ~/$file
done

echo "All done!"
echo
echo "Remember to install bash-completion and git-prompt if not already installed"
echo "Git-propmt can be found here: https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh"
echo "If xmonad refuses to recompile, try to install libghc-xmonad-contrib-dev"
echo "And you'll need to install urxvt-perls as well. (https://github.com/muennich/urxvt-perls/)"

