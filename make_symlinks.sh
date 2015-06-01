#!/bin/bash


dir=~/dotfiles
olddir=~/old_dotfiles
files="bashrc vimrc Xdefaults conkyrc"
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
  echo "Backing up $file"
  mv ~/$file ~/old_dotfiles/
  echo "Creating symlink to $file"
  ln -s $dir/$file ~/$file
done

echo "All done!"
echo
echo "Remember to install bash-completion and git-prompt if not already installed"
echo "Git-propmt can be found here: https://raw.githubusercontent.com/git/git/master/contrib/completion/git-prompt.sh"

