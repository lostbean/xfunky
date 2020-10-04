chmod +x config/plasma-workspace/env/set_window_manager.sh
mkdir -p $HOME/.config/plasma-workspace/env/
ln -i -s $(pwd)/config/plasma-workspace/env/set_window_manager.sh $HOME/.config/plasma-workspace/env/set_window_manager.sh

cp -u fonts/Monofur/*.ttf $HOME/.local/share/fonts

mkdir -p $HOME/.xfunky
mkdir -p $HOME/.xfunky/statusbar
ln -i -s $(pwd)/statusbar/* $HOME/.xfunky/statusbar

cd xmonad
stack install