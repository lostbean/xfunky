chmod +x config/plasma-workspace/env/set_window_manager.sh
mkdir -p $HOME/.config/plasma-workspace/env/
ln -i -s $(pwd)/config/plasma-workspace/env/set_window_manager.sh $HOME/.config/plasma-workspace/env/set_window_manager.sh

mkdir -p $HOME/.xfunky
ln -i -s $(pwd)/statusbar $HOME/.xfunky/statusbar

cd xmonad
stack install