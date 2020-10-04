chmod +x config/plasma-workspace/env/set_window_manager.sh
mkdir -p $HOME/.config/plasma-workspace/env/
cp -u $(pwd)/config/plasma-workspace/env/set_window_manager.sh $HOME/.config/plasma-workspace/env/set_window_manager.sh

cp -u fonts/Monofur/*.ttf $HOME/.local/share/fonts

mkdir -p $HOME/.xfunky
cp -u $(pwd)/xfunky/* $HOME/.xfunky

mkdir -p $HOME/.xfunky/statusbar
cp -u $(pwd)/statusbar/* $HOME/.xfunky/statusbar

cd xmonad
stack install