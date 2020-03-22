chmod +x config/plasma-workspace/env/set_window_manager.sh
mkdir -p $HOME/.config/plasma-workspace/env/
ln -i -s config/plasma-workspace/env/set_window_manager.sh $HOME/.config/plasma-workspace/env/set_window_manager.sh

mkdir -p $HOME/.conky
ln -i -s conky/conky.conf $HOME/.conky/conky.conf

cd xmonad
stack install