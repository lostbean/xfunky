
os.execute("sleep 1")

conky.config = {
      background = false
    , out_to_console = true
    , out_to_x = false
    , update_interval = 1.0
    , total_run_times = 0
    , use_spacer = none
    , lua_load = 'utils.lua'
}

font = "^fn(Monofur Nerd Font:size=12)"

basic = [[${time %a %b %d %I:%M%P}]]
cores = [[${loadavg 1}  ${freq_g}GHz ${lua cpuvbar cpu1}^p(5)${lua cpuvbar cpu2}^p(5)${lua cpuvbar cpu3}^p(5)${lua cpuvbar cpu4}]]
men   = [[  ${lua h100bar memperc} 﫭 ${lua h100bar swapperc}]]
disks = [[File system / ${fs_bar /}/home ${fs_bar /home}/data ${fs_bar /data}]]
batt0 = [[${if_existing /sys/class/power_supply/BAT0/uevent}${battery BAT0}${endif}]]
wifi    = [[${if_existing /proc/net/route wlp3s0} ^p(5)${wireless_essid wlp3s0}${endif}]]
wifiBar = [[${if_existing /proc/net/route wlp3s0} ^p(4)${lua wirelessQualityBar wlp3s0}^p(5)${wireless_essid wlp3s0}${endif}]]
wifiIO  = [[${if_existing /proc/net/route wlp3s0}total: ↑${totalup wlp3s0} ↓${totaldown wlp3s0} speed: ↑${upspeedf wlp3s0}KiB ↓${downspeedf wlp3s0}KiB ${endif}]]

--conky.text = cpu .. cores .. men .. disks .. batt0 .. batt1 .. wifi
conky.text = font .. cores .. "    " .. men .. " " .. wifi .. " | " .. batt0 .. ' | ' .. basic