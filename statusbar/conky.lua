
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


basic = [[${time %a %b %d %I:%M%P}   load ${loadavg 1}   temp ${acpitemp} deg]]
cores = [[cpu@${freq_g}GHz ${lua cpuvbar cpu1} ${lua cpuvbar cpu2} ${lua cpuvbar cpu3} ${lua cpuvbar cpu4}]]
men   = [[memory ${lua h100bar memperc} | swap ${lua h100bar swapperc}]]
disks = [[File system / ${fs_bar /}/home ${fs_bar /home}/data ${fs_bar /data}]]
batt0 = [[${if_existing /sys/class/power_supply/BAT0/uevent}Battery ${battery BAT0}${endif}]]
batt1 = [[${if_existing /sys/class/power_supply/BAT1/uevent}Battery ${battery BAT1}${endif}]]
wifi  = [[${if_existing /proc/net/route wlp3s0}Wireless ${wireless_essid wlp3s0} ${wireless_link_bar 10,10 wlp3s0} total: ↑${totalup wlp3s0} ↓${totaldown wlp3s0} speed: ↑${upspeedf wlp3s0}KiB ↓${downspeedf wlp3s0}KiB ${endif}]]

--conky.text = cpu .. cores .. men .. disks .. batt0 .. batt1 .. wifi
conky.text = cores .. "    " .. men .. "  " .. basic