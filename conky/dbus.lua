local lgi = require 'lgi'
local Gio = lgi.require 'Gio'
local core = require 'lgi.core'
local GLib = lgi.require 'GLib'
local Variant = GLib.Variant
local bus = Gio.bus_get_sync(Gio.BusType.SESSION)

local last_value = ''

--Create a user callback function that needs to operate when signal is received
function onDBusSignalCallback(conn, sender, object_path, interface_name, signal_name, user_data)
    local str = string.format("SIGNAL - object_path:%s, interface_name:%s, signal_name:%s", object_path, interface_name, signal_name)
    --print(Variant.get_type_string(user_data))
    print(user_data.data)
    last_value = user_data.data
end

function conky_setup()
    print('>>> Setup')

    local sub_id = bus:signal_subscribe(
        'org.xmonad.Log',
        'org.xmonad.Log',
        'Update',
        nil,
        nil,
        Gio.DBusSignalFlags.NONE,
        onDBusSignalCallback)
    if sub_id then
            print("Subscription id", sub_id)
    end
end


conky_setup()

function conky_get_msg()
    print(last_value)
    return last_value
end

--local main_loop = GLib.MainLoop()
--main_loop:run()