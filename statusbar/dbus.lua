local lgi = require 'lgi'
local Gio = lgi.require 'Gio'
local GLib = lgi.require 'GLib'
local Variant = GLib.Variant

local bus = Gio.bus_get_sync(Gio.BusType.SESSION)

function onDBusSignal(connection, sender, objectPath, interfaceName, signalName, userData)
    --print(Variant.get_type_string(userData))
    print(userData.data)
end

local subscription_id = bus:signal_subscribe(
    'org.xmonad.Log',
    'org.xmonad.Log',
    'Update',
    nil,
    nil,
    Gio.DBusSignalFlags.NONE,
    onDBusSignal)
if subscription_id then
    print("Subscribed to DBUS")
end

--ctx = GLib.MainLoop():get_context()
-- Run a single blocking iteration
--if ctx:iteration(true) == true then
--  print("something changed!")
--end

local main_loop = GLib.MainLoop()
main_loop:run()