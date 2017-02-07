import weechat
import os

weechat.register("notify3", "aiya000", "0.1", "MIT", "notify-send with python3", "", "")

# The abstract of notify-send command
def notify_send(title, message):
    os.system("notify-send '%s' '%s'" % (title, message))


# The event hook when privmsg caught
def on_privmsg_catch(data, signal, signal_data):
    show_privmsg( channel_name = signal.split(",")[0]
                , room_name    = signal_data.split(" ")[2]
                , user_name    = weechat.info_get("irc_nick_from_host", signal_data)
                , message      = signal_data.split(":")[-1]
                )
    return weechat.WEECHAT_RC_OK

# Show privmsg to the display
def show_privmsg(channel_name, room_name, user_name, message):
    notify_send( title   = "%s(%s)" % (channel_name, room_name)
               , message = "%s: %s" % (user_name, message)
               )


weechat.hook_signal("*,irc_in2_privmsg", "on_privmsg_catch", "")
