import weechat
import os

weechat.register("notify3", "aiya000", "0.1", "MIT", "notify-send with python3", "", "")

def on_privmsg_catch(data, signal, signal_data):
    channel_name = signal.split(",")[0]
    room_name    = signal_data.split(" ")[2]
    user_name    = weechat.info_get("irc_nick_from_host", signal_data)
    message      = signal_data.split(":")[-1]
    notify_message(channel_name, room_name, user_name, message)
    return weechat.WEECHAT_RC_OK

def notify_message(channel_name, room_name, user_name, message):
    os.system("notify-send '%s(%s)' '%s: %s'" % (channel_name, room_name, user_name, message))

weechat.hook_signal("*,irc_in2_privmsg", "on_privmsg_catch", "")
