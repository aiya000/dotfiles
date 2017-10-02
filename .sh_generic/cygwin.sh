#!/bin/sh
alias pbcopy='tee /dev/clipboard > /dev/null'
alias traceroute='cocot tracert'
alias xdg-open=explorer
alias_of cygrunsrv='cocot cygrunsrv'
alias_of csc='cocot csc'
alias_of ifconfig='cocot ipconfig'
alias_of ping='cocot ping'
alias_of route='cocot route'
alias_of netstat='cocot netstat'
alias_of nslookup='cocot nslookup'
alias_of updatedb='updatedb --localpaths="/bin /dev /etc /home /lib /usr /var /opt" --prunepaths="/usr/tmp /var/tmp"'
alias_of mysql='mysql --pager="less -r -S -n -i -F -X" --protocol=TCP'
alias_of ps='ps -W'

# Emulate sudo (See http://qiita.com/AinoMegumi/items/fd56711fe1fd2a0e1bbf)
function sudo () {
    which cocot > /dev/null 2>&1
    if [ "$?" -eq 0 ] ; then
        cygwin_sudo_cocot=cocot
    else
        cygwin_sudo_cocot=''
    fi
    $cygwin_sudo_cocot powershell Start-Process "$1" -Verb runas
}
