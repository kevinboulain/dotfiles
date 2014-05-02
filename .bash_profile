export PATH='/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'

null='/dev/null'
data_mount='/Volumes/Data'

function use_default_venv {
    # creates and uses $default_venv
    default_venv='stuff'

    # test if $data_mount if correctly mounted
    mount 2> $null | grep "$data_mount (hfs, local, journaled)" >& $null
    if [ $? -eq 0 ]
    then
	export WORKON_HOME='/Volumes/Data/stuff/python/venv'
	. /usr/local/bin/virtualenvwrapper.sh

	# test if $default_venv exists
	lsvirtualenv -b 2> $null | grep $default_venv >& $null
	if [ $? -eq 0 ]
	then
	    # use it
	    workon $default_venv
	else
	    # create it
	    echo "$default_venv venv not found, creating it..."
	    mkvirtualenv $default_venv
	fi
    else
	echo "$data_mount is not mounted..."
    fi
}

function brew {
    # brew can not work in a venv
    deactivate
    /usr/local/bin/brew "$@"
}

use_default_venv

alias clean="find . -type f -name '*~' -delete"
alias l='ls'
