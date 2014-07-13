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
	export WORKON_HOME="$data_mount/stuff/python/venv"
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
	echo "$data_mount is not properly mounted..."
	echo "Not switching to $default_venv venv..."
    fi
}

function brew {
    # brew can not work in a venv
    if [ -n "$VIRTUAL_ENV" ]
    then
	# deactivate only if working on a venv
	echo 'Using brew within a venv, deactivating it...'
	deactivate
    fi
    /usr/local/bin/brew "$@"
}

function finder_show_all {
    if [ "$1" == 'true' ] || [ "$1" == 'false' ]
    then
	/usr/bin/defaults write com.apple.finder AppleShowAllFiles $1
	/usr/bin/killall Finder
    else
	echo 'Boolean parameter required...'
    fi
}

use_default_venv

#alias clean="find . -type f -name '*~' -delete"
alias clean="find . -name '*.pyc' -delete -or -name '*~' -delete -or -name '*.o' -delete"
alias clean_list="find . -name '*.pyc' -or -name '*~' -or -name '*.o'"
alias l='ls'
