export PATH='/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'

null='/dev/null'
data_mount='/Volumes/Data'

default_venv='stuff'

mount 2> $null | grep $data_mount >& $null
if [ $? -eq 0 ]
then
    export WORKON_HOME='/Volumes/Data/stuff/python/venv'
    . /usr/local/bin/virtualenvwrapper.sh

    lsvirtualenv -b 2> $null | grep $default_venv >& $null
    if [ $? -eq 0 ]
    then
	workon $default_venv
    else
	echo "$default_venv venv not found, creating it..."
	mkvirtualenv $default_venv
    fi
else
    echo "$data_mount is not mounted..."
fi

function brew {
    deactivate
    /usr/local/bin/brew "$@"
}

alias clean="find . -type f -name '*~' -delete"
alias l='ls'
