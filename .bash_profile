export PATH='/usr/local/bin:/usr/bin:/bin:/usr/sbin:/sbin'

null='/dev/null'
data_mount='/Volumes/Data'
mount 2> $null | grep $data_mount >& $null
if [ $? -eq 0 ]
then
    export WORKON_HOME='/Volumes/Data/stuff/python/venv'
    . /usr/local/bin/virtualenvwrapper.sh

    default_venv='stuff'
    lsvirtualenv -b 2> $null | grep $default_venv >& $null
    if [ $? -eq 0 ]
    then
	workon stuff
    else
	echo "$default_venv venv not found, creating it..."
	mkvirtualenv stuff
    fi
else
    echo "$data_mount is not mounted..."
fi

alias clean="find . -type f -name '*~' -delete"
alias l='ls'
