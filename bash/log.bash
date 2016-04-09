# a simple function that log to stdout the caller name and all its parameters
function log {
    local -r function_name=${FUNCNAME[1]:-cli}

    if [ $# -eq 0 ]; then
        echo "$function_name: called ${FUNCNAME[0]} without any message"
    else
        echo "$function_name: $@"
    fi
}
