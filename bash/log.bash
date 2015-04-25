#!/usr/bin/env bash

# a simple function that log to stdout the caller name and all its parameters
function log {
    local function_name='cli'
    if [ -n "${FUNCNAME[1]}" ]; then
        function_name=${FUNCNAME[1]}
    fi
    function_name="$function_name()"

    if [ $# -eq 0 ]; then
        echo "$function_name: called ${FUNCNAME[0]} without any message"
    else
        echo "$function_name: $@"
    fi
}
