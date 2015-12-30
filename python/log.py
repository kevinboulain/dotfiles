import functools

import logging
logger = logging.getLogger(__name__)

stringify = str

def set_root(level='warning'):
    """
    Set the minimum level for root logger

    :param str level: the minimum level to process
    """
    # the log format
    logging.basicConfig(
        format='[%(asctime)s][%(levelname)s] %(message)s',
        datefmt='%H:%M:%S'
    )
    # get the root logger
    root_logger = logging.getLogger()
    # set the minimum level for the root logger
    root_logger.setLevel(getattr(logging, level.upper()))


def log_function_call(level, function, *args, **kwargs):
    """
    A closure that handle the formatting and the logging

    :param str level: a specific log level for this log call
    :param function function: the function that will be called
    :param list *args: the unnamed function parameters
    :param list **kwargs: the named function parameters
    :rtype: dict
    """
    # handle the log level tests here, before any processing
    log_level = getattr(logging, level.upper())

    # avoid useless processing of parameters
    # by only treating messages that will be processed by the logger
    if not logger.isEnabledFor(log_level): return lambda **kwargs: None

    # the log function that will be used
    log_function = getattr(logger, level.lower())

    # the arguments of the function call
    arguments = ', '.join([stringify(arg) for arg in args])
    keywords_arguments = ', '.join([
        '%s=%s' % (key, stringify(value))
        for key, value in kwargs.iteritems() # iteritems: python2
    ])

    # the call informations
    call_informations = {
        'call': call.func_name, # the call function name
        'module': function.__module__, # the module name
        'function': function.func_name, # the function called
        'parameters': ', '.join( # the call parameters
            [arguments] if arguments else [] +
            [keywords_arguments] if keywords_arguments else []),
    }

    def log_formatted(**contents):
        """
        The formatting and logging function

        :param dict **contents: the format contents
        """
        log_message = (
            '{call}: {module}.{function}({parameters}) {what} {value}'
        ).format(**contents)
        return log_function(log_message.rstrip())
    return lambda what='', value='': log_formatted(
        what=stringify(what),
        value=stringify(value),
        **call_informations
    )

def call(function=None, level='debug'):
    '''
    Decorator to debug functions calls.

    :param function function: the decorated function
    :param str level: a specific log level for this log call
    :rtype: function
    '''
    # if there is no function, it has been called with arguments
    # returns the decorator with the optionals arguments for the next call
    if function is None: return functools.partial(call, level=level)

    # frunctools.wraps allow to not loose the function informations
    @functools.wraps(function)
    def log_call(*args, **kwargs):
        """
        The inner function of the call decorator

        :param list *args: the unnamed function parameters
        :param list **kwargs: the named function parameters
        """
        # create a closure that will store every call related variables
        log_function = log_function_call(level, function, *args, **kwargs)

        # log that the function has been called
        log_function()

        # do the call
        try: value = function(*args, **kwargs)
        except Exception as exception:
            # log that the function has raised an exception
            log_function(what='raise', value=exception)
            raise

        # log that the function has returned
        log_function(what='return', value=value)
        return value
    return log_call
