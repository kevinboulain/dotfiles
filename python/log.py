import types
import functools
import logging

logger = logging.getLogger(__name__)

def set_root(level):
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

def stringify(argument):
    """
    Return a 'stringified' representation of the passed argument

    :param str argument: an object to be stingified
    :rtype: str
    """
    # do not call repr() on classes
    # this types can be stringified without any further processing
    if isinstance(argument, (
            types.NoneType, types.BooleanType,
            types.IntType, types.LongType, types.FloatType, types.ComplexType,
            types.StringTypes,
            #types.TupleType, types.ListType, types.DictType,
            )):
        return str(argument)
    # function
    elif isinstance(argument, (types.FunctionType, types.LambdaType,)):
        return str(argument.func_name)
    # list kind tuple
    elif isinstance(argument, (types.TupleType, types.ListType,)):
        return str('[%s]' % ', '.join([
            stringify(arg) for arg in argument
        ]))
    # dict type
    elif isinstance(argument, (types.DictType,)):
        return str('{%s}' % ', '.join([
            '%s: %s' % (stringify(key), stringify(value))
            for key, value in argument.iteritems() # iteritems: python2
        ]))
    # handle everything else, treated as an object
    else:
        # we have a new-style class
        if isinstance(argument, type):
            return str('%s new-style class' % argument.__name__)
        # we have an old style class
        if not hasattr(argument, '__class__'):
            return str('%s old-style class' % argument.__name__)
        # we have an instance
        if isinstance(argument, argument.__class__):
            return str('%s instance' % argument.__class__.__name__)
    # should never get here
    assert 0, "can't stringify %s" % str(argument)

def call(function=None, level='debug'):
    '''
    Decorator to debug functions calls.
    May decrease performance!

    :param str level: a specific log level for this log call
    '''
    # if there is no function, it has been called with arguments
    # returns the decorator with the optionals arguments for the next call
    if function is None:
        return functools.partial(call, level=level)
    # frunctools.wraps allow to not loose the function informations
    @functools.wraps(function)
    def log_then_call(*args, **kwargs):
        # avoid useless processing of parameters
        # by only treating messages that will be processed by the logger
        if logger.isEnabledFor(getattr(logger, level)):
            # arguments
            arguments = ', '.join([
                stringify(arg) for arg in args
            ])
            # keyword arguments
            keywords_arguments = ', '.join([
                '%s=%s' % (key, stringify(value))
                for key, value in kwargs.items()
            ])
            if arguments and keywords_arguments:
                parameters = '%s, %s' % (arguments, keywords_arguments)
            elif arguments or keywords_arguments:
                parameters = '%s' % (
                    arguments if arguments else keywords_arguments
                    )
            else: parameters = ''
            if level not in call.available_levels:
                raise ValueError(
                    "Unknown log level '%s', available ones: %s" % (
                        level, ', '.join(call.available_levels)
                    )
                )
            getattr(logger, level)(
                '%s: %s.%s(%s)' % (
                    call.func_name, function.__module__,
                    function.__name__, parameters
                )
            )
        return function(*args, **kwargs)
    return log_then_call

call.available_levels = ('critical', 'error', 'warning', 'info', 'debug')
