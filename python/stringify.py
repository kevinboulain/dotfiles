import types

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
    # list or tuple
    elif isinstance(argument, (types.TupleType, types.ListType,)):
        return '[{}]'.format(', '.join([stringify(arg) for arg in argument]))
    # dict
    elif isinstance(argument, (types.DictType,)):
        return '{{{}}}'.format(', '.join([
            '{}: {}'.format(stringify(key), stringify(value))
            for key, value in argument.iteritems() # iteritems: python2
        ]))
    # exception
    elif isinstance(argument, Exception):
        return '{}: {}'.format(argument.__class__.__name__, argument)
    # function
    elif isinstance(argument, (types.FunctionType, types.LambdaType,)):
        return str(argument.func_name)
    # handle everything else, treated as an object
    else:
        # we have a new-style class
        if isinstance(argument, type):
            return '{} new-style class'.format(argument.__name__)
        # we have an old style class
        elif not hasattr(argument, '__class__'):
            return '{} old-style class'.format(argument.__name__)
        # we have an instance
        elif isinstance(argument, argument.__class__):
            return '{} instance'.format(argument.__class__.__name__)

    # should never get here
    assert 0, "can't stringify %s" % str(argument)
