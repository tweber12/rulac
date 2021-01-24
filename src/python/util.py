"""
A collection of helper functions that don't belong anywhere else
"""

import errno
import os

def makedirs(path):
    """
    Creates a directory and its parent directories

    Does nothing if the directory already exists.
    """
    try:
        os.makedirs(path)
    except OSError as err:
        if err.errno == errno.EEXIST:
            pass
        else:
            raise err
