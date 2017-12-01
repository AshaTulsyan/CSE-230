"""This class is designed to raise failure exception.
	It will return the failure exception.
"""
class Failure(Exception):
    """Failure exception initialzie"""
    def __init__(self,value):
    	"""initialize"""
        self.value=value
    def __str__(self):
    	"""represent"""
        return repr(self.value)

