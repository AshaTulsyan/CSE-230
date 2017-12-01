#PA 4

import re

"Miscellaneous functions to practice Python"

class Failure(Exception):
    """Failure exception"""
    def __init__(self,value):
        self.value=value
    def __str__(self):
        return repr(self.value)

# Problem 1

# data type functions

def closest_to(l,v):
    d = v
    res = v
    for elt in l:
      if abs(elt-v) < d:
        d = min(abs(elt-v),d)
        res = elt 
    return res

def make_dict(keys,values):
    return dict(zip(keys,values))

   
# file IO functions
def word_count(fn):
    fh = open(fn,"r")
    dic = {}
    words = re.split("\W+",fh.read().lower().strip())
    for word in words:
        if word:
            if word not in dic:
                dic[word] = 1
            else:
                dic[word] += 1
    fh.close()
    return dic









