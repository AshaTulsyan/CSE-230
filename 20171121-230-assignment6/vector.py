from misc import Failure


class Vector(object):
  """
  implement a variety of operations for the Vector class
  """

  def __init__(self,a):
    """a constructor to the Vector class"""
    if isinstance(a,(int,long)):
      if a < 0:
        raise ValueError("Vector length cannot be negative")
      else:
        self.x = [0.0]*a
    elif isinstance(a,(str,unicode,list,tuple,bytearray,buffer,xrange)):
      self.x=[]
      for b in a:
        self.x.append(b)
    else:
      raise TypeError("It is not a valid vector length")
  
  def __repr__(self): 
    """return a string code which could be used to initialize the Vector"""
    return "Vector(" + str(self.x) + ")" 
  
  def __len__(self):
    """return the length of the Vector"""
    return len(self.x)
  
  def __iter__(self):
    """return an object that can iterate over the elements of the Vector"""
    for b in self.x:
      yield b
  

  def __add__(self,other):
    """ + operator"""
    if len(self) == len(other):
      added = list(a+b for a,b in zip(self,other))
      return Vector(added)
    else:
      raise ValueError("The length is not matched")
  
  def __radd__(self,other):
    """ right add operator"""
    if len(self) == len(other):
      added = list(a+b for a,b in zip(self,other))
      return Vector(added)
    else:
      raise ValueError("The length is not matched")
  
  def __iadd__(self,other):
    """ += operator"""
    if len(self) == len(other):
      added = list(a+b for a,b in zip(self,other))
      self.x = added
      return self
    else:
      raise ValueError("The length is not matched") 
  
  def dot(self,other):
    """return the dot product of the argument"""
    if len(self) == len(other):
      res = 0
      for a,b in zip(self,other):
        res += a*b
      return res
    else: 
      raise ValueError("The length is not matched")
  
  def __getitem__(self,key):
    """allow element level access to the Vector"""
    return self.x[key]
  
  def __setitem__(self,key,value):
    """set the item"""
    self.x[key] = value 
  
  def __getslice__(self,i,j):
    """allow slice level access to the Vector"""
    return self.x[i:j]
  
  def __setslice__(self,i,j,sequence):
    """allow slice level access to the Vector"""
    if len(self.x[i:j]) == len(sequence):
      self.x[i:j] = sequence
    else:
      raise ValueError("The length is not matched")
  
  def __eq__(self,other):
    """ = operator"""
    if isinstance(other, Vector):
      for a,b in zip(self,other):
        if a!= b:
          return False
      return True
    else:
      return False
  
  def __gt__(self,other):
    """ > operator"""
    # sorted() returns a new sorted list, leaving the original list unaffected
    # list.sort() sorts the list in place
    for a,b in zip(sorted(self.x,reverse = True),sorted(other,reverse = True)):
      if a > b:
        return True
      elif a < b:
        return False
      else: 
        continue
    return False
  
  def __ge__(self,other):
    """>= operator"""
    for a,b in zip(sorted(self.x,reverse = True),sorted(other,reverse = True)):
      if a > b:
        return True
      elif a < b:
        return False
      else: 
        continue
    return True
  
  def __lt__(self,other):
    """< operator"""
    for a,b in zip(sorted(self.x,reverse = True),sorted(other,reverse = True)):
      if a < b:
        return True
      elif a > b:
        return False
      else: 
        continue
    return False
  
  def __le__(self,other):
    """<= operator"""
    for a,b in zip(sorted(self.x,reverse = True),sorted(other,reverse = True)):
      if a < b:
        return True
      elif a > b:
        return False
      else: 
        continue
    return True
  
  def __ne__(self,other):
    """!= operator"""
    if isinstance(other, Vector):
      for a,b in zip(self,other):
        if a == b:
          return False
      return True
    else:
      return True
