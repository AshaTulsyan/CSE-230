from misc import Failure

class profiled(object):
    """Count the fuction to see how many times it has been executed."""
    def __init__(self,f):
        self.__count=0
        self.__f=f
        self.__name__=f.__name__
    def __call__(self,*args,**dargs):
        self.__count+=1
        return self.__f(*args,**dargs)
    def count(self):
        return self.__count
    def reset(self):
        self.__count=0


class traced(object):
    """
    Print out an ASCII art tree of the recursive calls and their return values.return
    If an exception occurs in the function, the nesting level must be adjusted to the appropriate level
    where the exception is caught.
    The return value of the funciton is returned to the caller
    after all printing is complete.
    """
    count = 0
    def __init__(self,f):
        self.__f = f
        self.__name__= f.__name__
    def __call__(self, *args,**kwargs):
        print "| " * traced.count + ",- " + self.__name__ + "(" + self.repr(*args,**kwargs) +")"
        traced.count += 1
        # raise exception
        try:
            res = self.__f(*args,**kwargs)
            traced.count -= 1
            print "| "*traced.count + "`- " + str(res)
            return res     
        except Exception, exception:
            traced.count -= 1
            raise exception 
    def repr(self,*args,**kwargs):
        """ used to return the representation of the arguments and the keyword arguments """
        res = ""
        if args:
            for i in args:
                res += str(i) + ", "
            res = res[:-2]
        if kwargs:
            res += ", "
            for k,v in kwargs.items():
                res += str(k) + "=" + str(v) + ", "
            res = res[:-2]
        return res


class memoized(object):
    """
    The decorator check to see if the function has already been called with 
    the given arguments. If so, the decorator should return the value
    the function returned when it was last called with the given arguments.
    If the function last threw an exception when called with the given arguments,
    the same exception should be thrown again. 
    If the function has not been called with the given arguments, then call it and record
    the value or exception. Then return the return value or raise the thrown exception.
    """
    def __init__(self,f):
        self.__name__ = f.__name__
        self.__f = f
        self.dict = {}
    def __call__(self,*args,**kwargs):
        string = str(args) + str(kwargs)
        # record the value or raise exception
        if string in self.dict:
            if(isinstance(self.dict[string],Exception)):
                raise self.dict[string]
            else:
                return self.dict[string]
        else:
            try:
                res = self.__f(*args, **kwargs)
                self.dict[string] = res
                return res
            except Exception, exception:
                self.dict[string] = exception
                raise exception


# run some examples.  The output from this is in decorators.out
def run_examples():
    for f,a in [(fib_t,(7,)),
                (fib_mt,(7,)),
                (fib_tm,(7,)),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (fib_mp.reset,()),
                (fib_mp,(7,)),
                (fib_mp.count,()),
                (even_t,(6,)),
                (quicksort_t,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (quicksort_mt,([5,8,100,45,3,89,22,78,121,2,78],)),
                (change_t,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                (change_mt,([9,7,5],44)),
                ]:
        print "RUNNING %s(%s):" % (f.__name__,", ".join([repr(x) for x in a]))
        rv=f(*a)
        print "RETURNED %s" % repr(rv)

@traced
def fib_t(x):
    if x<=1:
        return 1
    else:
        return fib_t(x-1)+fib_t(x-2)

@traced
@memoized
def fib_mt(x):
    if x<=1:
        return 1
    else:
        return fib_mt(x-1)+fib_mt(x-2)

@memoized
@traced
def fib_tm(x):
    if x<=1:
        return 1
    else:
        return fib_tm(x-1)+fib_tm(x-2)

@profiled
@memoized
def fib_mp(x):
    if x<=1:
        return 1
    else:
        return fib_mp(x-1)+fib_mp(x-2)

@traced
def even_t(x):
    if x==0:
        return True
    else:
        return odd_t(x-1)

@traced
def odd_t(x):
    if x==0:
        return False
    else:
        return even_t(x-1)

@traced
def quicksort_t(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_t([x for x in l[1:] if x<pivot])
    right=quicksort_t([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

@traced
@memoized
def quicksort_mt(l):
    if len(l)<=1:
        return l
    pivot=l[0]
    left=quicksort_mt([x for x in l[1:] if x<pivot])
    right=quicksort_mt([x for x in l[1:] if x>=pivot])
    return left+l[0:1]+right

class ChangeException(Exception):
    pass

@traced
def change_t(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_t(l[1:],a)
    else:
        try:
            return [l[0]]+change_t(l,a-l[0])
        except ChangeException:
            return change_t(l[1:],a)

@traced
@memoized
def change_mt(l,a):
    if a==0:
        return []
    elif len(l)==0:
        raise ChangeException()
    elif l[0]>a:
        return change_mt(l[1:],a)
    else:
        try:
            return [l[0]]+change_mt(l,a-l[0])
        except ChangeException:
            return change_mt(l[1:],a)


