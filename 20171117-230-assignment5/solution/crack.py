
from misc import *
import crypt
import re

def load_words(filename,regexp):
    with open(filename) as f:
      content = f.readlines()
    newContent = []
    for x in content:
      temp = re.findall(regexp,x.strip())
      if temp:
        for t in temp:
          newContent.append(t)
    return newContent

def transform_reverse(str):
    return [str,str[::-1]]

def transform_capitalize_helper(str):
    if not str:
      yield ""
    else:
      temp = str[0]
      if temp.upper() == temp.lower():
        for subEach in transform_capitalize_helper(str[1:]):
          yield temp + subEach
      else:
        for subEach in transform_capitalize_helper(str[1:]):
          yield temp.upper() + subEach
          yield temp.lower() + subEach

def transform_capitalize(str):
    return list(transform_capitalize_helper(str))

def transform_digits_helper(str,dic):
    if not str:
      yield ""
    else:
      temp = str[0]
      if temp.upper() == temp.lower() or (temp.lower() not in dic):
        for subEach in transform_digits_helper(str[1:],dic):
          yield temp + subEach
      else:
        for subEach in transform_digits_helper(str[1:],dic):
          yield temp + subEach
          for digit in dic[temp.lower()]:
            yield digit + subEach

def transform_digits(str):
    dic = {'o':['0'], 'i':['1'], 'l':['1'], 'z':['2'], 'e':['3'],'a':['4'],'s':['5'],'b':['6','8'],'t':['7'],'g':['9'],'q':['9']}    
    return list(transform_digits_helper(str,dic))

def check_pass(plain,enc):
    salt = enc[:2]
    if crypt.crypt(plain,salt) == enc:
      return True
    return False

def load_passwd(filename):
    res = []
    with open(filename) as f:
      content = f.read().splitlines()
    for x in content:
      dic = {'account':'','shell':'','UID':0,'GID':0,'GECOS':'','directory':'','password':''}
      temp = re.split(':',x)
      dic['account'] = temp[0]
      dic['password'] = temp[1]
      dic['UID'] = int(temp[2])
      dic['GID'] = int(temp[3])
      dic['GECOS'] = temp[4]
      dic['directory'] = temp[5]
      dic['shell'] = temp[6]
      res.append(dic)
    return res
    
def crack_pass_file(pass_filename,words_filename,out_filename):
    passwords = {each['password']: each['account'] for each in load_passwd(pass_filename)} 
    pa = [ each['password'][:2] for each in load_passwd(pass_filename)]
    fw = open(out_filename,'w')
    with open(words_filename) as f:
      content = f.read().splitlines()
    for x in content:
      for y in pa:
        for z in transform_reverse(x):
          crypted = crypt.crypt(z,y)
          if crypted in passwords:
            fw.write(passwords[crypted]+'='+x+'\n')

