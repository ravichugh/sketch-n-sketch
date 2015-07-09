
def trimNewline(s): return s[:-1]
def write(f, s): f.write(s)
def writeLn(f, s): f.write(s + '\n')

def readLittle(name):
  f = '../examples/' + name + '.little'

  yield (name + ' = \"\n')

  for s in open(f):
    yield s.replace('\\','\\\\')

  yield ('\n\"\n')

