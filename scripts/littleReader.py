import io
def trimNewline(s): return s[:-1]
def write(f, s): f.write(s)
def writeLn(f, s): f.write(s + '\n')

def readLittle(name, folder='../examples/'):
  f = folder + name + '.little'

  ## yield (name + ' = \"\n')
  ## following version is to facilitate line/col numbers:
  yield (name + ' =\n \"\"\"')

  for s in io.open(f,'r',encoding="utf8"):
    s = s.replace('\\','\\\\')
    s = s.replace('\"','\\\"')
    yield s

  yield ('\n\"\"\"\n\n')

