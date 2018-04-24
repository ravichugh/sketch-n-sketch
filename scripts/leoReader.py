## mostly duplicating littleReader.py
import io

def trimNewline(s): return s[:-1]
def write(f, s): f.write(s)
def writeLn(f, s): f.write(s + '\n')

def readLeo(name, folder='../examples/'):
  f = folder + name + '.elm'

  # e.g. LEO_TO_ELM fromleo/markdown ---> fromleo_markdown = ...
  name = name.replace('/', '_')

  ## yield (name + ' = \"\n')
  ## following version is to facilitate line/col numbers:
  yield (name + ' =\n \"\"\"')

  for s in io.open(f,encoding="utf8"):
    s = s.replace('\\','\\\\')
    s = s.replace('\"','\\\"')
    yield s

  yield ('\n\"\"\"\n\n')
