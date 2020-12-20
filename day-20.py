# there is no way in hell I'm porting this to idris

import sys
tiles = [(int(p.split()[1][:-1]),  tuple(map(tuple, p.split("\n")[1:]))  ) for p in sys.stdin.read().strip().split("\n\n")]

def borders(t):
  tz = tuple(map(tuple, zip(*t)))
  return {t[0], t[-1], t[0][::-1], t[-1][::-1], tz[0], tz[-1], tz[0][::-1], tz[-1][::-1]}

def can_border(t1, t2):
  b1 = borders(t1)
  b2 = borders(t2)
  return len(b1&b2) > 0

from collections import defaultdict
count = defaultdict(int)

for j, (it, t) in enumerate(tiles):
  for iu, u in tiles[j+1:]:
    if can_border(t, u): count[it]+=1; count[iu]+=1

prod=1
corners = []
for k,v in count.items():
  if v==2: prod *= k; corners.append(k)
print(prod)

c0 = corners[0]
tiles = dict(tiles)
layout = {(0,0): (c0, tiles[c0])}
candidates = set(tiles.keys()) - {c0}
frontier = {(0,0)}
no_need = set()

def rev(x): return tuple(x[::-1])

def flips(t):
  tz = tuple(map(tuple, zip(*t)))
  for i in (1,-1):
   yield t[::i]
   yield tz[::i]
   yield tuple(map(rev, t[::i]))
   yield tuple(map(rev, tz[::i]))

def lay():
  for (x,y) in frontier:
    tid, t = layout[x,y]
    ns = sum(kk in layout for kk in ((x-1,y),(x+1,y),(y-1,y),(y+1,y)))
    if ns == 4: no_need.add((x,y))
    for cid in list(candidates):
      for cf in flips(tiles[cid]):
        if t[0] == cf[-1]: return (x,y-1,cf,cid)
        if t[-1] == cf[0]: return (x,y+1,cf,cid)
        if tuple(r[0] for r in t) == tuple(r[-1] for r in cf): return (x-1,y,cf,cid)
        if tuple(r[-1] for r in t) == tuple(r[0] for r in cf): return (x+1,y,cf,cid)

for i in range(150):
  z = lay()
  if z is None: break
  nx,ny,cf,cid = z
  # print(nx,ny,cid)
  candidates -= {cid}
  frontier.add((nx,ny))
  frontier -= no_need
  no_need.clear()
  layout[nx,ny] = (cid, cf)

#print(layout)


s = [[0]*12*8 for i in range(12*8)]

for (x,y), (cid,c) in layout.items():
  for yy,row in enumerate(c[1:-1]):
    for xx,c in enumerate(row[1:-1]):
      s[y*8+11*8+yy][x*8+xx] = c

import re

# monster width is 20, line width is 12*8=96
# so we expect monster -> 77 chars (96-20+newline) -> more monster...
# note that . doesn't match newlines in regex, but [\s\S] does.
monster = re.compile('(?=\
..................#.[\\s\\S]{77}\
#....##....##....###[\\s\\S]{77}\
.#..#..#..#..#..#...)', re.MULTILINE)

s = [''.join(k) for k in s]
zs = [''.join(r) for r in zip(*s)]
rs = [r[::-1] for r in s]
zrs = [''.join(r[::-1]) for r in zip(*s)]
oris = [
    '\n'.join(s),
    '\n'.join(s[::-1]),
    '\n'.join(zs),
    '\n'.join(zs[::-1]),
    '\n'.join(rs),
    '\n'.join(rs[::-1]),
    '\n'.join(zrs),
    '\n'.join(zrs[::-1]),
]

n = 0
for ori in oris:
  n = max(n, len(re.findall(monster, ori)))

print(oris[0].count('#') - n * monster.pattern.count('#'))
