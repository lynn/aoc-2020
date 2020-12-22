for recursive in [False, True]:
  p1 = [19,5,35,6,12,22,45,39,14,42,47,38,2,26,13,30,4,34,43,40,16,8,23,50,36]
  p2 = [1,21,29,41,32,28,9,37,49,20,17,27,24,3,33,44,48,31,15,25,18,46,7,10,11]

  def winner(p1, p2):
    seen = set()
    while p1 and p2:
      t = (tuple(p1),tuple(p2))
      if t in seen: return 1
      seen.add(t)
      c1 = p1.pop(0)
      c2 = p2.pop(0)
      if recursive and len(p1) >= c1 and len(p2) >= c2:
        p1wins = (winner(p1[:c1], p2[:c2]) == 1)
      else:
        p1wins = (c1 > c2)
      if p1wins: p1 += [c1,c2]
      else: p2 += [c2,c1]
    return 1 if p1 else 2

  winner(p1, p2)
  p = p1+p2
  print(sum(i*c for i,c in enumerate(p[::-1], 1)))
