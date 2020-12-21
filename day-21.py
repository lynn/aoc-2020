import sys
from z3 import Solver, Int, Distinct, Or

# Total number of distinct ingredients.
n_i = 0
# Maps ingredient names to numbers.
ingredients = {}
# Maps allergens to integer SAT variables.
# ("Each allergen is found in exactly one ingredient".)
allergens = {}
# All ingredient numbers that ever showed up.
all_inums = []

solver = Solver()
for line in sys.stdin:
  iline, aline = line.strip().split(' (contains ')
  inames = iline.split()
  anames = aline[:-1].split(', ')
  # Convert ingredients to numbers:
  inums = []
  for i in inames:
    if i not in ingredients:
      n_i += 1
      ingredients[i] = n_i
    inums.append(ingredients[i])
  all_inums += inums
  # Make SAT clauses for allergens:
  # "a b c (contains x, y)"  =>  (x=a or x=b or x=c) and (y=a or y=b or y=c)
  for a in anames:
    if a not in allergens:
      allergens[a] = Int(a)
    solver.add(Or([allergens[a] == i for i in inums]))

# All variables are distinct ("Each ingredient contains zero or one allergen",
# i.e. no ingredient occurs twice or more among the solution.)
solver.add(Distinct(list(allergens.values())))

# Merry Christmas!!!!!!!!!!!!
solver.check()
m = solver.model()

# *
bad_ingredients = {m[a].as_long() for a in m}
print('*  ', sum(i not in bad_ingredients for i in all_inums))

# ** Oh no, we need the ingredient names again, dfgjh ok
lookup = {v: k for k, v in ingredients.items()}
answer = [(a.name(), lookup[m[a].as_long()]) for a in m]
print('** ', ','.join(i for (a,i) in sorted(answer)))
