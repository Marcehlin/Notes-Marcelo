print(8>5)
print(8==10)

import numpy as np

a = np.array([True,False,True,True,False])
b = np.array([False,True,False,True,False])

print(a&b)
print(a|b)
print(np.logical_xor(a,b))