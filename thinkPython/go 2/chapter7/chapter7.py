import math
def test_square_root():
    for i in range(1,10):
        print(i,i**(1/2),math.sqrt(i),i**(1/2)-math.sqrt(i))
test_square_root()