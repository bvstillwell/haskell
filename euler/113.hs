import Common

digits = 10
num = digits * 10
width = 100000


pascal = (init.init.tail.pascalRow) digits
multiplier = 1:[2,2..]
distance = [0,1..]

calc p m d = m*p*(width-d)

answer = sum $ zipWith3 calc pascal multiplier distance
