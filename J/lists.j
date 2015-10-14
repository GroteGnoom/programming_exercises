addlists =: dyad : 0 NB. Bad version of +
r =. ''
count =. # x
i =. 0
while. i < count do.
  left =. i { x
  right =. i { y
  sum =. left + right
  r =. r , sum
  i =. i +1
end.
r
)
