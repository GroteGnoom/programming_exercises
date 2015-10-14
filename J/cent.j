centigrade =: 3 : 0 NB. 'centigrade =. verb define' means the same
t1 =. y - 32 NB. Really should be y, not x
t2 =. t1 * 5
t3 =. t2 % 9
)


centigrade2 =. verb define
t1 =. y - 32
t2 =. t1 * 5
t3 =. t2 % 9
)

centigrade3 =: 3 : '((y - 32) * 5) % 9'

