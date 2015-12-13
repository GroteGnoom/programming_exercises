x = seq( from = -2,
         to = 2,
         by = 0.1)

y = x ^ 2

plot (x,
      y,
      col = " skyblue",
      type = "l")

# ?plot
# ?par
# help.start()
# ??plot
# ?"??"

(TRUE | FALSE) & FALSE

all.equal(0.1*10/10, 0.1)

v1 = c (3, 4.5)

v1

v2 = c(4 , 5)

v2

v1 * v2

1:3^3

s = seq( to = 2,
         by = 0.1,
         length.out = 100
         )

s

rep(v1, times =2)
rep(v1, times = c(4,2))
rep (v1, each = 2, length = 20)

rep( v2, each=2, times=c(1,2,7,1) )

v3 = c (1, 2, 3, 4)
v3 [c(1,2)]
v3 [c(-1, -2)]
v3 [c(FALSE, TRUE, FALSE, TRUE)]

names (v3) = c("one", "two", "three", "four")
v3 [c("one", "two")]

v4 = c ("high", "high", "high", "medium", "low", "medium")

v4f = factor (v4)
as.numeric(v4f)

v4fo = factor ( v4, levels = c("low", "medium", "high"), ordered = TRUE)
v4fo
as.numeric(v4fo)
v4fol = factor ( v4, levels = c ("low", "medium" ,"high"), ordered = TRUE, labels=c("bottom", "middle", "top"))
v4fol

m1 = matrix( 1:4,
        ncol =2,
        byrow=TRUE, 
        dimnames = list(RowNames = c("Row1", "Row2"),
                        ColNames = c("Col1", "Col2")))

m1[1,2]
m1["Row1", "Col2"]
m1[,1]
m1[1:2,1]
m1[3]

a1 = array( 1:6,
             dim = c(1,2,3),
             dimnames = list(RowNames = c("Row1"),
                             ColNames = c("Col1", "Col2"),
                             Layername = c("Layer1", "Layer2", "Layer3")))

a1

l1 = list (1, "a", 1:3)
l1
l2 = list ("a" = 1, "b" = "a", "c" = 1:3)
l2
l1[2]
l2[2]
l1[[2]]
l2[[2]]
l1[[2]][1]
l2$b[1]

d = data.frame( numbers=1:3 , names=c("one","two","three") )
d
d$names
d[[1]]
d[1]
d[1,2]

getwd()

HGNNG = read.csv("programming_exercises/R/dbda/HGNNG.csv")
HGNNG$Hair
write.csv(HGNNG, file = "programming_exercises/R/dbda/HGNNG.csv", row.names=FALSE, quote = FALSE)
save(HGNNG, file = "programming_exercises/R/dbda/HGNNG.Rdata")
load(file = "programming_exercises/R/dbda/HGNNG.Rdata")
objects()

summary(v3)
summary(factor(v3))
head(s)
tail(s)
str(s)
aggregate(x=HGNNG$Number, by=list(HGNNG$Gender, HGNNG$Hair), FUN= median)
aggregate(x=list(Number = HGNNG$Number), by=list(Gender = HGNNG$Gender, Hair = HGNNG$Hair), FUN= median)
aggregate(Number ~ Gender + Hair, data = HGNNG, FUN= median)
aggregate(x=list(Count=rep(1,NROW(HGNNG))),
          by=list(Gender = HGNNG$Gender, Hair = HGNNG$Hair), FUN= sum)
table(list(Gender=HGNNG$Gender,Hair=HGNNG$Hair))

a1
apply (a1, MARGIN = c(1,3), FUN= sum)

install.packages("reshape2")
library(reshape2)
a1m = melt(a1)
a1m

funky = function (a , b, c=2) {
  return (a + b^2 + c)
}

funky (3, b=4)

x=0
if (x <= 1) {
  show("jaja")
} else {
  show("zozo")
}

for ( i in 1:10) {
  show(i)
}
?Control

proc.time()

plot( x=1:4 , y=c(1,3,2,4) , type="o" )
