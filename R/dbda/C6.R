source ("programming_exercises/R/dbda/BernData.R")
source ("programming_exercises/R/dbda/HDIofICDF.R" )
source ("programming_exercises/R/dbda/openGraphSaveGraph.R") # read in graph functions

prior = c(4,4) 
posterior1 = BernBeta(prior, c(1))
posterior2 = BernBeta(posterior1, c(1))
posterior3 = BernBeta(posterior2, c(0))

#6.2
a58 = rep(1, times=58)
b42 = rep(0, times=42)
prior62 = c(1,1)
posterior62 = BernBeta(prior62, c(a58,b42))
a57 = rep(1, times=57)
b43 = rep(0, times=43)
posterior62B = BernBeta(posterior62, c(a57,b43))
f55 = rep(1, times=55)
j45 = rep(0, times=45)
prior63 = c(1,1)
posterior63 = BernBeta(prior63, c(f55,j45))
