N=5000

flipsequence = sample (x = 0:1, prob=c(0.5,0.5), size = N, replace = TRUE )

r = cumsum(flipsequence)

plot (1:N, r/ (1:N), ylim = c(0.0, 1.0))

abline (h=0.5, lty = "dotted")
text (N, .6, paste(r[N]/N))



      