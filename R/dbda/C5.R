Theta = seq (0, 1, by = 0.01)
unNormalizedPrior = pmin(Theta, 1- Theta)
prior = unNormalizedPrior/sum(unNormalizedPrior)
Data = c(rep(0,3), rep(1,1))
z= sum (Data)
N = length (Data)
pDataGivenTheta = Theta^z * (1-Theta)^(N-z)
pData = sum (pDataGivenTheta * prior)
pThetaGivenData = pDataGivenTheta * prior / pData
plot(Theta,prior)
plot(Theta, pDataGivenTheta)
plot(Theta, pThetaGivenData)

