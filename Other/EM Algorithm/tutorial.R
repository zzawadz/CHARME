# Example based on: http://ai.stanford.edu/~chuongdo/papers/em_tutorial.pdf

coin = c(1,0,0,1,0)


res = c(5,9,8,4,7)

sum(res[coin==1]/(length(res[coin==1])*10))
sum(res[coin==0]/(length(res[coin==0])*10))
p1 = 0.6
p2 = 0.5


for(i in 1:1000)
{

l1 = dbinom(res,prob=p1,size = 10)
l2 = dbinom(res,prob=p2,size = 10)

ll = l1+l2
w1 = l1/ll
w2 = l2/ll

p1 = sum(w1*res)/(sum(w1*res) + sum(w1*(10-res)))
p2 = sum(w2*res)/(sum(w2*res) + sum(w2*(10-res)))
}

p1
p2

####

### Even bigger example:
ncoin = 5   # number of coin
nsamp = 1000 # number of samples
nthr  = 100  # number of throws in each sample

coin = sample(1:ncoin,nsamp, replace = TRUE)
probs= sort(sample(seq(0.1,0.9,0.1), ncoin))

plot(coin, cex = 1.5, pch = 19, col = coin)


sample = 1:nsamp

for(i in 1:nsamp) sample[i] = rbinom(1,size = nthr,prob = probs[coin[i]])

# initialization of probabilities
niter = 2000
estprobs = sample(seq(0.1,0.9,0.05), ncoin)

for(i in 1:niter)
{
  denmat = sapply(estprobs, function(p) dbinom(sample,nthr,prob = p))
  summat = rowSums(denmat)
  weights = denmat/summat
  
  tmp1 = colSums(weights*sample)
  tmp2 = colSums(weights* (nthr-sample))
  
  estprobs = tmp1/(tmp1+tmp2)
}

### Results:
cbind(sort(estprobs),probs)

ordest = order(estprobs)
ord_weights = t(sapply(1:nsamp, function(i) weights[i, ordest]))

class = sapply(1:nsamp, function(i) which(ord_weights[i,]==max(ord_weights[i,])))

points(class)

sum(coin == class)/length(class)



