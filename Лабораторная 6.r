#1 Замоделировать процесс
n = 1000
S0 = 100
a = 0.5
sig = 0.8
del = 0.0001

Broun = function(n) {
  B = 0
  for(i in 2:n) {
    B[i] = B[i - 1] + rnorm(1, 0, sqrt(del))
  }
  return(B)
}

Ansabl = function(A0, a, d, n) {
  A = 0
  A = (A0 * exp(((1:n) * del) * (a - (d ** 2) / 2) + d * Broun(n)))
  return(A)
}
ansabl1 = Ansabl(S0, a, sig, n)
ansabl1

#2 Рассчитать оценки параметров броуновского движения
x = log(ansabl1[2]) - log(ansabl1[1])
for(i in 3:length(ansabl1)) {
  x[i-1] = log(ansabl1[i]) - log(ansabl1[i-1])
}

sig_teor = var(x)/del
sqrt(sig_teor)
sig

a_teor = mean(x)/del + sig_teor/2
a_teor
a