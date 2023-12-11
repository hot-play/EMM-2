#1 Провести выкладки и обосновать...
n = 200
del = 0.0001
x = seq(0, (del * (n - 1)), del)

Broun = function(n) {
  B = 0
  for(i in 2:n) {
    B[i] = B[i - 1] + rnorm(1, 0, sqrt(del))
  }
  return(B)
}

plot(x, Broun(n), type='l', ylim = c(-1, 1))
dover1 = (3 * sqrt((1:n) * del))
dover2 = -(3 * sqrt((1:n) * del))
lines(x, dover1, lty=2, lwd=3)
lines(x, dover2, lty=2, lwd=3)

#2  
for(i in 2:200){
  lines(x, Broun(N), type='l', col=i)
}

#3 Построить ансамбль реализаций процесса, замоделированного на предыдущем шаге,
# и вывести все реализации процесса на один график.
# Т.е. построить 200 реализаций процесса (8) на одном графике.
Ansabl = function(A0, a, d, n) {
  A = 0
  A = (A0 * exp(((1:N) * del) * (a - (d ** 2) / 2) + d * Broun(n)))
  return(A)
}
plot(x, Ansabl(1, 0.5, 0.9, n), type='l', ylim = c(0, 2))

#4 Построить 200 реализаций процесса (10) на одном графике.
for(i in 2:200) {
  lines(x, S(1, 0.5, 0.9, N), type='l', col=i)
}


