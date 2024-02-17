#1 Реализовать 1000 случайных величин 𝑁𝑡 при фиксированном значении 𝑡 = 50, 𝜆 = 2
l = 2
t = 50
Nt = 0
for(i in 1:1000) {
  tr = 0
  col = 0
  while(sum(tr) < t) {
    tr[col+1] = rexp(1,l)
    col = col + 1
  }
  Nt[i] = col
  i = i + 1
}

#2 Построить гистограмму реализованной последовательности и наложить на гистограмму график
layout(matrix(c(1), 1))
layout.show(1)
hist(Nt,freq = FALSE, col = "red")
f = function(x, n) {
  fx = (((l*t) ** x) / factorial(x) ) * exp(-l * t)
  return(fx)
}
f(Nt, length(Nt))
x = 1:Nt[1]
curve(f(x, Nt[1]), add = TRUE)

#3 Реализовать процесс до фиксированного момента времени
Ti = 0
i = 1
Ti[1] = tr[1]
Ti[1]
while(i < Nt[1]) {
  Ti[i+1] = Ti[i] + tr[i+1]
  i = i + 1
}
UT = function(c, m, U0) {
  U = 0
  U[1] = U0
  trr = 0
  j = 1
  while(j < Nt[1]) {
    U[j+1] = U[j] + c * tr[j+1] - rexp(1, m)
    j = j + 1
  }
  return(U)
}
layout(matrix(c(1, 2), 1, 2))
layout.show(2)
plot(UT(5, 0.5, 50)~Ti, type='l')
plot(UT(1, 0.5, 50)~Ti, type='l')

#4 Рассчитать выборочную вероятность разорения фирмы при следующих
# значениях параметров:
mu = 1/3
U0 = 100
N = 1000
lm = 0.3
t = 1000
c = 1
tr = 0
Nt = 0
while(sum(tr) < t){
  tr[Nt+1] = -log(1-runif(1, 0, 1))/lm
  Nt = Nt + 1
}
Nt = Nt-1

Ti = 0
i = 1
Ti[1] = tr[1]
while(i < Nt){
  Ti[i+1] = Ti[i] + tr[i+1]
  i = i + 1
}
proces=function(c, mu, U0) {
  U = 0
  U[1] = U0
  trr = 0
  j = 0
  while(j < Nt) {
    j = j + 1
    if(U[j] > 0) { 
      U[j+1] = U[j] + c*tr[j+1] - rexp(1, mu)
    }
    else return(1)
  }
  return(0)
}
proces_1 = 0
for(b in 1:N) {
  proces_1 = proces_1 + proces(c, mu, U0)
}
Ksi_t = proces_1 / N
Ksi_t
po = (c*mu)/lm - 1
po
Ksi = exp(-mu * po * U0 / (1 + po))
Ksi

