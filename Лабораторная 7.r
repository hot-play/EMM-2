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

#3 Реализовать процесс
# Рассчитать выборочную вероятность разорения фирмы при следующих
# значениях параметров
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
