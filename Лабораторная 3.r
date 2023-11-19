library(readr)
library(stats)
library(forecast)
library(tseries)
# Реализовать 𝐴𝑅(2)𝐴𝑅𝐶𝐻(3) процесс
ar2_arch3 = function(teta, a, n) {
  x = double(n)

  s = a[1]
  x[1] = (s ** (1/2)) * rnorm(1, 0, 1)

  s = a[1] + a[2] * x[1] ** 2
  x[2] = x[1] * teta[1] + (s ** (1/2)) * rnorm(1, 0, 1)

  s = a[1] + a[2] * x[2] ** 2 + a[3] * x[1] ** 2
  x[3] = teta %*% matrix(c(x[2], x[1]), 2, 1) + (s ** (1/2)) * rnorm(1, 0, 1)

  for (i in 4:n) {
    s = a[1] + a[2] * x[i-1] ** 2 + a[3] * x[i-2] ** 2 + a[4] * x[i-3] ** 2
    x[i] = teta %*% matrix(c(x[i-1], x[i-2]), 2, 1) + (s ** (1/2)) * rnorm(1, 0, 1)
  }

  plot(x, type='l')
  return(x)
}

teta = matrix(c(-0.3, 0.4), 1, 2)
a = c(1, 0.2, 0.1, 0.2)
ar_arch_1 = ar_arch(teta, a, 2100)

# Разделить, полученную на первом шаге последовательность

learn = ar_arch_1[1:2000]
test = ar_arch_1[2001:2100]

# На основе обучающей выборки получить оценки параметров
arima_1 = arima(learn, order = c(2, 0, 0), include.mean = F)
teta = c(arima_1$coef[1], arima_1$coef[2])
learn_1 = learn[1]
learn_1[2] = learn[2] - teta[1] * learn[1]
i = 3:2000
learn_1[i] = learn[i] - teta %*% matrix(c(learn[i-1], learn[i-2]), 2, 1)

garch_1 = garch(learn_1, c(0, 3))
a = c(garch_1$coef[1], garch_1$coef[2], garch_1$coef[3], garch_1$coef[4])

# Построить последовательность прогнозов
prognoz = function(teta, a, test, n) {

  x = double(n)
  up = double(n)
  down = double(n)

  s = a[1]
  x[1] = 0
  down[1] = x[1] - s ** (1/2)
  up[1] = x[1] + s ** (1/2)

  s = a[1] + a[2] * x[1] ** 2
  x[2] = test[1] * teta[1]
  down[2] = x[2] - s ** (1/2)
  up[2] = x[2] + s ** (1/2)

  s = a[1] + a[2] * x[2] ** 2 + a[3] * x[1] ** 2
  x[3] = teta %*% matrix(c(test[2], test[1]), 2, 1)
  down[3] = x[3] - s ** (1/2)
  up[3] = x[3] + s ** (1/2)

  for (i in 4:n) {
    s = a[1] + a[2] * x[i-1] ** 2 + a[3] * x[i-2] ** 2 + a[4] * x[i-3] ** 2
    x[i] = teta %*% matrix(c(test[i-1], test[i-2]), 2, 1)
    down[i] = x[i] - s ** (1/2)
    up[i] = x[i] + s ** (1/2)
  }

  plot(x, type='p', col='green')
  lines(test, type='l', col='blue')
  lines(up, type='l', lty='2525', col='red')
  lines(down, type='l', lty='2525', col='red')
  return(x)
}

prognoz_1 = prognoz(teta, a, test, 100)

# Импортировать скачанные данные
data = read_delim(file = "testdata.txt")
# Построить график динамики актива
P = data$`<OPEN>`
plot(P, type='l')
# Привести данные к стационарному виду

x = 0
for(i in 2:length(P)) {
  x[i] = (P[i] - P[i-1]) / P[i-1]
}

# Построить график доходностей
plot(x, type='l')

# Повторить шаги 2-4 для последовательности
data_learn = x[1:4800]
data_test = x[4801:5000]
arima_2 = arima(data_learn, order = c(2, 0, 0), include.mean = F)
teta = c(arima_2$coef[1], arima_2$coef[2])
learn_2 = data_learn[1]
learn_2[2] = data_learn[2] - teta[1] * data_learn[1]
i = 3:4800
learn_2[i] = data_learn[i] - teta %*% matrix(c(data_learn[i-1], data_learn[i-2]), 2, 1)
garch_2 = garch(learn_2, c(0, 3))
a = c(garch_2$coef[1], garch_2$coef[2], garch_2$coef[3], garch_2$coef[4])

ar2_arch3_2 = ar2_arch3(teta, a, data_test, 200)
prognoz_2 = prognoz(teta, a, data_test, 200)
  