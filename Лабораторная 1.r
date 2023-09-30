# Построить график процесса 𝐴𝑅(1)
ar = function(n, O) {
  	x = double(n)
	x[1] = 0
  	for (i in 2 : n) {
    	x[i] = O * x[i-1] + rnorm(1, 0, 0.5)
  	}
  	return(x)
}

ar_1 = ar(1000,0.5)
ar_2 = ar(1000,1)
ar_3 = ar(1000,1.5)

plot(ar_1,type = 'l')
plot(ar_2,type = 'l')
plot(ar_3,type = 'l')
 
# Оценить параметр |𝜃| ≤ 1 по методу наименьших квадратов (МНК)
mnk = function(n, ar) {
   	sum_1 = 0
   	sum_2 = 0
   	for(i in 2 : n) {
     	sum_1 = sum_1 + (ar[i-1] * ar[i-1])
     	sum_2 = sum_2 + (ar[i-1] * ar[i])
   	}
  	return (sum_2/sum_1)
}

k = mnk(100, ar_1)

# Найти оценку максимального правдоподобия (МП) параметра 𝜃 процесса 𝐴𝑅(1).
mp = function(O) {
	sum = 0
   	for (i in 2 : 100) {
     	sum = sum + (ar_1[i] - O * ar_1[i-1])^2
   	}
   	return (sum)
}

# Сделайте вывод о взаимосвязи оценок МНК и МП для параметра 𝜃 в случае гауссовского шума.
mnk_1 = mnk(100, ar_1)
mp_1 = optimise(f=mp, interval=c(-10,10))

mnk_1
mp_1
# Минимум совпал

# Рассчитать МНК-оценки для объема выборки 𝑘 = 10, 11, . . . , 𝑛. Посмотереть на динамику оценки, в зависимости от объема выборки, и сделать выводы.
ar_1 = ar(1000, 0.5)
mnk_10 = double(10)
mnk_10[1] = 0
for (i in 2 : 10) {
    mnk_10[i] = mnk(i, ar_1)
}
plot(mnk_10, type = 'l')


mnk_1000 = double(n - 10)
for (i in 10 : n) {
    mnk_1000[i - 9] = mnk(i, ar_1)
}
plot(mnk_1000, type = 'l')

# Построить график устойчивого процесса 𝐴𝑅(2), из 𝑛 наблюдений.
ar2 = function(n, O1, O2) {
    x = double(n)
    x[1] = 1
    x[2] = 2
    for (i in 3 : n) {
       	x[i] = O1 * x[i - 2] + O2 * x[i - 1] + rnorm(1, 0, 0.5)
    }
    return(x)
}
 
ar_2 = ar2(1000, 0.1, 0.2)
plot(ar_2, type = 'l')
 
# Вычислить значение параметра 𝐴𝑅(2), используя функцию 𝑎𝑟𝑖𝑚𝑎 пакета 𝑠𝑡𝑎𝑡�
library("stats")
arima(ar_2, order = c(2, 0, 0), include.mean = FALSE)