# Построить график стационарного процесса {ℎ𝑛} и график волатильности
# {𝜎𝑛} процесса 𝐺𝐴𝑅𝐶𝐻(1,0), из 𝑛 = 1000 наблюдений.
garch1 = function(a0, a1, n) {
    d = double(n)
    h = double(n)
    h[1] = 0
    for(i in 2 : n + 1){
        d[i] = a0 + a1 * h[i-1] ** 2
        h[i] = rnorm(1, 0, 1) * sqrt(d[i])
    }
    plot(h, type='l')
    plot(d, type='l')
    return(h)
}
g_1 = garch1(0.6, 0.2, 1000)

# Оценить параметры 𝑎0 и 𝑎1 с помощью метода наименьших квадратов
# (МНК), путем преобразования процесса 𝐴𝑅𝐶𝐻(1) ≡ 𝐺𝐴𝑅𝐶𝐻(1, 0) к
# процессу авторегрессии первого порядка.

mnk = function(g, n) {
    M = double(2)
    x = 0
    y = 0
    xx = 0
    xy = 0
    for (i in 2 : n) {
        x = x + (g[i-1] ** 2)
        y = y + (g[i] ** 2)
        xx = xx + (g[i-1] ** 4)
        xy = xy + (g[i-1] ** 2) * (g[i] ** 2)
    }
    M[1] = (xy / n-x * y / (n ** 2)) / (xx / n - (x / n) ** 2)
    M[2] = (y / n) - (M[1] * x / n)
    return (M)
}
ev = mnk(g_1, 1000)
ev

# Оценить параметры 𝑎0, 𝑎1 при помощи функции 𝑔𝑎𝑟𝑐ℎ() пакета 𝑡𝑠𝑒𝑟𝑖𝑒𝑠 по
# выборке {ℎ𝑛}.
library(tseries)
garch(g_1, order = c(0, 1))

# Построить график стационарного процесса 𝐺𝐴𝑅𝐶𝐻(3,0), из 𝑛 = 1100 наблюдений.
garch3 = function(a, n){
    h = double(n)
    s = double(n)
    h[1] = 0
    
    s[2] = a[1] + a[2] * h[1] ** 2
    h[2] = rnorm(1, 0, 1) * sqrt(s[2])
    
    s[3] = a[1] + a[2] * h[2] ** 2 + a[3] * h[1] ** 2
    h[3] = rnorm(1, 0, 1) * sqrt(s[3])
    
    s[4] = a[1] + a[2] * h[3] ** 2 + a[3] * h[2] ** 2 + a[4] * h[1] ** 2
    h[4] = rnorm(1, 0, 1) * sqrt(s[4])
    
    for(i in 5:n) {
        s[i] = a[1] + a[2] * h[i-1] ** 2 + a[3] * h[i-2] ** 2 + h[4] * h[i-3] ** 2
        h[i] = rnorm(1, 0, 1) * sqrt(s[i])
    }
    
    plot(h, type='l')
    plot(s, type='l')
    return(h)
}

a = c(0.1, 0.3, 0.4, 0.2)
p = garch3(a, 1100)
p_1 = p[1:1000]
p_2 = p[1001:1100]
a_new = garch(p_1, c(0, 3))

a = c(a_new$coef[1], a_new$coef[2], a_new$coef[3], a_new$coef[4])
a

prognoz = function(a, g) {
    n = length(g)
    s = double(n)
    s[1] = a[1]
    s[2] = a[1] + a[2] * g[1] ** 2
    s[3] = a[1] + a[2] * g[2] ** 2 + a[3] * g[1] ** 2
    for(i in 4:n) {
        s[i] = a[1] + a[2] * g[i-1] ** 2 + a[3] * g[i-2] ** 2 + g[4] * g[i-3] ** 2
    }
    plot(g*g, type='l', col = "black")
    lines(s, type='p', col = "red")
    return(s)
}
prognoz_1 = prognoz(a, p_1)

# Построить стационарный процесс 𝐺𝐴𝑅𝐶𝐻(1,1), из 𝑛 = 1000 наблюдений
# и оценить его параметры (𝑎0, 𝑎1, 𝑏1)
# по выборке {ℎ𝑛}, используя функцию
# 𝑔𝑎𝑟𝑐ℎ().

#a[3] - в данной функции это b1

garch_1_1 = function(a, n) {
    h = double(n)
    s = double(n)

    h[1] = 1
    s[1] = a[1] + a[2] * h[1]

    for(i in 2:n) {

    s[i] = a[1] + a[2] * h[i-1] ** 2 + a[3] * h[i-1] ** 2
    h[i] = rnorm(1, 0, 1) * sqrt(s[i])

    }
    plot(h, type = 'l')
    plot(s, type = 'l')
    return(h)
}
g_1 = garch_1_1(c(0.1, 0.25, 0.6), 1000)
garch(g_1, c(1, 1))