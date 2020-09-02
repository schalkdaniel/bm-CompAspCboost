x = runif(10000000, 10, 1000)
y = sin(x)
yn = y + 1

f = approxfun(x = x, y = (y - yn)^2)
integrate(f = f, upper = max(x), lower = min(x))


