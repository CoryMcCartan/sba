rt_mix = function(n, df=1, n_mix=10) {
    quantiles = seq(0.5/n_mix, 1-0.5/n_mix, length.out=n_mix)
    prec_vals = qgamma(quantiles, df/2, df/2)
    rnorm(n, 0, sample(sqrt(1/prec_vals), n, replace=T))
}

dt_mix = function(x, df=1, log=F, n_mix=10) {
    quantiles = seq(0.55/n_mix, 1-0.48/n_mix, length.out=n_mix)
    sd_vals = sqrt(1/qgamma(quantiles, df/2, df/2))
    out = rowMeans(vapply(sd_vals, function(s) dnorm(x, sd=s), numeric(length(x))))
    if (isTRUE(log)) log(out) else out
}

if (F) {
curve(dt_mix(x, 1, n_mix=8), -8, 8)
curve(dt(x, 1), -8, 8, add=T, col='red')
x = seq(-8, 8, length.out=200)

plot(dt(x, 1) - dt_mix(x, 1, n_mix=1), type='l', ylim=c(-0.01, 0.01))

#qqplot(rt(1e4, 3), rt_mix(1e4, 3, 40))


eval_error = function(lqdif, df=1, bound=20, n_eval=200) {
    quantiles = cumsum(exp(lqdif))
    sd_vals = sqrt(1/qgamma(quantiles, df/2, df/2))
    q_min = pt(-bound, df=df)
    eval_q = seq(q_min, 1-q_min, length.out=n_eval)
    x = qt(eval_q, df=df)
    out = rowMeans(vapply(sd_vals, function(s) dnorm(x, sd=s), numeric(n_eval)))
    sqrt(sum((out - dt(x, df=df))^2))
}

df = 1
bound = 20
n_eval = 500

n_mix = 7
init_lqdif = log(c(0.5/n_mix, rep(1/n_mix, n_mix-1)))
res = optim(init_lqdif, eval_error, df=df, bound=bound, n_eval=n_eval)
res$value
qq = cumsum(exp(res$par))
plot(qq, type='b')

q_min = pt(-bound, df=df)
eval_q = seq(q_min, 1-q_min, length.out=n_eval)
x = qt(eval_q, df=df)

sd_vals = sqrt(1/qgamma(qq, df/2, df/2))
out = rowMeans(vapply(sd_vals, function(s) dnorm(x, sd=s), numeric(length(x))))
plot(x, dt(x, df) - out, type='l', ylim=c(-0.01, 0.01), xlim=c(-10, 10))

plot(x, dt(x, df), type='l', col='red', xlim=c(-10, 10))
lines(x, out)

}
