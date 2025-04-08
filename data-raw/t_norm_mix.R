library(dplyr)

eval_t_error = function(lqdif, df=1, bound=20, n_eval=200) {
    quantiles = cumsum(exp(lqdif))
    sd_vals = sqrt(1/qgamma(quantiles, df/2, df/2))
    q_min = pt(-bound, df=df)
    eval_q = seq(q_min, 1-q_min, length.out=n_eval)
    x = qt(eval_q, df=df)
    out = rowMeans(vapply(sd_vals, function(s) dnorm(x, sd=s), numeric(n_eval)))
    sqrt(mean((out - dt(x, df=df))^2))
}

find_t_mix = function(df, n_mix, bound=25, n_eval=250) {
    init_lqdif = log(c(0.5/n_mix, rep(1/n_mix, n_mix-1)))
    res = optim(init_lqdif, eval_t_error, df=df, bound=bound, n_eval=n_eval)
    qq = cumsum(exp(res$par))
    attr(qq, "quality") = res$value
    qq
}

t_norm_mix = tidyr::crossing(df = c(1, 2, 3, 4, 7, 10, 20, 40, 100),
                n_mix = c(4, 7, 12)) %>%
    rowwise() %>%
    mutate(qq = list(as.numeric(find_t_mix(df, n_mix))))

filter(t_norm_mix, n_mix==7) %>%
    mutate(id = list(1:max(n_mix))) %>%
    tidyr::unnest(c(qq, id)) %>%
    group_by(df) %>%
    mutate(chg = qq - lag(qq, default=0)) %>%
ggplot(aes(id, chg, color=df, group=df)) +
    geom_line()

usethis::use_data(t_norm_mix, overwrite = TRUE)
