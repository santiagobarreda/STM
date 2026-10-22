d = h95_data_no_NA
formants = d[, c("f1", "f2", "f3")]
nd = normalize(formants, d$speaker, d$vowel)
template = create_template(nd[, 1:3], nd$vowels)
lf = log(as.matrix(formants))
lf0 = log(d$f0)
