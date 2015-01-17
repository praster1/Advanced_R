cronbach = function (mat) 
{
    mat = na.omit(mat)
    nmat = ncol(mat)
    pmat = nrow(mat)
    alpha = (nmat/(nmat - 1)) * (1 - sum(apply(mat, 2, var))/var(apply(mat,1, sum)))
    resu = list(sample.size = pmat, number.of.items = nmat, alpha = alpha)
    resu
}