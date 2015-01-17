make_digit_mat = function(vec)
{
	n = length(vec)
	uniq = unique(vec)
	sort_uniq = sort(unique(vec))
	
	res_mat = matrix(0, n, length(uniq))
	
	for (i in 1:length(uniq))
	{
		res_mat[which(vec == sort(unique(vec))[i]), i] = 1
	}
	
	dimnames(res_mat)[[2]] = sort(unique(vec))
	
	return(res_mat)
}
