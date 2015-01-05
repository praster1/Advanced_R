SturgesK = function(vec)
{
	len_class = nclass.Sturges(vec)
	len_uniq = length(unique(vec))
	
	if (len_class >= len_uniq) {
		dat = vec;
		result = list(data = dat);
	} else {
		breaks = hist(vec, breaks = "Sturges", plot=FALSE)$breaks;
		
		where_vec = NULL;
		for (i in 2:length(breaks))
		{
			if (i != length(breaks))
			{
				where = which(vec <= breaks[i])
			} else {
				where = c(1:length(vec))
			}
			if (is.null(where_vec) == FALSE)
			{
				where = where[-where_vec]
			}
			where_vec = c(where_vec, where)
			vec[where] = letters[i-1]
		}
		
		result = list(	breaks = breaks,
							data = vec)
	}

	return(result)
}