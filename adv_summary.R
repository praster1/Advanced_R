adv_summary = function(dat)
{
	if (is.matrix(dat)) {
		type = "matrix_or_frame";
	} else if (is.data.frame(dat)) {
		type = "matrix_or_frame";
	} else if (is.vector(dat)) {
		type = "vector";
	} else {
		print("입력하는 인자는 vector, matrix, 또는 data frame 형태여야 합니다.");
		break;
	}
	
	adv_summary_vector = function(temp_dat)
	{
		res01_n			= length(temp_dat)
		res02_min		= summary(temp_dat)[1]
		res03_1stQu	= summary(temp_dat)[2]
		res04_median	= summary(temp_dat)[3]
		res05_mean	= summary(temp_dat)[4]
		res06_3rdQu	= summary(temp_dat)[5]
		res07_max		= summary(temp_dat)[6]
		res08_sd		= sd(temp_dat[is.na(temp_dat)==FALSE])
		res09_NA		= sum(is.na(temp_dat))
		
		temp_res = t(as.matrix(c(	res01_n,			res02_min,			res03_1stQu,
												res04_median,	res05_mean,		res06_3rdQu,
												res07_max,			res08_sd,			res09_NA)))
						
						
		dimnames(temp_res)[[2]] = c(	"Size",		"Min.",			"1st Qu.",
													"Median",	"Mean",			"3rd Qu.",
													"Max.",		"Std. dev.",	"NA's")
		
		return(temp_res)
	}
	
	if (type == "matrix_or_frame") {
		res = apply(dat, 2, adv_summary_vector);
		dimnames(res)[[1]] = c(	"Size",		"Min.",			"1st Qu.",
											"Median",	"Mean",			"3rd Qu.",
											"Max.",		"Std. dev.",	"NA's")
		return(res);
	} else if (type == "vector") {
		return(adv_summary_vector(dat));
	}
}