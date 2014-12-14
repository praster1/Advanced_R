packages = c("bnlearn")
if (length(setdiff(packages, rownames(installed.packages()))) > 0)
{
	install.packages(setdiff(packages, rownames(installed.packages())))  
}