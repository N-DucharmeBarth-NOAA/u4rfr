

#' A wrapper function which treats missing values (removes, imuptes, etc.) found in a data set.
#' This function is set-up to handle data geared towards univariate analysis (e.g. a single response predicted by multiple covariates).
#' 
#' @param data_df A class data.frame object to treat any missing values (e.g. NAs)
#' @param na_thresh Specify a proportion between 0 and 1. Any covariate with the proportion of missing data greater than this threshold value will simply be excluded.
#' @param treatment_type Specifies how the missing values are treated.
#' \describe{
#'		\item{'omit'}{Missing values are omitted from the data set.}
#'		\item{'central_tendency'}{Missing values are replaced with the median and modal values for continuous and discrete covariates, respectively.}
#'      \item{class(list)}{A list either with entries (type = "resample", "random_seed" = i) or (type = "impute", ntree = n). For type resample, missing values are replaced with randomly re-sampled observations where i sets the random seed. For type impute, the missRanger package is used to impute missing values using a random forest machine learning algorithm.}
#' }
#' @param id_var The column name of 'data_df' containing the observation id or row id.
#' @param response_var The column name of 'data_df' containing the response variable. This column is not treated for missing values. 
#' @param covariate_vec The column names of covariates to treat for missing values.
#' @param random_seed Sets a random seed.
#' @param verbose Defaults to FALSE. Print the report?
#' @return A named list is returned. 
#' \describe{
#'		\item{data}{A class data.frame object without any missing values.}
#'		\item{report}{A text report of the processing that occurred.}
#' }
#' @export
#' @importFrom data.table as.data.table
#' @importFrom magrittr %>%
#' @importFrom missRanger missRanger
#' @importFrom randomForest na.roughfix
#' 

# Nicholas Ducharme-Barth
# 09/02/2022
# NA treatment wrapper function
# Copyright (C) 2022  Nicholas Ducharme-Barth
# You should have received a copy of the GNU General Public License along with this program.  If not, see <https://www.gnu.org/licenses/>.

#_____________________________________________________________________________________________________________________________
# define wrapper function - na_treatment
# args: data_df, na_thresh, treatment_type (omit, quick, list(random_seed, ntree, mtry)), response_var, covariate_vec

	na_treatment = function(data_df, na_thresh, treatment_type, id_var, response_var, covariate_vec,random_seed,verbose=FALSE)
	{
		set.seed(random_seed)
		A = proc.time()
		# define initial data
			row_id = data_df[,id_var]
			keep_columns = c(response_var,covariate_vec)
			data = data.table::as.data.table(data_df) %>% .[,..keep_columns] %>% setnames(.,response_var,"response") %>%
				   .[!is.na(response)]
			keep_columns[1] = "response"

		# trim by na_thresh
			isNA_expand = function(x){is.na(x)|as.character(trimws(x))==""}
			na_rate = as.vector(as.matrix(data[,lapply(.SD,function(x)mean(isNA_expand(x)))]))
			names(na_rate) = keep_columns
			update_keep_columns = keep_columns[which(keep_columns %in% names(na_rate[which(na_rate<=na_thresh)]))]
			data_update = as.data.frame(data[,..update_keep_columns])
			covar_removed = setdiff(keep_columns,update_keep_columns)

		# handle na's
			if(is.list(treatment_type))
			{
				if(treatment_type[["type"]] == "resample")
				{
					data_out = missRanger::missRanger(data_update,. - response ~ 1,seed=treatment_type[["random_seed"]])
					data_out$ID = row_id
					data_out = data_out[,c("ID",update_keep_columns)]
					colnames(data_out)[1] = id_var
					method = treatment_type[["type"]]
				} else if (treatment_type[["type"]] == "impute"){
					data_out = missRanger::missRanger(data_update,. - response ~ . - response, num.trees = treatment_type[["ntree"]],mtry = min(c(floor(sqrt(length(update_keep_columns)-1)),treatment_type[["mtry"]])),seed=treatment_type[["random_seed"]])
					data_out$ID = row_id
					data_out = data_out[,c("ID",update_keep_columns)]
					colnames(data_out)[1] = id_var
					method = treatment_type[["type"]]
				}
			} else {
				if(treatment_type == "omit")
				{
					data_update$ID = row_id
					data_update = data_update[,c("ID",update_keep_columns)]
					colnames(data_update)[1] = id_var
					data_out = na.omit(data_update)
					method = treatment_type
				} else if(treatment_type == "central_tendency"){
					if(sum(sapply(data_update, class) == "character")>0)
					{
						# change character to factor
							chr_col = which(sapply(data_update, class) == "character")
							for(i in 1:length(chr_col))
							{
								data_update[,chr_col[i]] = as.factor(data_update[,chr_col[i]])
							}
							data_out = randomForest::na.roughfix(data_update)
						# change factor back to character
							for(i in 1:length(chr_col))
							{
								data_out[,chr_col[i]] = as.character(data_out[,chr_col[i]])
							}
					} else {
						data_out = randomForest::na.roughfix(data_update)
					}
					data_out$ID = row_id
					data_out = data_out[,c("ID",update_keep_columns)]
					colnames(data_out)[1] = id_var
					method = treatment_type
				} 
			}
			B = proc.time()

		# reporting
			report = c("Missing value report",
					   "",
					   paste0("The original data contained ",nrow(data_df)," rows and ",ncol(data_df)," columns with ",sum(is.na(data_df))," missing values."),
					   paste0(length(covar_removed)," covariates were removed for failing to meet the missing value threshold of ",na_thresh*100,"%:"),
					   paste0("'",covar_removed,collapse="', '","'"),
					   "",
					   paste0("The remaining missing values were dealt with using the '",method,"'' approach."),
					   paste0("It took ",round((B-A)[3]/60,digits=2)," minutes to handle the remaining ",sum(is.na(data_update))," missing values."),
					   "",
					   paste0("The final data contains ",nrow(data_out)," rows and ",ncol(data_out)," columns."),
					   paste0("The following covariates were retained:"),
					   paste0("'",update_keep_columns[-1],collapse="', '","'"))

		# return report and output data.frame
			if(verbose)
			{
				print(report)
			}
			gc()
			return(list(data=data_out,report=report))
	}