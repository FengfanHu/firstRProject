library(palmerpenguins)

max_cor_var<-function(df,col_name){
  # function to determine the variable with maximal correlation
  v_col<-df%>%select(all_of(col_name)) # extract variable based on col_name
  df_num<- df%>%
    select_if(is.numeric)%>%
    select(-all_of(col_name))
  # select all numeric variables excluding col_name
  correlations<-unlist(map(df_num, function(x){cor(x,v_col,use="complete.obs")}))
  # compute correlations with all other numeric variables
  max_abs_cor_var<-names(which(abs(correlations)==max(abs(correlations)))) # extract the variable name 
  cor<-as.double(correlations[max_abs_cor_var])
  # compute the correlation
  
  return(data.frame(var_name=max_abs_cor_var,cor=cor))
  # return dataframe
}


top_correlates_by_var <- function(df) {
  numeric_colNames <- list(df %>% select_if(is.numeric) %>% colnames)
  return(unlist(numeric_cols) %>% map(~max_cor_var(df, .x)))
}
numeric_colNames <- list(penguins %>% select_if(is.numeric) %>% colnames)
res <- map(unlist(numeric_colNames), ~max_cor_var(penguins, .x))

