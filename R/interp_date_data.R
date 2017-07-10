#' Interpolate Missing Data Linearly by Date
#'
#' This function takes in a data frame and column names (as strings) and returns a new data frame with interpolated data.
#' The frequency of the interpolated date can be specified
#' Can also be useful for cutting data down even if interpolation is note required.
#' @param x A Data Frame with a date variable and a value variable
#' @param date_var the name, as a string, of the date variable
#' @param val_var the name, as a string, of the value variable
#' @param type either "quarterly", "monthly" or "daily" 
#'
#' @export
#'

interp_date_data <- function(x, date_var="date", val_var="total", type="quarterly"){

    #x <- filter(df, trust=="Bondex")
  #date_var="datesomething" 
  #val_var="total"
  
  #X should be a data frame with a date and a value variable with missing data
  if(nrow(x)<2){return(
    data.frame(date_var=x[[date_var]],val_var=x[[val_var]],"interp"=NA))}
  
  date <- 
    seq(from=min(x[[date_var]]), to = max(x[[date_var]]), 
        by="1 day")
  date <- as.data.frame(date)
  
  names(date) <- date_var
  
  #create key to know if it was actual or interpolated
  x$interp <- "Actual"
  
  interp_base <- left_join(date,x)
  
  na_s <- which(is.na(interp_base[[val_var]]), arr.ind=TRUE)
  not_na_s <- which(!is.na(interp_base[[val_var]]), arr.ind=TRUE)
  
  for(i in na_s){
    #i <- 4
    #Get Previous Non_NA and Next Non_NA and interpolate
    prev_index <- not_na_s[not_na_s<i][length(not_na_s[not_na_s<i])]
    prev_total <- interp_base[prev_index,val_var]
    prev_date <- interp_base[prev_index, date_var]
    
    next_index <- not_na_s[not_na_s>i][1]
    next_total <- interp_base[next_index,val_var]
    next_date <- interp_base[next_index, date_var]
    
    interp_base[i,val_var] <- 
      (prev_total+((next_total-prev_total)/as.numeric((next_date-prev_date)))*as.numeric(interp_base[i,date_var]-prev_date))
    
  }
  interp_base$interp[is.na(interp_base$interp)] <- "Interpolated"
  #interp_base$trust[is.na(interp_base$trust)] <- interp_base$trust[1]
  
  #cut down to proper periods (daily, weekly, quarterly)
  if(type=="quarterly"){
    final_interp <- 
      interp_base %>%
      mutate(quarter = substr(.data[[date_var]],6,13)) %>% 
      filter(quarter %in% c("03-31","06-30","09-30","12-31")) %>% 
      arrange(.data[[date_var]]) %>% 
      select(-quarter)
  }
  if(type=="monthly"){
    start <- as.Date(as.yearmon(min(x[[date_var]])),frac = 0)
    end <- as.Date(as.yearmon(max(x[[date_var]])),frac = 1)
    months <- seq(from=start,to=end+1, by="1 month")-1
    
    final_interp <- 
      interp_base %>%
      filter(.data[[date_var]] %in% months) %>% 
      arrange(.data[[date_var]])
  }
  if(type=="daily"){
    final_interp <- interp_base  
  }
  
  return(final_interp)
}
