#' Interpolate Missing Data Linearly by Date
#'
#' This function takes in a data frame and column names (as strings) and returns a new data frame with interpolated data.
#' The frequency of the interpolated date can be specified
#' Lastly, if the date variable is numeric, it assumes it is a year and switches to the end of the year for a date
#' Can also be useful for cutting data down even if interpolation is note required.
#' @param x A Data Frame with a date variable and a value variable
#' @param date_var the name, as a string, of the date variable
#' @param val_var the name, as a string, of the value variable
#' @param frequency either "quarterly", "monthly" or "daily"
#' @param type either "spline" or "linear"
#' @param cumulative whether the value needs to be summed in order to be interpolated
#'
#' @export
#'

interp_date_data <- function(x, date_var="date", val_var="total", frequency="quarterly",
                             type="linear", cumulative=F){

  #x <- filter(input_df, trust=="Bondex")
  #date_var="year"
  #val_var="payments"
  #type="spline"
  #frequency="quarterly"
  #x <- arpc_assets
  #x <- cashflow
  #type <- "linear"
  #cumulative=T

  #X should be a data frame with a date and a value variable with missing data
  if(nrow(x)<2){return(
    data.frame(date_var=x[[date_var]],val_var=x[[val_var]],"interp"=NA))}

  if(class(x[[date_var]])=="numeric"){
    if(cumulative==T){
    first_date <- as.Date(paste(x[1,date_var]-1, 12, 31,sep="-"), "%Y-%m-%d")
    list_dates <- as.Date(paste(x[[date_var]], 12, 31,sep="-"), "%Y-%m-%d")
    df <- data.frame("x" = c(first_date,list_dates),"y" = c(0,x[[val_var]]))
    names(df) <- c(date_var,val_var)
    x <- df
    }
    if(cumulative==F){
    x[[date_var]] <- as.Date(paste(x[[date_var]], 12, 31,sep="-"), "%Y-%m-%d")
    }
  }

  date <-
    seq(from=min(x[[date_var]]), to = max(x[[date_var]]),
        by="1 day")
  date <- as.data.frame(date)

  names(date) <- date_var

  #create key to know if it was actual or interpolated
  x$interp <- "Actual"

  if(cumulative==T){
    x %<>%
      mutate(y = cumsum(.data[[val_var]]))
    x[[val_var]] <- x$y
    x$y <- NULL
  }

  interp_base <- left_join(date,x)


  ##Linear
  if(type=="linear"){
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

  }}

  ##Spline
  if(type=="spline"){
    beg <- as.numeric(min(x[[date_var]]))
    end <- as.numeric(max(x[[date_var]]))

    #function
    func = splinefun(x=x[[date_var]], y=x[[val_var]], method="natural",  ties = mean)
    pred_period <- interp_base[is.na(interp_base[[val_var]]),date_var]
    n_int <- func(pred_period)
    interp_base[is.na(interp_base[[val_var]]), val_var] <- n_int
  }


  interp_base$interp[is.na(interp_base$interp)] <- "Interpolated"
  #interp_base$trust[is.na(interp_base$trust)] <- interp_base$trust[1]

  #cut down to proper periods (daily, weekly, quarterly)
  if(frequency=="quarterly"){
    final_interp <-
      interp_base %>%
      mutate(quarter = substr(.data[[date_var]],6,13)) %>%
      filter(quarter %in% c("03-31","06-30","09-30","12-31")) %>%
      arrange(.data[[date_var]]) %>%
      select(-quarter)
  }
  if(frequency=="monthly"){
    start <- as.Date(as.yearmon(min(x[[date_var]])),frac = 0)
    end <- as.Date(as.yearmon(max(x[[date_var]])),frac = 1)
    months <- seq(from=start,to=end+1, by="1 month")-1

    final_interp <-
      interp_base %>%
      filter(.data[[date_var]] %in% months) %>%
      arrange(.data[[date_var]])
  }
  if(frequency=="daily"){
    final_interp <- interp_base
  }
  #Get change in cumulative

  if(cumulative==T){
    final_interp[[val_var]] <- final_interp[[val_var]]-lag(x = final_interp[[val_var]],
                                                           n = 1,
                                                           default = 0)}

  return(final_interp)
}
