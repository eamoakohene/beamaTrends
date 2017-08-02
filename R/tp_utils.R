tp_utils <- R6::R6Class(
  "tp_utils",

  public = list(

     brexit_text = NULL
    ,brexit_label = FALSE
    ,db_name = NULL


    ,initialize = function( db_name = NULL){

        self$set_db_name( db_name )

    }

    ,set_db_name = function(value){

        if( !is.null(value) ) {

            self$db_name = value

        }else{

            self$db_name = private$global_DB

        }

        invisible(self)
    }

    ,set_brexit_text = function(x="2016-06-23", y=0, label= "ref", angle = 90, size = 3){


        self$brexit_text <-  data.frame( x = as.Date(x), y = y, label = label, angle = angle, size = size)
        return( self)

    }

    ,get_brexit_text = function(){

        return( self$brexit_text )

    }

   ,set_brexit_label = function(value){
       if(!missing(value) && !is.null(value)&& is.logical(value) ){
           self$brexit_label <- value
       }
       invisible( self )
   }

   ,get_brexit_label = function(){
       return(
           self$brexit_label
       )
   }

    ,get_fred_key = function(){
        return('2b51779f15571ec088a3f4b158054d0a')
    }

    ,get_db = function(){
        return(self$db_name)
    }

    ,get_db_name = function(){
         self$get_db()
    }

    ,get_db_btrends = function(){
        return(private$global_DB_BTRENDS)
    }

    ,get_db_indx = function(){
        return(private$global_DB_INDX)
    }

    ,run_sql = function(qry) {
        return( sqldf::sqldf( DBI::SQL(qry), dbname= self$db_name ))
    }

    ,run_sql_btrends = function(qry) {
        return(sqldf::sqldf( DBI::SQL(qry), dbname= private$global_DB_BTRENDS ))
    }

    ,run_sql_indx = function(qry) {
        return(sqldf::sqldf(  DBI::SQL(qry ), dbname= private$global_DB_INDX))
    }

   ,run_sql_fc = function(qry) {
       return(sqldf::sqldf(  DBI::SQL(qry ), dbname= private$global_DB_FORECAST))
   }


    ,plot_save = function(
        file="glance.png",width=650,height=390,path="W:/reports/latex/images/",ppi=72
    ){
        ggplot2::ggsave(file=paste0(path,file,".png"),height=height/ppi,width=width/ppi,dpi=ppi,units="in")
    }

    ,vec_shift_left = function( x, delta = 1 ){

        if( missing(x) ){ stop( "Please supply x" ) }

        n <- length( x )
        y <- numeric( n )
        loop_start <- 1 + delta
        loop_end <- n
        j_counter <- 1

        for(i in loop_start:loop_end){

            y[j_counter] <- x[i]
            j_counter <- j_counter + 1

        }

        loop_start <-  n - delta + 1
        loop_end <- n


        for(i in loop_start:loop_end){
            y[ j_counter] <- NA
            j_counter <- j_counter + 1
        }

        return(y)
    }
    ,to_proper_case =  function(txt){

        return(
            paste0(
                toupper( substr(txt, 1, 1) ),
                tolower( substr(txt, 2, nchar(txt)) )
            )
        )

    },

    to_df = function( ts_data, ts_name = NULL, rm_na = FALSE ){

        my_data <- ts_data
        #my_start <- stats::start( ts_data )



        if( ! is.ts( my_data )){
            cat("Time series data expected\n")
            cat("You supplied ", storage.mode( my_data ),"\n")

        }

        my_start <- stats::start( ts_data )
        my_frq <- stats::frequency( ts_data )

        # cat("My Start yr  =", my_start[1], '\n')
        # cat("My Start mth =", my_start[2], '\n')
        # cat("My Frequency =", my_frq, '\n')

        #plot(my_data)

        my_df <- NULL


        ###
        #return( my_data )
        ###

        # cat("FRQ = ", my_frq , '\n')

        if( my_frq %in% c(12)){

            my_df <- data.frame( date=zoo::as.Date(zoo::as.yearmon(time( ts_data ))), value=as.matrix( ts_data ))

        }else if(my_frq == 4){

            my_len <- length(ts_data)
            my_start <- stats::start( ts_data )
            my_start_date <- lubridate::make_date( my_start[1], 3 * my_start[2], 1 )
            my_dates <- seq( as.Date( my_start_date ), by='quarter', length.out = my_len )
            my_df <- data.frame( date=my_dates, value = as.matrix( ts_data ) )

        }else if(my_frq == 1){

            my_len <- length(ts_data)
            my_dates <- as.Date(
                sprintf(
                    "%s-%s-%s",
                    my_start[ 1 ]:(my_start[ 1 ] + my_len - 1),
                    1 ,
                    1
                )
            )

            my_df <- data.frame( date=my_dates, value = as.matrix( ts_data ) )
        }

        my_df$yr <- lubridate::year( my_df$date )
        my_df$mth <- lubridate::month( my_df$date )

        if( !is.null( ts_name ) ){
            my_df$name <- ts_name
        }

        if( rm_na ){

            my_df <- dplyr::filter( my_df, !is.na( value ) )
        }

        return( my_df )

    }


  )

  ,private = list(

    global_DB_BTRENDS = 'R:/packages/btrends/btrends.sqlite',
    global_DB_INDX = 'R:/packages/bindices/beama_indices.sqlite',
    global_DB = 'R:/shiny/beama/bmonitor/bss.sqlite',
    global_DB_FORECAST = 'R:/packages/beamaTrends/inst/extdata/beamaTrends.sqlite',

    get_db_con = function(){
      return(
        DBI::dbConnect(RSQLite::SQLite(), dbname= private$global_DB )
      )
    }

    ,get_db_con_indx = function(){
        return(
            DBI::dbConnect(RSQLite::SQLite(), dbname= private$global_DB_INDX )
        )
    }

    ,get_db_con_btrends = function(){
        return(
            DBI::dbConnect(RSQLite::SQLite(), dbname= private$global_DB_BTRENDS )
        )
    }



    ,str_pos = function(x,pattern=","){
      my_str <- gregexpr(pattern =pattern,x)
      return(my_str[[1]][1])
    }

    ,split_str = function(q="EUR,GBP,USD"){
      my_str <- gsub(",","','",q)
      return (
        paste0("('",my_str,"')")
      )
    }

    ,split_text = function(q="EUR,GBP,USD"){
      private$split_str(q)
    }

    ,set_decimal = function(x, k, cut = 200){
        beamaUtils::set_decimal(x = x, k = k, cut = cut)

    }

    ,en_code =  function(x, from_encoding = "UTF-8", to_encoding = "UTF-8"){
      require(dplyr)

      # names of columns are encoded in specified encoding
      my_names <-   iconv(names(x), from_encoding, to_encoding)

      # if any column name is NA, leave the names otherwise replace them with new names
      if(any(is.na(my_names))){
        names(x)
      } else {
        names(x) <- my_names
      }

      # get column classes
      x_char_columns <- sapply(x, class)

      # identify character columns
      x_cols <- names( x_char_columns[ x_char_columns == "character"] )

      # convert all string values in character columns to specified encoding
      x <-  x %>% dplyr::mutate_each_( dplyr::funs( iconv(., from_encoding, to_encoding)),    x_cols)

      # return x
      return(x)
    }

  )
)

##NON CLASS FXNS ###
tpu.update_self <- function(){
    install.packages("R:/packages/beamaTrends_0.1.zip", repos=NULL)
}

tpu.get_db <- function(){
    tp_utils$new()$get_db()
}
