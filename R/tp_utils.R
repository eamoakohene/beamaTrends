tp_utils <- R6::R6Class(
  "tp_utils",

  public = list(



    initialize = function(){

    }

    ,get_fred_key = function(){
        return('2b51779f15571ec088a3f4b158054d0a')
    }

    ,get_db = function(){
        return(private$global_DB)
    }


    ,get_db_btrends = function(){
        return(private$global_DB)
    }

    ,get_db_indx = function(){
        return(private$global_DB_INDX)
    }

    ,run_sql = function(qry) {
        return(sqldf::sqldf(  DBI::SQL(qry), dbname= private$global_DB ))
    }

    ,run_sql_btrends = function(qry) {
        return(sqldf::sqldf( DBI::SQL(qry), dbname= private$global_DB_BTRENDS ))
    }

    ,run_sql_indx = function(qry) {
        return(sqldf::sqldf(  DBI::SQL(qry ), dbname= private$global_DB_INDX))
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

    }

  )

  ,private = list(

    global_DB_BTRENDS = 'R:/packages/btrends/btrends.sqlite',
    global_DB_INDX = 'R:/packages/bindices/beama_indices.sqlite',
    global_DB = 'R:/shiny/beama/bmonitor/bss.sqlite',

    get_db_con = function(){
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
    ,set_decimal = function(x, k){
      if( !(is.na(x)||missing(x)||is.null(x)) ){
        if(x[1]<200){
          format(round(x, k), nsmall=k,big.mark=",")
        }else {
          format(round(x, 0), nsmall=0,big.mark=",")
        }
      }
    }
    ,en_code =  function(x, from_encoding = "UTF-8", to_encoding = "UTF-8"){
      require(dplyr)

      # names of columns are encoded in specified encoding
      my_names <-
        iconv(names(x), from_encoding, to_encoding)

      # if any column name is NA, leave the names
      # otherwise replace them with new names
      if(any(is.na(my_names))){
        names(x)
      } else {
        names(x) <- my_names
      }

      # get column classes
      x_char_columns <- sapply(x, class)
      # identify character columns
      x_cols <- names(x_char_columns[x_char_columns == "character"])

      # convert all string values in character columns to
      # specified encoding
      x <-
        x %>%
        mutate_each_(funs(iconv(., from_encoding, to_encoding)),
                     x_cols)
      # return x
      return(x)
    }

  )
)
