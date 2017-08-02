#' This class is used to add an ONS time series data to trends_data
#' table in bss.sqlite database. The meta information of the timeseries
#' should first be save in trends_meta. For existing code in trends_meta
#' the constructor only requires code but for new series all the constructor
#' parameters must be supplied.
#' It is also possible to add non-ONS figures by passing a timeseries object
#' to variable x in method \code{add_trends}
#'
tg_ons <- R6::R6Class(

    'tg_ons',
    inherit = tp_utils,

    public = list(

        data_code = NULL,
        data_ts = NULL,
        data_freq = NULL,

        data_desc = NULL,
        data_unit = NULL,
        data_freq_str = 'm',
        data_src = NULL,
        temp_df = '',

        meta_info = NULL,
        is_new = FALSE,
        is_growth = FALSE,
        is_auto = FALSE,
        delay_trends_update = FALSE,

        initialize = function( code, desc = NULL, unit = NULL, src = 'Office for National Statistics' , frq_str = 'm', is_growth = FALSE, is_auto = FALSE, db_name = NULL ){

            super$initialize( db_name )
            self$set_code( code )
            self$check_code_status()

            if( self$is_new ){

                self$set_description( desc )
                self$set_unit( unit )
                self$set_src( src )
                self$set_freq_str( frq_str )

                self$set_growth( is_growth )
                self$set_auto( is_auto )



            }else{

                self$data_desc <- self$meta_info$data_desc[1]
                self$data_unit <- self$meta_info$data_unit[1]
                self$data_src <- self$meta_info$data_src[1]

                #return(self$my_info$data_frq[1])

                self$data_freq_str <- switch(
                    as.character(self$meta_info$data_frq[1]),
                        "12" = "m",
                         "4" = "q",
                         "1" = "y"
                 )

            }


        },

        check_code_status = function(){

            qry<- sprintf( "select * from trends_meta where data_code='%s' limit 1", self$data_code )
            results <- self$run_sql(qry)

            if( nrow( results ) > 0){

                self$meta_info <- results
                self$is_new <- FALSE

            }else{

                self$is_new <- TRUE

            }

            invisible( self )
        },

        set_code = function( value ){

            if( !is.null( value ) || missing( value )){
                self$data_code <- value
            }

            invisible( self )
        },

        set_delay_update = function( value ){

            if( !is.null( value ) || missing( value )){
                self$delay_trends_update <- value
            }

            invisible( self )
        },

        set_unit = function( value ){

            if( !is.null( value ) || missing( value )){
                self$data_unit <- value
            }

            invisible( self )
        },
        set_freq_str = function( value ){

            if( !is.null( value ) || missing( value )){
                self$data_freq_str <- value
            }

            invisible( self )
        },

        set_growth = function( value ){

            if( !is.null( value ) || missing( value )){
                self$is_growth <- value
            } else {
                cat('is_growth is NULL or missing')
            }

            invisible( self )
        },

        set_auto = function( value ){

            if( !is.null( value ) || missing( value )){
                self$is_auto <- value
            } else {
                cat('Unit is NULL or missing')
            }

            invisible( self )
        },

        set_src = function( value ){

            if( !is.null( value ) || missing( value )){
                self$data_src <- value
            }

            invisible( self )
        },

        set_description = function( value ){

            if( !is.null( value ) || missing( value )){
                self$data_desc <- value
            }

            invisible( self )
        },

        get_code = function(){
            return( self$data_code )
        },

        get_growth = function(){
            return( self$is_growth )
        },

        get_auto = function(){
            return( self$is_auto )
        },

        get_description = function(){
            return( self$data_desc )
        },

        get_unit = function(){
            return( self$data_unit )
        },

        get_freq_str = function(){
            return( self$data_freq_str )
        },

        get_src = function(){
            return( self$data_src )
        },

        get_insert_sql = function(yr, mth,dy, data_value){

            sql_base <- "insert into trends_data (yr,mth,dy,data_desc,data_unit,data_src,data_code,data_value) values (%s,%s,%s,'%s','%s','%s','%s', %s);"
            sql <- sprintf( sql_base, yr,mth, dy, self$data_desc, self$data_unit, self$data_src, self$data_code , data_value)
            return(sql)
        },

        get_update_sql = function(yr, mth, data_value){

            sql_base <- "update trends_data set data_value= %s where yr= %s and mth=%s and data_code='%s';"
            sql  <- sprintf( sql_base, data_value, yr, mth, self$data_code)
            return(sql)
        },

        update_data = function(  fx=c('insert','update'), data_points_yrs=0){

            cat("Just entered update_data fxn ...\n")

            if(is.null( self$data_ts) ){
                cat("Cannot proceed with empty time series. Aborting ...")
                return(1)
            }
            cat("checkde ts status ...\n")

            mydata <- self$temp_df <- self$to_df( self$data_ts )
            mydata_n <- nrow(mydata)


            cat("checked number of rows ...\n")
            sql<-NULL
            myfx <- match.arg( fx )

            loop_range <- 1:mydata_n

            if( !(data_points_yrs == 0) ){


                loop_start<- ( mydata_n - data_points_yrs * self$data_freq )

                if(loop_start > 0){

                    loop_range <- loop_start:mydata_n
                }
            }

            for(i in loop_range){

                if( myfx == 'insert' ){

                    sql <- self$get_insert_sql( yr= mydata$yr[i], mth= mydata$mth[i], dy = 1 , data_value=mydata$value[i] )#sql

                }else{

                    sql <- self$get_update_sql( yr=  mydata$yr[i],  mth= mydata$mth[i],  data_value=mydata$value[i])#sql

                }

                result <- self$run_sql(sql)
            }#for


        },


        add_trends_meta = function(){

            if( is.null( self$data_code ) ) { stop( 'Code required') }
            if( is.null( self$data_unit ) ){ stop( 'New code "Unit" required') }
            if( is.null( self$data_freq_str) ) { stop( 'New code "Frequency" (eg. m, q or y) required') }
            if( is.null( self$data_src ) ){ stop( 'New code "Data source" required') }
            if( is.null( self$data_desc ) ) { stop( 'New code "Description" required') }

            self$data_freq <- switch(
                self$data_freq_str,
                 "m" = 12,
                 "q" = 4,
                 "y" = 1
             )

            my_growth <- ifelse( self$is_growth, 1, 0)
            my_auto   <- ifelse( self$is_auto,   1, 0)

            sql_base<- "insert into trends_meta(data_code, data_desc, data_src, data_frq, data_unit, is_growth,is_auto_update) values ('%s','%s','%s',%s,'%s',%s,%s);"
            sql <-       sprintf( sql_base, self$data_code, self$data_desc, self$data_src, self$data_freq,self$data_unit, my_growth, my_auto)

            #self$temp_sql <- sql

            self$run_sql( sql )
            cat(sprintf("Code %s successfully added to trends_meta! \n", self$data_code))
            self$is_new <- FALSE

        },

        add_trends_data = function(data_point_yrs=0, x = NULL){

            if( self$is_new){

              self$add_trends_meta()

            }

            my_ts <- my_data <- NULL

            if(!is.null( x ) && is.ts( x )){

                my_data <- x

            }else{

                my_ts <- onsR2::download(code = self$data_code ) #lite_ons_download(code)


                if(!is.null( my_ts)){

                    cat("Just entered if ...\n")
                    my_data <- switch(

                      self$data_freq_str,

                     'm' = my_ts$m_data,
                     'q' = my_ts$q_data,
                     'y' = my_ts$y_data

                    )
                }else{

                    cat('NULL timeseries. Aborting process');
                    return(NULL)
                }
            }

            cat("selected ts ...\n")

            self$data_ts <- my_data
            self$data_freq <- stats::frequency( my_data )

            my_desc <- self$data_desc
            my_unit <- self$data_unit

            cat("assigned meta ...\n")

            cat("Now starting update ...\n")
            self$update_data( fx='insert', data_points_yrs = data_point_yrs )
            cat( sprintf("%s - %s completed ...\n", self$data_code, self$data_desc) )

            if( self$delay_trends_update == FALSE){
                cat( "Now updating period ...\n" )
                SQ <- storedQry::SQ$new( self$db_name )$set_name("trends_update_periods")$qry_exec()
            }

            cat( "All done folks ...\n" )


        }



    ),
    private = list()
)
