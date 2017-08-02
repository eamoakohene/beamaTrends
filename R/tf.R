
tc <- R6::R6Class(
    'tc',
    inherit = tg,

    public = list(

         y1 = NULL
        ,y2 = NULL
        ,m1 = NULL
        ,m2 = NULL
        ,fc_method = NULL
        ,fc_methods = c('arima','holtw','meanf','rwf','thetaf','holt','x12','mapa','bsts','splinef','ets','avg')
        ,fc_methods_packed = 'x12,arima,holtw,rwf,thetaf,holt,mapa,bsts,meanf,splinef,ets'
        ,fc_ahead = 2
        ,fc_ahead_factor = 12
        ,fc_interval = c(0.95)
        ,fc_data = NULL
        ,fc_model = NULL
        ,fc_upper = NULL
        ,fc_lower = NULL
        ,fc_mean = NULL
        ,fc_object = NULL
        ,fc_issue = NULL
        ,fc_start = NULL

        ,initialize = function(

            x, x_name = NULL, x_start=NULL, x_frq = NULL,
            db_limit = list(yr=NULL, mth=12 ),  db_name = NULL,
            y1 = NULL, y2 = NULL, m1 = NULL, m2 = NULL
        ){

            super$initialize(x = x, x_start= x_start, x_frq = x_frq, db_limit = db_limit, db_name = db_name  )
            self$set_ahead_factor( frequency( self$data_ts ))

            self$set_yr( c(y1, y2) )
            self$set_mth( c(m1, m2) )

            invisible(self)
        }


        ,set_method = function(value){
            if(!missing(value)  & value %in% self$fc_methods){

                self$fc_method <- value

            }
            invisible(self)
        }
        ,set_method_packed = function(value){

            if(!missing(value) ){
                self$fc_methods_packed <- value

            }
            invisible(self)
        }


        ,set_ahead = function(value){
            if(!missing(value) ){
                self$fc_ahead <- value

            }
            invisible(self)
        }

        ,set_ahead_factor = function(value){

            if(!missing(value) && !is.null( value )){

                self$fc_ahead_factor <- value

            }
            invisible(self)
        }
        ,set_interval = function(value){
            if(!missing(value) ){
                self$fc_interval <- value

            }
            invisible(self)
        }

        ,set_y1 = function(value = 2017){
            if(!missing(value) ){
                self$y1 <- value

            }else{

                self$y1 <- lubridate::year(Sys.Date())
            }

            invisible(self)
        }

        ,set_y2 = function(value = 2036){

            if(!missing(value) ){

                self$y2 <- value

            }else{
                self$y2 <- 2036
            }
            invisible(self)
        }

        ,set_yr = function( value ){

            if( length( value ) == 2){

                self$set_y1( value[ 1 ])$set_y2( value[ 2 ])

            }

            invisible( self )
        }

        ,set_m1 = function(value = 1){
            if(!missing(value) ){
                self$m1 <- value

            }else{
                self$m1 <- 1
            }
            invisible(self)
        }

        ,set_m2 = function(value = 12){
            if(!missing(value) ){
                self$m2 <- value

            }else{
                self$m2 <- 12
            }
            invisible(self)
        }

        ,set_mth = function( value ){

            if( length( value ) == 2){

                self$set_m1( value[ 1 ])$set_m2( value[ 2 ])

            }

            invisible( self )
        }

        ,get_fc_start = function(){

            if( !is.null(self$data_ts) ){

                my_end <- stats::end( self$data_ts )
                my_yr <- my_end[ 1 ]
                my_mth <- my_end[ 2 ]

                if(my_mth == 12){
                    my_yr <- my_yr + 1
                    my_mth <- 1
                } else {
                    my_mth <- my_mth + 1
                }

                self$fc_start <- c(my_yr, my_mth)
                return( self$fc_start)

            }else{
                return(NULL)
            }

        }

        ,get_ts = function(){

            return(self$data_ts)
        }


        ,set_properties = function(){


            if(!is.null(self$fc_object)){


                if( self$fc_method == 'mapa') {

                    my_start <- self$get_fc_start()
                    my_mean <- self$fc_object$outfor
                    names(my_mean) <- NULL
                    self$fc_mean  <- ts(  my_mean  , start=my_start,frequency = self$data_freq )
                    self$fc_upper <- ts( unique( self$fc_object$PI[ 1,] ), start=my_start,frequency = self$data_freq )
                    self$fc_lower <- ts( unique( self$fc_object$PI[2,]  ), start=my_start,frequency = self$data_freq )

                }else if ( self$fc_method == 'bsts'){

                    my_start <- self$get_fc_start()
                    self$fc_mean <- ts( self$fc_object$mean, start = my_start, frequency = self$data_freq )
                    self$fc_upper <- ts( self$fc_object$interval[2,], start=my_start,frequency = self$data_freq )
                    self$fc_lower <- ts( self$fc_object$interval[1,], start=my_start,frequency = self$data_freq )

                }else{

                    self$fc_mean <- self$fc_object$mean
                    self$fc_upper <- self$fc_object$upper[,1]
                    self$fc_lower <- self$fc_object$lower[,1]

                }



                self$fc_data <- cbind(

                    lower = self$fc_lower,  fc=self$fc_mean,  upper=self$fc_upper
                )

            }

            invisible(self)

        }

        ,fc_arima = function(){

            cat("entering fc_arima \n")

            self$fc_method <- 'arima'
            self$fc_object <- forecast::forecast(
                forecast::auto.arima( self$get_ts() ), h = self$fc_ahead * self$data_freq,level = self$fc_interval
            )
            cat("finished calculating arima object \n")
            self$set_properties()

            return(self$fc_data)
        }

        ,fc_holtw = function(){

            cat("entering fc_holtw \n")

            self$fc_method <- 'holtw'
            self$fc_object <- forecast::forecast(
                stats::HoltWinters( self$get_ts(),gamma = FALSE ), h = self$fc_ahead * self$data_freq, level = self$fc_interval
            )
            self$set_properties()

            return(self$fc_data)
        }

        ,fc_holt = function(){

            cat("entering fc_holt \n")

            self$fc_method <- 'holt'
            self$fc_object <-  forecast::holt(
                self$get_ts(), h = self$fc_ahead * self$data_freq, level = self$fc_interval
            )
            self$set_properties()

            return(self$fc_data)
        }

        ,fc_rwf = function(){

            cat("entering fc_rwf \n")

            self$fc_method <- 'rwf'
            self$fc_object <-  forecast::rwf(
                self$get_ts(), h = self$fc_ahead * self$data_freq, level = self$fc_interval
            )
            self$set_properties()

            return(self$fc_data)
        }

        ,fc_meanf = function(){

            cat("entering fc_meanf \n")

            self$fc_method <- 'meanf'
            self$fc_object <-  forecast::meanf(
                self$get_ts(), h = self$fc_ahead * self$data_freq, level = self$fc_interval
            )
            self$set_properties()

            return(self$fc_data)
        }

        ,fc_thetaf = function(){

            cat("entering fc_thetaf \n")

            self$fc_method <- 'thetaf'
            self$fc_object <-  forecast::thetaf(
                self$get_ts(), h = self$fc_ahead * self$data_freq, level = self$fc_interval
            )
            self$set_properties()

            return(self$fc_data)

        }
        ,fc_splinef = function(){

            cat("entering fc_splinef \n")

            self$fc_method <- 'splinef'
            self$fc_object <-  forecast::splinef(
                self$get_ts(), h = self$fc_ahead * self$data_freq, level = self$fc_interval
            )
            self$set_properties()

            return(self$fc_data)

        }

        ,fc_ets = function(){

            cat("entering fc_ets \n")

            self$fc_method <- 'ets'
            my_ets <- forecast::ets( self$get_ts() )

            self$fc_object <-  forecast::forecast.ets(
                my_ets, h = self$fc_ahead * self$data_freq, level = self$fc_interval
            )
            self$set_properties()

            return(self$fc_data)

        }

        ,fc_mapa = function(){

            cat("entering fc_mapa \n")

            self$fc_method <- 'mapa'
            self$fc_object <-  MAPA::mapa(
                self$get_ts(), fh = self$fc_ahead * self$data_freq, conf.lvl = self$fc_interval, outplot=0
            )
            self$set_properties()

            return(self$fc_data)
        }

        ,fc_bsts = function(){

            cat("entering fc_bsts \n")

            require(pipeR)

            self$fc_method <- 'bsts'
            my_index <- self$get_ts()
            self$fc_object <- my_index %>>%
            { bsts::AddLocalLinearTrend( list(), .)} %>>%
                bsts::AddSeasonal( my_index , nseasons=frequency( my_index ) ) %>>%
                { bsts::bsts( my_index, state.specification = ., niter = 1000) } %>>%
                predict( horizon = self$fc_ahead * self$data_freq, burn=100)


            self$set_properties()

            return(self$fc_data)

        }

        ,get_data = function(){

            return( self$fc_data )
        }

        ,get_db_data = function(  methods = c('avg'),ord=TRUE){

            sq <- storedQry::SQ$new( db = private$global_DB_FORECAST )$set_name( 'fc_get_data')
            my_data <- sq$set_params(

                list(
                    `@i_dt1` = beamaUtils::ddays( self$y1, self$m1 ),
                    `@i_dt2` = beamaUtils::ddays( self$y2, self$m2 ),
                    `@i_method` = self$fc_method,
                    `@i_type` = self$fc_type,
                    `@i_code` = self$fc_name
                )

            )$qry_exec()



            my_data <- tidyr::spread(my_data,fc_type,value)
            names(my_data) <- c('yr','mth','code','method','forecast','lower','upper')

            if(ord){
                my_data <- data.frame(

                    yr = my_data$yr,
                    mth= my_data$mth,
                    code = my_data$code,
                    method = my_data$method,

                    lwr= my_data$lower,
                    fc= my_data$forecast,
                    upr= my_data$upper
                )
            }

            return(my_data)
        }

        ,exec_sql = function(qry){

            return (self$run_sql(qry) )
        }


    ),#public


    private = list(

    )#private

)

tf<- R6::R6Class(
    'tf'
    ,inherit = tc
    ,public = list(

        index_title = "MyData",

        x12_spec_filename = "MyData",
        x12_exe_path = "W:/reports/forecast/winx13/x13as/x13as.exe",
        x12_output_dir = "W:/reports/forecast/graphics/",
        x12_is_easter = TRUE,
        x12_model = '',


        fc_only = TRUE

        ,set_title = function (value){
            if(!missing(value)){
                self$index_title <- value
                self$x12_spec_filename <- value
                invisible(self)
            }
        }

        ,clear_temp = function(){

            temp <- base::dir(
                self$x12_output_dir, pattern = paste0(self$x12_spec_filename,".*"), full.names = TRUE
            )
            if( length(temp) > 0) {
                base::file.remove(temp)
            }
            cat('Temporary files cleared!')
        }

        ,set_dir_formatted = function(){

            win_separator <- tolower(Sys.info()['sysname'])=="windows"
            names(win_separator) <- NULL

            if(win_separator){

                self$x12_output_dir <- gsub( "/", "\\\\", self$x12_output_dir)

            }else{

                self$x12_output_dir <- gsub( "\\", "/", self$x12_output_dir)

            }
            invisible(self)
        }

        ,get_ts_formatted = function(){

            ROW_LIMIT = 20

            abc <- self$get_ts()
            ts_start <- start( abc)
            ts_end <- end( abc )

            get_year <- function(yr){ window( abc , start=c(yr,1), end = c(yr,frequency(abc)) )}

            ts_rows <- vector()
            span <-  ts_start[2] - ts_start[1]
            start_row <- ts_start[1]

            if ( span > ROW_LIMIT){
                start_row <- ts_end[1]- ROW_LIMIT
                ts_start[1] <- start_row
            }

            for(yr in start_row : ts_end[1]){
                ts_rows[length(ts_rows)+1] <- paste( c( get_year(yr) ),collapse=" ")
            }

            return(ts_rows)
        }

        ,get_model = function(){

            return(

                list(
                    transform="Yes",
                    model=c(1,1,1,1,0,0),
                    txt='automdl{}'
                )
            )
        }

        ,get_spc = function(){

            ts_rows <- self$get_ts_formatted()
            ts_forecast <- self$fc_ahead * self$data_freq
            ts_start <- start( self$data_ts )
            ts_end <- end( self$data_ts )
            ts_freq <- frequency( self$data_ts )


            spc <- base::vector()
            spc[length(spc)+1] <- paste( "series { \n title = \"",self$index_title,"\" \n start = ", paste(ts_start[1],ts_start[2],sep="."), "\n  period=",ts_freq," \n")

            spc[length(spc)+1] <- "data=( \n"

            for(i in 1:length(ts_rows)){
                spc[length(spc)+1] <- paste0( ts_rows[i],"\n")
            }

            spc[length(spc)+1] <- ")\n}\n"

            spc[length(spc)+1] <- self$get_model()$txt


            spc[length(spc)+1] <- paste("\n forecast { maxlead=",ts_forecast," }\n",sep="")

            spc[length(spc)+1] <- switch(

                self$get_model()$transform ,
                "No" = "transform{} \n",
                "transform{ function=auto } \n"
            )

            if( self$x12_is_easter ){
                spc[length(spc)+1] <- "regression{ aictest=(td easter) } \n"
            }

            spc[length(spc)+1] <- "outlier{ } \n"
            spc[length(spc)+1] <- "slidingspans{ } \n"
            spc[length(spc)+1] <- "history{estimates=(sadj sadjchng)} \n"
            spc[length(spc)+1] <- "x11 { }"

            return(spc)
        }

        ,save_spc = function(){

            file_spc <- paste0(
                self$x12_output_dir, paste( self$x12_spec_filename,"spc",sep=".")
            )

            spc_conn <- base::file(file_spc)
            base::writeLines(self$get_spc(),spc_conn)
            base::close(spc_conn)
            invisible(self)
        }

        ,get_batch_file = function(){

            x12_file <-  paste0( self$x12_output_dir, self$x12_spec_filename )
            x12_bat <-  vector()
            x12_bat[ length( x12_bat ) + 1 ] <- paste( self$x12_exe_path,x12_file, " -g ", self$x12_output_dir,sep=" ")
            file_bat <- paste0( self$x12_output_dir, paste(self$x12_spec_filename, "bat", sep= "."))
            bat_conn <- base::file( file_bat )
            base::writeLines( x12_bat, bat_conn)
            base::close( bat_conn )
            return(file_bat)

        }

        ,run_batch_file = function(){

            cur_dir <- getwd()
            setwd( self$x12_output_dir )
            base::system2( self$get_batch_file() )
            setwd( cur_dir )
            invisible( self )
        }

        ,fc_x12 = function(){

            cat("entering fc_x12 \n")

            self$set_dir_formatted()
            self$set_title( self$data_code)
            self$save_spc()
            self$run_batch_file()

            table_list <- vector("list",4)
            tables_names <- c("lower","fc","upper")

            # read forecast figures
            save_cur_dir <- getwd()

            setwd( self$x12_output_dir )
            my_files <- paste0( self$x12_spec_filename, ".fct" )
            tables_file <- paste0( self$x12_output_dir, my_files )

            ## test-begings ##
            #return( tables_file)
            ## test-ends ##

            try(  series <- read.table(tables_file, as.is = T, skip = 2),silent=TRUE)
            setwd(save_cur_dir)

            if(class(series) == "try-error") {stop("Error reading forecast data")}

            dt <- series[,1]
            begin <- c( dt[1] %/% 100, dt[1] %% 100)
            fq <- max( dt%%100 )

            my_lower <- ts(data = series[,3], start = begin, frequency = fq)
            my_fc <- ts(data = series[,2], start = begin, frequency = fq)
            my_upper <- ts(data = series[,4], start = begin, frequency = fq)


            self$fc_data <- cbind(
                lower = my_lower,  fc= my_fc,  upper= my_upper
            )

            return(self$fc_data)

        }

        ,get_fc = function( x = 'arima'){

            my_df <- NULL
            my_fc <- switch(
                x,
                'arima' = self$fc_arima(),
                'holt' = self$fc_holt(),
                'holtw' = self$fc_holtw(),
                'rwf' = self$fc_rwf(),
                'meanf' = self$fc_meanf(),
                'thetaf' = self$fc_thetaf(),
                'mapa' = self$fc_mapa(),
                'bsts' = self$fc_bsts(),
                'x12' = self$fc_x12(),
                'splinef' =self$fc_splinef(),
                'ets' = self$fc_ets()
            )

            if( !is.null( my_fc)){

                a <-  my_fc [ ,3]

                my_lw  <- beamaUtils::ts_to_df( my_fc[ , 1] )
                my_fc  <- beamaUtils::ts_to_df( my_fc[ , 2] )
                my_upp <- beamaUtils::ts_to_df( a )



                my_lower <- data.frame(
                    yr        = my_lw$yr,
                    mth       = my_lw$mth,
                    fc_method = x,
                    fc_type   = 'lower',
                    value     = my_lw$value
                )

                my_upper <- data.frame(
                    yr        = my_upp$yr,
                    mth       = my_upp$mth,
                    fc_method = x,
                    fc_type   = 'upper',
                    value     = my_upp$value
                )

                my_forecast <- data.frame(
                    yr        = my_fc$yr,
                    mth       = my_fc$mth,
                    fc_method = x,
                    fc_type   = 'forecast',
                    value     = my_fc$value
                )

                my_df <- rbind( my_lower, my_forecast)
                my_df <- rbind(my_df, my_upper)
            }

            return( my_df)

        }

        ,get_fcs = function( is_spread = TRUE, avg_only = FALSE, avg_excl = 'bsts,holtw,holt', avg_join = NULL){

            ex_len <- 0
            mt_len <- 0
            my_mt <- NULL
            my_ex <- NULL

            if( !is.null( avg_excl )){

                if(nchar( avg_excl ) > 0 ){
                    my_ex <- strsplit( avg_excl, ',')[[1]]
                    ex_len <- length( my_ex )
                }

            }

            if(! is.null( self$fc_methods_packed )){

                if(nchar( self$fc_methods_packed ) > 0){

                    my_mt <- strsplit( self$fc_methods_packed,',')[[1]]
                    mt_len <- length( my_mt )
                }

            }


            if( mt_len == 0){ return( NULL )}

            cat( sprintf("Adding %s forecast ...\n", my_mt[ 1 ] ))
            dfs <- self$get_fc( my_mt[ 1 ])


            if( mt_len > 1){

                for( i in 2:length( my_mt )){

                    cat( sprintf("Adding %s forecast ...\n", my_mt[ i ]) )
                    dfs <- rbind( dfs, self$get_fc( my_mt[ i ]))

                }

                my_sql <- NULL

                if( ex_len > 0){

                    my_sql <- sprintf(
                        "select yr,mth, 'avg' as fc_method,fc_type, avg( value) as value from dfs where fc_method not in %s group by yr,mth,fc_type order by fc_type,yr,mth",
                        beamaUtils::split_str( avg_excl )
                    )

                }else{

                    my_sql <- "select yr,mth, 'avg' as fc_method,fc_type, avg( value) as value from dfs group by yr,mth,fc_type order by fc_type,yr,mth"

                }

                #return( my_sql )

                my_avg <- sqldf::sqldf( my_sql )

                if( avg_only ){

                    dfs <- my_avg

                }else{

                    cat( sprintf("Adding %s forecast ...\n","AVERAGE")  )
                    dfs <-  rbind(dfs, my_avg)

                }
            }

            if( is_spread){

                my_spread <- tidyr::spread( dfs, key = fc_type, value = value )

                if( is.null(avg_join) ){

                    return(

                        my_spread
                    )

                }else{

                    actual_data <- beamaUtils::ts_to_df( avg_join )

                    my_actual <- data.frame(
                           yr  = actual_data$yr,
                           mth = actual_data$mth,
                        actual = actual_data$value
                    )

                    my_spread <- dplyr::inner_join( my_actual, my_spread, by = c('yr','mth'))
                    my_spread$diff <- my_spread$forecast - my_spread$actual

                    return( my_spread )
                }

            }else{

                if( ! is.null(avg_join) ){

                    actual_data <- beamaUtils::ts_to_df( avg_join )

                    #return(actual_data)

                    for( i in 1:mt_len){

                        my_actual_df <- data.frame(
                            yr        = actual_data$yr,
                            mth       = actual_data$mth,
                            fc_method = my_mt[ i ],
                            fc_type   = 'actual',
                            value     = actual_data$value
                        )
                        dfs <- rbind( dfs, my_actual_df)
                    }#for

                    #bind average

                    my_actual_df <- data.frame(
                        yr        = actual_data$yr,
                        mth       = actual_data$mth,
                        fc_method = 'avg',
                        fc_type   = 'actual',
                        value     = actual_data$value
                    )

                    dfs <- rbind( dfs, my_actual_df)

                }#if

                return( dfs)

            }

          NULL


        }

        ,recalc_forecast_avg = function(fco, avg_excl = NULL, is_spread = FALSE, avg_only = FALSE){

            my_fco <- fco

            if( is.null(fco) ){ return( NULL )}
            if( is.null( avg_excl )){

                if(avg_only){

                    my_fco <-  dplyr::filter( fco, tolower(fc_method) == 'avg' )

                    if(is_spread){

                        my_fco <- tidyr::spread( my_fco, key = fc_type, value = value )
                    }

                    return(
                        dplyr::filter( my_fco )
                    )

                }else{

                    if(is_spread){

                        my_fco <- tidyr::spread( my_fco, key = fc_type, value = value )
                    }


                    return( my_fco )
                }
            }

            ex_len <- 0
            my_ex <- NULL
            my_dfs <- NULL

            if( !is.null( avg_excl )){

                if(nchar( avg_excl ) > 0 ){
                    my_ex <- strsplit( avg_excl, ',')[[1]]
                    ex_len <- length( my_ex )
                }

            }


            my_fco <- dplyr::filter( fco, !(tolower(fc_method) == 'avg') )
            my_fco_avg_acutal <- dplyr::filter(fco, tolower(fc_method) == 'avg', tolower(fc_type) == 'actual' )

            my_sql <- NULL

            if( ex_len > 0){

                my_sql <- sprintf(
                    "select yr,mth, 'avg' as fc_method,fc_type, avg( value) as value from my_fco where (fc_method not in %s) and (fc_type not in ('actual')) group by yr,mth,fc_type order by fc_type,yr,mth",
                    beamaUtils::split_str( avg_excl )
                )

            }else{

                my_sql <- "select yr,mth, 'avg' as fc_method,fc_type, avg( value) as value from my_fco where fc_type not in ('actual') group by yr,mth,fc_type order by fc_type,yr,mth"

            }

            my_avg <- sqldf::sqldf( my_sql )

            if( avg_only ){

                my_fco <- my_avg

            }else{

                cat( sprintf("Adding %s forecast ...\n","AVERAGE")  )
                my_fco <-  rbind(fco, my_avg)

            }

            if( nrow(my_fco_avg_acutal) > 0 ){

                my_fco <- rbind( my_fco, my_fco_avg_acutal)

            }

            if( is_spread) {

                my_spread <- tidyr::spread( my_fco, key = fc_type, value = value )
                return(  my_spread )

            }else{

                return( my_fco )
            }


        }

        ,plot_fcs = function(
            avg_only = FALSE, avg_excl = NULL, plot_excl = NULL, avg_join = NULL,
            strip_col = beamaColours::get_grayblue(), strip_fcol = 'white', is_themed = TRUE,
            strip_txt_size = 12, title_font_size = 14,
            fcs_object = NULL, excl_limits = FALSE
        ){

            require(ggplot2)
            require(ggthemes)

           my_fc <- NULL

           if( !is.null( fcs_object)){

                my_fc <- fcs_object

           }else{

                my_fc <- self$get_fcs( is_spread = F, avg_only = avg_only, avg_excl = avg_excl, avg_join = avg_join )
           }

           if( !is.null( plot_excl )){

               my_plot_excl <- strsplit( plot_excl, ',')[[1]]
               my_fc <- dplyr::filter( my_fc, !(fc_method %in% my_plot_excl ) )

           }

          if( excl_limits){

               my_fc <- dplyr::filter( my_fc, !(fc_type %in% c('lower','upper')) )

           }

           my_fc$date <- with(my_fc, as.Date( paste(yr, mth, 1, sep ='-')))

           g <- ggplot(my_fc, aes(x= date, y = value, group = fc_type))
           g <- g + geom_line( aes( colour = fc_type), size = 1.0)
           g <- g + facet_wrap(~fc_method)

           if( is.null(avg_join) ){

               if(!excl_limits){

                       g <- g + scale_color_manual(
                           values = c(
                               lower= beamaColours::get_grayblue(),
                               upper = beamaColours::get_grayblue(),
                               forecast = beamaColours::get_pink()
                           )
                       )

                       g <- g + scale_linetype_manual(
                           values = c(
                              lower = 'dashed',
                              upper = 'dashed',
                              forecast = 'solid'
                           )
                        )
               }else{

                   g <- g + scale_color_manual(
                       values = c(
                           forecast = beamaColours::get_pink()
                       )
                   )

                   g <- g + scale_linetype_manual(
                       values = c(
                           forecast = 'solid'
                       )
                   )


               }

           }else{

               if(!excl_limits){

                       g <- g + scale_color_manual(
                           values = c(
                               lower= beamaColours::get_grayblue(),
                               upper = beamaColours::get_grayblue(),
                               forecast = beamaColours::get_pink(),
                               actual = beamaColours::get_blue()
                           )
                       )

                       g <- g + scale_linetype_manual(
                           values = c(
                               lower = 'dashed',
                               upper = 'dashed',
                               forecast = 'solid',
                               actual = 'solid'
                           )
                       )

               }else{

                   g <- g + scale_color_manual(
                       values = c(
                           forecast = beamaColours::get_pink(),
                           actual = beamaColours::get_blue()
                       )
                   )

                   g <- g + scale_linetype_manual(
                       values = c(
                           forecast = 'solid',
                           actual = 'solid'
                       )
                   )


               }


           }

           if(is_themed){
               g <- g + theme_igray()
               #g <- g + scale_colour_tableau("colorblind10")
               g <- g + theme(
                   strip.background = element_rect(colour = strip_col, fill = strip_col),
                   strip.text.x = element_text(family="Museo 300", face="plain",colour = strip_fcol, size= strip_txt_size),
                   legend.position = "none",
                   legend.title = element_blank(),
                   text = element_text(family="Museo 300", face="plain"),
                   plot.title = element_text(family="Museo 500", face="plain", size= title_font_size)

               )
           }

           g <- g + theme(
               legend.title = element_blank(),
               legend.position = 'top'
           )

           g <- g + xlab("") + ylab( "" )

           print(g)

        }

    )#public

) #class






