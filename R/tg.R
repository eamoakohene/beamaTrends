
SMART_LABELS =  list(
    MAT="Moving Annual Average",
    MTH="Monthly",
    QTR="Quarterly",
    YR = "Yearly",
    YTD = "Year-to-date",
    MM1 = '1-month % change',
    MM3 = '3-month % change',
    MM12 = '12-month % change',
    QQ1 = '1-quarter % change',
    QQ4 = '4-quarter % change',
    YY1 = '1-year % change',
    MAT1 = 'MAT: 1-month % change',
    MAT12 = 'MAT: 12-month % change',
    MAT4 = 'MAT: 4-quarter % change',
    YTD1 = 'YTD: 1-month % change',
    YTD4 = 'YTD: 4-quarter % change',
    YTD12 = 'YTD: 12-month % change'
)

BREXIT_POINT = "2016-06-23"

#'Helper class for aggregation and generating growth from timeseries object
#'

tg <- R6::R6Class(
    "tg",

    public = list(

        data_df = NULL,  # input dataframe
        data_raw = NULL, # unformatted data input
        data_ts = NULL,  # input timeseries
        data_freq = NULL, # frequency of timeseries data
        data_count = NULL, # number of data points
        data_format = 4,   # data format 0=from db, 1 = timeseries object, 2 = dataframe, 3 = vector
        data_start = NULL, # starting point of timeseries data
        data_agg = NULL,   # holds aggregated format (eg monthly to quarterly, quarterly to yearly)
        data_agg_to = NULL, # destination format
        data_fx = 1,        # data periodicity
        data_growth = NULL, # growth timeseries object
        data_growth_fx = NULL, # growth periodicity
        data_code = NULL,
        data_days = NULL,

        db_name = 'R:/shiny/beama/bmonitor/bss.sqlite', #repository of data in sqlite
        db_sql = NULL, #query to fetch data from database
        db_df = NULL,  #query results in dataframe format
        db_ts = NULL,  #query resutls in timeseries format
        db_freq = NULL, #timeseries frequency


        #data format values
        DB = 0, TS = 1,DF=2,VEC = 3, UNKNOWN = 4,

        #data periods
        MTH=0, QTR = 1, YR = 2, YTD=3, MAT= 4, MQT=5, NOTKNOWN = 6,



        initialize = function(x, x_start=NULL, x_frq = NULL, db_limit = list(yr=NULL, mth=12) ){

            self$data_raw <- x
            if( ! is.null( db_limit$yr )){ self$data_days <- with(db_limit, 372 * yr + 31 * mth + 1 ) }

            self$set_data_format(self$data_raw)

            if(self$data_format == self$UNKNOWN ){
                cat("Unknown data format. Format should be : Timeseries, Dataframe or Vector")
                return(NULL)
            }

            if(self$data_format == self$DB){

                if(!is.null(x_start)){ self$set_data_start(x_start) }
                if(!is.null(x_frq)){ self$set_data_frequency(x_frq) }
                self$set_db_ts_properties()

            }else{

                self$set_data_start(x_start)
                self$set_data_frequency(x_frq)
                self$set_data_ts()
                self$set_data_df()
            }
        },

        #'set growth periodicity
        #'monthly, quarter, etc
        set_growth_fx =function(value){
            if(!is.null(value) && value %in% private$MM1:private$NOTHING){
                self$data_growth_fx <- value
            }else{
                cat("Invalid growth rate ")
            }
            invisible(self)
        },

        #'get growth periodicity
        get_growth_fx = function(){
            return(self$data_growth_fx)
        },

        #'get growth data
        #'fx = growth function to use
        #'  mm1 = "current month compared to pervious month"
        #'  mm3 = "current month compared to 3 months ago"
        #'  mm12 = "current month compared to a year ago"
        #'  qq1 = "current quarter compared with pervious quarter"
        #'  qq4 = "current quarter compared to a year ago
        #'  yy1 = "current year compared to previous year"
        #'  mat1 = "moving annual total compared to a year ago"
        #'  mqt1 = "moving quarterly total compared to a quarter ago"
        #'  mqt4 = "moving quarterly total compared to a year ago"
        #'  ytd12 = "monthly year to date compared to previous year"
        #'  ydt4 = "quarterly year to date compared to previous year"
        #'
        #'ops = operation to perform, either "sum" or "avg"

        get_growth_data = function(fx='mm12', ops='sum'){

            self$data_growth<- switch( fx,
               mm1 = self$get_mm1(),
               mm3 = self$get_mm2(),
               mm12 =self$get_mm12(),
               qq1 = self$get_qq1(),
               qq4 = self$get_qq4(),
               yy1 = self$get_yy1( ops = ops ),
               mat1 = self$get_mat1( ops = ops ),
               mat12 = self$get_mat12( ops = ops ),
               mat4 = self$get_mat4( ops = ops ),
               ytd12 = self$get_ytd12( ops = ops ),
               ytd4 = self$get_ytd4( ops = ops ),

               NULL

            )
            return( self$data_growth )

        },

        #' set format of original data
        set_data_format = function(value){

           if(!missing(value) && !is.null(value)){

                my_format <- self$UNKNOWN

                if( is.data.frame(value) ){

                    my_format <- self$DF

                }else if(is.ts(value)){

                    my_format <- self$TS

                }else if( is.character(value) ){

                    self$data_code <- value
                    my_format <- self$DB

                }else if( is.vector(value)){
                    my_format <- self$VEC

                }

                if( !(is.character(value) )){

                    if(!(my_format == self$UNKNOWN)){

                        self$data_code <- paste0(
                            'btrends-',
                            lubridate::year(   Sys.Date() ), '-',
                            lubridate::month(  Sys.Date() ), '-',
                            lubridate::day(    Sys.Date() ), '-',
                            lubridate::hour(   Sys.Date() ),
                            lubridate::minute( Sys.Date() ),
                            lubridate::seconds(Sys.Date() )
                        )

                    }
                }

               self$data_format <- my_format
           }
            invisible(self)
        },

        #' get format of original data
        get_data_format = function(){
            return(
                self$data_format
            )
        },

        #' set start parameter for timeseries object in format
        #' value = c(year,month)
        set_data_start = function(value){

            my_start <-  value

            if( is.null( value )){
                if(self$data_format == self$TS){

                    self$data_start <- stats::start( self$data_raw)

                }else if(self$data_format == self$VEC && !is.null(value)){
                    my_local_start <- value
                    if(length(my_local_start) == 2 ){
                        if( (my_local_start[1] > 1900) && (my_local_start[2] %in% 1:12)){
                            self$data_start <- value
                        }else{
                            cat("Start year or month is out of range")
                            return(NULL)
                        }
                    }else {
                        cat("Start should be vector in format (year,month)")
                        return(NULL)
                    }

                }else{

                    my_col_names <- tolower(names(self$data_raw))
                    my_col_len <- length(my_col_names)
                    if(my_col_len < 2){
                        cat("Insufficient number of columns. Dataframe should either be in format [yr,mth,value] or [yr,value]")
                        return(NULL)
                    }

                    if(my_col_len == 3){
                        if( ("yr" %in% my_col_names) && ("mth" %in% my_col_names)){
                            my_df <- dplyr::arrange(self$data_raw,yr,mth)
                            self$data_start <- c(my_df$yr[1],my_df$mth[1])

                        }else{
                            cat("What meanest thou, o sleeper!\n")
                            cat("Invalid column names: column names should be (yr,mth,value)\n")
                            cat("Perhaps you need a coffee break\n")
                            return(NULL)
                        }
                    }else{
                        if( ("yr" %in% my_col_names) ){
                            my_df <- dplyr::arrange(self$data_raw,yr)
                            self$data_start <- c(my_df$yr[1])
                        }else{
                            cat("Invalid column names: column names should be (yr,value) or (yr,mth,value)")
                            return(NULL)
                        }#if yr

                    }#else 2 columns
                }#DF
            }else{
                self$data_start <- value
            }
            invisible(self)

        },#function set_data_start

        #' get start parameter of orginal timeseries object
        get_data_start = function(){
            return(
                self$data_start
            )
        },

        set_data_frequency = function(value){

            if(is.null( value )){

                if(self$data_format == self$TS){

                    self$data_freq <- stats::frequency( self$data_raw )

                }else if( (self$data_format %in% c(self$VEC,self$DF) ) && !is.null(value) && value %in% 1:365){

                    self$data_freq <- value

                }else if(self$data_format==self$DF && is.null(value)) {

                    if( dim(self$data_raw)[2] == 2){

                        self$data_freq <- 1

                    }else{

                            my_df <- self$data_raw
                            my_yr <- dplyr::distinct(self$data_raw,yr)
                            my_yr <- dplyr::arrange(my_yr,yr)
                            my_yr_n <- nrow(my_yr)

                            if(my_yr_n >2){

                                self$data_freq <- sqldf::sqldf( paste0("select count(yr) as n from my_df where yr =", my_yr$yr[2]) )$n

                            }else{

                                cat("Not enough data to calculate frequency automatically. Please manually supply frequency")
                                return(NULL)

                            }
                    } #DF with 3 cols
                } # DF auto freq
            }else{
                self$data_freq <- value
            }
            invisible(self)
        },#function set_freq

        get_data_freq = function(){
            return(
                self$data_freq
            )
        },

        set_data_ts = function(){

            if(self$data_format == self$TS){

                self$data_ts <- self$data_raw

            }else if(self$data_format == self$VEC){

                self$data_ts <- stats::ts( self$data_raw, start=self$data_start, frequency = self$data_freq)

            }else if( self$data_format == self$DF){

                self$data_ts <- stats::ts( self$data_raw$value, start=self$data_start, frequency = self$data_freq)

            }else if(self$data_format == self$DB){

                if(!is.null( self$db_ts )){
                    self$data_ts <- self$db_ts
                }else{
                    self$set_db_ts_properties()
                }
            }
            invisible(self)
        },

        get_data_ts = function(){

            return(
                self$data_ts
            )
        },

        get_data_db = function(){

            my_data <-  self$db_run_sql( sql )
            my_rows <- nrow(my_data)

            if(my_rows > 0){

                 self$data_df <- my_rows

                 my_yrs <- sqldf::sqldf("select distinct yr from my_data order by yr")

                 if(nrow(my_yrs) > 2){
                   my_yr <- my_yrs$yr[2]
                   my_mths <- dplyr::filter(my_data,yr == my_yr)
                   self$data_freq <- nrow(my_mths)
                 }else if( nrow(my_yrs) >0){
                     self$data_freq <- 1
                 }else{
                     cat("No data for this code = ", self$data_raw,'. Aborting ....\n')
                     return(NULL)
                 }
                 self$data_ts <- ts( c(my_rows$value), start = self$data_start, frequency = self$data_freq)


            }# rows > 0

        },

        db_get_sql = function(is_force=FALSE){

            if( is.null( self$db_sql )|| is_force){

                where2 <- ""

                if( ! is.null( self$data_days ) ){

                    where2 <- sprintf(" and data_days <= %s ", self$data_days )
                }

                sql1 <- "select yr,mth,data_code,data_value as value from trends_data where "
                sql2 <-  sprintf(" upper( data_code) in ('%s') %s order by yr, mth", toupper( self$data_raw ), where2)
                self$db_sql <-  paste0( sql1, sql2 )
            }
            return( self$db_sql)
        },

        get_db_sql = function(is_force = FALSE){

           self$db_get_sql(is_force = is_force)

        },

        db_run_sql = function(){

            return(sqldf::sqldf( DBI::SQL( self$db_get_sql() ) ,dbname =  self$db_name))

        },

        db_get_data = function(force=FALSE){

            if(!is.null( self$db_ts) && !force) {

                return(self$db_ts)

            }else{

                set_db_ts_properties()

            }
            return( self$db_ts)
        },

        set_db_ts_properties = function(){

            self$db_df <- my_data <- self$db_run_sql()

            if( is.null( self$data_start) ){ self$data_start <- c( my_data$yr[ 1 ], my_data$mth[ 1 ] )}

            if( is.null( self$data_freq)){

                my_yrs <- sqldf::sqldf( "select distinct yr from my_data order by yr" )

                if( nrow(my_yrs) > 2){

                    my_yr <- my_yrs$yr[2]
                    my_mths <- dplyr::filter(my_data,yr == my_yr)
                    self$data_freq <- my_frq <- nrow(my_mths)

                    if(self$data_freq == 4){

                        self$data_start <- c( my_data$yr[ 1 ], my_data$qtr[ 1 ] )

                    }


                }else if( nrow(my_yrs) >0){

                    self$data_freq <- 1

                }else {

                    cat("No data for code =",self$data_raw,"\n")
                    return(NULL)

                }
            }

            self$db_ts <- ts(c(my_data$value), start = self$data_start, frequency = self$data_freq)
            self$data_ts <- self$db_ts
            self$set_data_df()

            invisible(self)

        },



        db_get_freq = function(){

            if(!is.null( self$db_freq)){

                return( self$db_freq)

            }else if(!is.null( self$db_ts)){

                self$db_freq <- frequency( self$db_ts)
                return( self$db_freq)

            }else if( !is.null(self$db_df )){

                self$set_db_ts_properties ()
                return(self$data_freq)

            }
        },

        get_db_freq = function(){
            self$db_get_freq()
        },



        db_set_name = function(value){
            if( !is.null(value) ) {self$db_name = value}
            invisible(self)
        },

        set_db_name = function( value){
          self$db_set_name(value)
        },

        db_get_name = function(){
            return( self$db_name)
        },

        get_db_name = function(){
           self$db_get_name()
        },

        to_df = function( ts_data, ts_name = NULL, rm_na = FALSE ){

            my_df <- NULL
            my_frq <- frequency( ts_data)

            if(my_frq %in% c(1,12)){

                my_df <- data.frame( date=zoo::as.Date(zoo::as.yearmon(time( ts_data ))), value=as.matrix( ts_data ))

            }else if(my_frq == 4){

                my_len <- length(ts_data)
                my_start <- stats::start( ts_data )
                my_start_date <- lubridate::make_date( my_start[1], 3 * my_start[2], 1 )
                my_dates <- seq( as.Date( my_start_date ), by='quarter', length.out = my_len )

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

        },

        set_data_df =function(){

            self$data_df <- self$to_df ( self$data_ts )
            invisible(self)

        },

        get_data_df = function(){
            return(
                self$data_df
            )
        },

        get_ytd_ts = function(ops='sum'){

            n <- nrow( self$data_df )
            x <- dplyr::arrange( self$data_df, date )
            is_sum <- tolower( trimws( ops) ) =='sum'

            ytd <- numeric( n )
            ytd[1] <- x$value[1]
            j <- 1
            total <- ytd[1]

            for(i in 2:n){

                if(x$yr[i]==x$yr[i-1]){

                    j <- j + 1
                    total <- total + x$value[i]
                    ytd[i] <- ifelse( is_sum, total, total/j)
                    #cat('i=',i,' j=',j, ' total = ', total, ' total/j ', total/j, "\n")

                }else{

                    ytd[i] <-  x$value[i]
                    j <- 1
                    total <- ytd[1]
                }
            }

            return(
                stats::ts( ytd, start = self$data_start, frequency = self$data_freq )
            )
        },

        get_yearly_ts = function(ops='sum'){

            is_sum <- tolower( trimws( ops) ) =='sum'

            my_sql <- 'select yr, sum(value) as total from my_df group by yr order by yr'

            #cat("is_sum = ", is_sum, "\n")
            if(!is_sum){
                 my_sql <- 'select yr, avg(value) as total from my_df group by yr order by yr '
            }

            my_df <- self$data_df
            my_agg <- sqldf::sqldf(my_sql)
            return(
                stats::ts( c(my_agg$total), start=c(my_df$yr[1],1), frequency = 1)
            )

        },

        set_agg = function(to = 'qtr', ops = 'sum'){

            my_to <- tolower( trimws( to ) )
            is_sum <- tolower( trimws( ops) ) =='sum'

            my_match <- switch( my_to,
                'mth' = self$MTH,
                'qtr' = self$QTR,
                'yr'  = self$YR,
                'ytd' = self$YTD,
                'mat' = self$MAT,
                'mqt' = self$MQT
            )

            if(is.null(my_match )){

                self$data_agg_to <- self$NOTKNOWN
                cat('Unknown conversion',to,'\n')
                return(NULL)

            }else{
                self$data_agg_to <- my_match
            }



            if( self$data_freq == 12){

                if( self$data_agg_to == self$MTH){

                    self$data_agg <- self$data_ts
                }

                if( self$data_agg_to == self$MQT ){

                    if(is_sum){
                        self$data_agg <- zoo::rollsumr( self$data_ts, k = 3 )
                    }else{
                        zoo::rollmeanr( self$data_ts, k = 3 )
                    }

                }else if( self$data_agg_to == self$MAT ){

                    if(is_sum){
                        self$data_agg <- zoo::rollsumr( self$data_ts, k =12 )
                    }else{
                        self$data_agg <- zoo::rollmeanr( self$data_ts, k = 12 )
                    }

                }else if( self$data_agg_to == self$YR ){

                     self$data_agg <- self$get_yearly_ts( ops = ops)

                }else if(self$data_agg_to == self$QTR){

                    if(is_sum){

                        q_sql <- "select yr,qtr,sum(value) as value from m_data group by yr,qtr"

                    }else{

                        q_sql <- "select yr,qtr,avg(value) as value from m_data group by yr,qtr"

                    }

                    m_data <- self$to_df( self$data_ts, 'x' )
                    m_data$qtr <- ceiling( m_data$mth / 3)
                    q_data <- sqldf::sqldf( q_sql )

                    if(nrow(q_data) > 0){
                        self$data_agg <- ts(q_data$value, start = c(q_data$yr[1], q_data$qtr[1]) , frequency = 4)
                    }else{
                        self$data_agg <- NULL
                    }



                }else if( self$data_agg_to == self$YTD){

                    self$data_agg <- self$get_ytd_ts( ops = ops)

                }else{
                       cat('Impossible conversion. You are trying to convert year,qtr or mth to mth. Conversion should be from month>(qtr,yr) or qtr>yr \n')
                        return(NULL)

                }#YTD

            }else if(self$data_freq == 4){

                if( self$data_agg_to == self$QTR){

                    self$data_agg <- self$data_ts

                }else if( self$data_agg_to == self$MAT ){

                    if( is_sum ){
                        self$data_agg <- zoo::rollsumr( self$data_ts, k = 4)
                    }else{
                        self$data_agg <- zoo::rollmeanr( self$data_ts, k = 4)
                    }

                }else if( self$data_agg_to == self$YR ){

                    self$data_agg <- self$get_yearly_ts( ops = ops)

                }else if( self$data_agg_to == self$YTD){

                    self$data_agg <- self$get_ytd_ts( ops = ops)

                }else{
                    cat('Impossible conversion \n')
                    return(NULL)
                }#YTD
            }else if(self$data_freq > 12){

                if( self$data_agg_to == self$MTH){

                }
            }

            invisible(self)
        },#function

        get_agg = function(){
            return(
                self$data_agg
            )
        },

        set_agg_default = function(to = 'qtr', ops = 'sum'){
            self$set_agg( to = to, ops = ops)

            self$data_raw <- self$data_agg
            self$set_data_format(self$data_raw)

            self$set_data_start( start(self$data_raw) )
            self$set_data_frequency( frequency( self$data_raw) )
            self$set_data_ts()
            self$set_data_df()

            invisible(self)

        },
        ### GROWTH FUNCTIONS ###
        get_mm1 = function(){
            if( self$data_freq == 12 ){

                my_growth <- quantmod::Delt( c(self$data_ts),k=1)*100
                self$data_growth <- ts( c(my_growth), start = self$data_start, frequency = self$data_freq)
                return(self$data_growth)

            }else{
                cat("Month to month growth rate not possible. Frequency must be 12")
                return(NULL)
            }
        },

        get_mm3 = function(){
            if( self$data_freq == 12 ){

                my_growth <- quantmod::Delt( c(self$data_ts),k=3)*100
                self$data_growth <- ts( c(my_growth), start = self$data_start, frequency = self$data_freq)
                return(self$data_growth)

            }else{
                cat("Three-monthly growth rate not possible. Frequency must be 12")
                return(NULL)
            }
        },

        get_mm12 = function(){
            if( self$data_freq == 12 ){

                my_growth <- quantmod::Delt( c(self$data_ts),k=12)*100
                self$data_growth <- ts( c(my_growth), start = self$data_start, frequency = self$data_freq)
                return(self$data_growth)

            }else{
                cat("Year on year monthly growth rate not possible. Frequency must be 12")
                return(NULL)
            }
        },

        get_qq1 = function(){
            if( self$data_freq == 4 ){

                my_growth <- quantmod::Delt( c(self$data_ts),k=1)*100
                self$data_growth <- ts( c(my_growth), start = self$data_start, frequency = self$data_freq)
                return(self$data_growth)

            }else if(self$data_freq == 12){

                my_data <- self$set_agg( 'qtr' )$get_agg()
                my_start <- start( my_data )
                my_growth <- quantmod::Delt( c( my_data ), k = 1)*100
                self$data_growth <- ts( c(my_growth), start = my_start, frequency = 4)
                return( self$data_growth )

            }else{
                cat("Quarter to quarter growth rate not possible. Frequency must be 4 or 12")
                return(NULL)
            }
        },

        get_qq4 = function(){
            if( self$data_freq %in% c(4,12) ){

                my_data <- NULL

                if( self$data_freq == 4){
                    my_data <- self$data_ts
                }else{
                    my_data <- self$set_agg( 'qtr' )$get_agg()
                }

                my_start <- start( my_data )
                my_growth <- quantmod::Delt( c( my_data ), k = 4)*100
                self$data_growth <- ts( c(my_growth), start = my_start, frequency = 4)
                return( self$data_growth )

            }else{
                cat("Year on year quarterly growth rate not possible. Frequency must be 4 or 12")
                return(NULL)
            }
        },

        get_yy1 = function( ops = 'sum'){
            if( self$data_freq %in% c(1,4,12) ){

                my_data <- NULL

                if( self$data_freq == 1){
                    my_data <- self$data_ts
                }else{
                    my_data <- self$set_agg( 'yr', ops = ops )$get_agg()
                }

                my_start <- start( my_data )
                my_growth <- quantmod::Delt( c( my_data ), k = 1)*100
                self$data_growth <- ts( c(my_growth), start = my_start, frequency = 1)
                return( self$data_growth )

            }else{
                cat("Year on year growth rate not possible. Frequency must be 1 or  4 or 12")
                return(NULL)
            }
        },
        get_mat1 = function( ops = 'sum'){
            if( self$data_freq %in% c(4,12) ){

                my_data <- self$set_agg( 'mat', ops = ops )$get_agg()

                my_start <- start( my_data )

                my_growth <- quantmod::Delt( c( my_data ), k = 1)*100
                self$data_growth <- ts( c(my_growth), start = my_start, frequency = self$data_freq)
                return( self$data_growth )

            }else{
                cat("Moving total growth rate not possible. Frequency must be 4 or 12")
                return(NULL)
            }
        },

        get_mat12 = function( ops = 'sum'){
            if( self$data_freq %in% c(4,12) ){

                my_data <- self$set_agg( 'mat', ops = ops )$get_agg()

                my_start <- start( my_data )

                my_k <- ifelse( self$data_freq == 12,12,4)

                my_growth <- quantmod::Delt( c( my_data ), k = my_k)*100
                self$data_growth <- ts( c(my_growth), start = my_start, frequency = self$data_freq)
                return( self$data_growth )

            }else{
                cat("Moving total growth rate not possible. Frequency must be 4 or 12")
                return(NULL)
            }
        },

        get_mat4 = function( ops = 'sum'){
            self$get_mat12( ops = ops)
        },



        get_ytd12 = function(ops = 'sum'){
            if( self$data_freq %in% c(4,12) ){

                my_data <- self$set_agg( 'ytd', ops = ops )$get_agg()

                my_start <- start( my_data )

                my_k <- ifelse( self$data_freq == 12,12,4)

                my_growth <- quantmod::Delt( c( my_data ), k = my_k)*100
                self$data_growth <- ts( c(my_growth), start = my_start, frequency = self$data_freq)
                return( self$data_growth )

            }else{
                cat("year todate total growth rate not possible. Frequency must be 4 or 12")
                return(NULL)
            }
        },

        get_ytd4 = function(ops = 'sum'){
            self$get_ytd12( ops = ops )
        },

        get_growth_df = function( ops = 'avg', select = private$ALL_CODES, select_yr = c(2000,2050)){

            n_select <- nchar(select)

            #get code description
            code_desc <- ''
            SQ <- storedQry::SQ$new( tg.get_db( 'trends' ) )$set_name( 'trends_meta_get_desc' )
            my_desc <- SQ$set_params( list( `@s_code`= toupper(self$data_code)  ) )$qry_exec()
            if(nrow(my_desc) >0 ){
              code_desc <- my_desc$description[1]
            }

            if(self$data_freq == private$frq_MTH  ){

                # n_select is the number of characters in growth code,
                # if < 6, then it likely to be just one growth code
                if( n_select < 6){

                    #cat("I am saving the planet\n")

                    my_select <- toupper(select)

                    my_data <- switch( my_select,
                       MM1   = self$to_df( self$get_mm1(), 'MM1', T),
                       MM3   = self$to_df( self$get_mm3(), 'MM3', T),
                       MM12  = self$to_df( self$get_mm12(), 'MM12', T),
                       QQ1   = self$to_df( self$get_qq1(), 'QQ1', T),
                       QQ4   = self$to_df( self$get_qq4(), 'QQ4', T),
                       YY1   = self$to_df( self$get_yy1( ops = ops ), 'YY1', T),
                       MAT1  = self$to_df( self$get_mat1( ops = ops ), 'MAT1', T),
                       MAT12 = self$to_df( self$get_mat12( ops = ops ), 'MAT12', T),
                       YTD4  = self$to_df( self$get_ytd4( ops = ops ), 'YTD4', T),
                       YTD12 = self$to_df( self$get_ytd12( ops = ops ), 'YTD12', T)

                    )
                }else{

                    my_mm1  <- self$to_df( self$get_mm1(), 'MM1', T)
                    my_mm3  <- self$to_df( self$get_mm3(), 'MM3', T)
                    my_mm12 <- self$to_df( self$get_mm12(), 'MM12', T)
                    my_qq1  <- self$to_df( self$get_qq1(), 'QQ1', T)
                    my_qq4  <- self$to_df( self$get_qq4(), 'QQ4', T)
                    my_yy1  <- self$to_df( self$get_yy1( ops = ops ), 'YY1', T)
                    my_mat1 <- self$to_df( self$get_mat1( ops = ops ), 'MAT1', T)
                    my_mat12 <- self$to_df( self$get_mat12( ops = ops ), 'MAT12', T)
                    my_ytd4 <- self$to_df( self$get_ytd4( ops = ops ), 'YTD4', T)
                    my_ytd12 <- self$to_df( self$get_ytd12( ops = ops ), 'YTD12', T)

                    my_data <- rbind(
                          my_mm1, my_mm3, my_mm12, my_qq1, my_qq4, my_yy1, my_mat1, my_mat12, my_ytd4, my_ytd12
                    )

                    my_data <- sqldf::sqldf(
                        sprintf("select * from my_data where name in %s and yr between %s and %s", beamaUtils::split_str( toupper( select ) ), select_yr[1], select_yr[2])
                    )

                }

                my_data$data_code <- self$data_code
                my_data$data_desc <- code_desc
                return(my_data)


            } else if( self$data_freq == private$frq_QTR ){

                if( n_select < 6){

                    my_select <- toupper(select)

                    my_data <- switch( my_select,
                           QQ1   =  self$to_df( self$get_qq1(), 'QQ1', T),
                           QQ4   = self$to_df( self$get_qq4(), 'QQ4', T),
                           YY1  = self$to_df( self$get_yy1( ops = ops ), 'YY1', T),
                           MAT1   = self$to_df( self$get_mat1( ops = ops ), 'MAT1', T),
                           MAT4   = self$to_df( self$get_mat1( ops = ops ), 'MAT4', T),
                           YTD4= self$to_df( self$get_ytd4( ops = ops ), 'YTD4', T)
                    )

                }else{

                    my_qq1 <-self$to_df( self$get_qq1(), 'QQ1', T)
                    my_qq4 <- self$to_df( self$get_qq4(), 'QQ4', T)
                    my_yy1 <- self$to_df( self$get_yy1( ops = ops ), 'YY1', T)
                    my_mat1 <- self$to_df( self$get_mat1( ops = ops ), 'MAT1', T)
                    my_ytd4 <- self$to_df( self$get_ytd4( ops = ops ), 'YTD4', T)
                    my_mat4 <- self$to_df( self$get_mat1( ops = ops ), 'MAT4', T)


                    my_data <- rbind(
                        my_qq1, my_qq4, my_yy1, my_mat1,  my_ytd4, my_mat4
                    )

                    my_data <- sqldf::sqldf(
                        sprintf("select * from my_data where name in %s and yr between %s and %s",  beamaUtils::split_str( toupper( select ) ), select_yr[1], select_yr[2])
                    )
                }

                my_data$data_code <- self$data_code
                my_data$data_desc <- code_desc
                return(my_data)


            } else if( self$data_freq == private$frq_YR ){

                my_yy1 <- self$to_df( self$get_yy1( ops = ops ), 'YY1', T)

                my_data <- sqldf::sqldf(
                    sprintf("select * from my_yy1 where name in %s and yr between %s and %s",  beamaUtils::split_str( toupper( select ) ), select_yr[1], select_yr[2])
                )
                my_data$data_code <- self$data_code
                my_data$data_desc <- code_desc
                return( my_data )


            }else {

                ### TO BE IMPLEMENTED
                return( NULL )
            }
        },

        get_agg_df = function( ops = 'avg', select = private$ALL_CODES, select_yr = c(1900,2050) ){


            if(self$data_freq == private$frq_MTH  ){

                my_mth  <- self$to_df( self$data_ts, 'MTH', T )

                my_qtr <- self$to_df( self$set_agg('qtr', ops = ops )$get_agg() , 'QTR', T )

                my_yr  <- self$to_df( self$set_agg('yr', ops = ops )$get_agg() , 'YR', T )
                my_ytd <- self$to_df( self$set_agg('ytd', ops = ops )$get_agg() , 'YTD', T )
                my_mat <- self$to_df( self$set_agg('mat', ops = ops )$get_agg() , 'MAT', T )


                my_data<-  rbind( my_mth, my_qtr, my_yr, my_ytd, my_mat )

                my_data <- sqldf::sqldf(
                    sprintf("select * from my_data where name in %s and yr>=%s and yr<=%s", beamaUtils::split_str( toupper(select ) ), select_yr[1], select_yr[2])
                )

                return(my_data)


            } else if( self$data_freq == private$frq_QTR ){

                my_qtr  <- self$to_df( self$data_ts, 'QTR', T )
                my_yr  <- self$to_df( self$set_agg('yr', ops = ops )$get_agg() , 'YR', T )
                my_ytd <- self$to_df( self$set_agg('ytd', ops = ops )$get_agg() , 'YTD', T )
                my_mat <- self$to_df( self$set_agg('mat', ops = ops )$get_agg() , 'MAT', T )


                my_data <- rbind( my_qtr, my_yr, my_ytd, my_mat )

                my_data <- sqldf::sqldf(
                    sprintf("select * from my_data where name in %s", beamaUtils::split_str( toupper(select )))
                )

                return(my_data)



            } else if( self$data_freq == private$frq_YR ){

                return( self$to_df( self$data_ts, 'YR', T ) )

            }else {

                ### TO BE IMPLEMENTED - daily fx
                #
                # --- MONTH ----

            }

        },

        #' is_growth = plot parameter or it growth
        #' select = MM1:   1 month growth
        #'          MM12: 12 months growth
        #'          MM3:   3 months growth
        #'          QQ1:  1 quarter growth
        #'          QQ4:  4 quarters growth
        #'          YTD12: year to date growth (monthly)
        #'          YTD4:  year to date growth (quarterly)
        #'          MAT1:  moving annual total compared with prev month
        #'          MAT12: moving annual total compared to prev year
        #'          YY1:  yearly growth
        #' ops         = operation to perform : c("sum", "avg")
        #' delta_x     = margins on x-axis in months
        #' delta_y     = margins on y-axis
        #' skale       = scale factor for y-axis
        #' skale_title = lable to go with y-axis scale factor
        #' title       = title of the plot
        #' select_yr   = year filter eg. c(2015,2020)
        #
        plot = function(
              is_growth = FALSE, select = NULL ,
              ops = 'avg', x_delta = c(0,0), y_delta = c(0,0),dp = 1, skale = 1 ,
              skale_title = 'Value', title = NULL, select_yr = NULL,
               strip_txt_size = 12, is_themed = F,
              line_col = beamaColours::get_corporate_blue(),
              strip_col = beamaColours::get_blue(),
              strip_fcol = 'white', brexit_mode = FALSE, brexit_mode_line = 0.8,
              smart_labels = SMART_LABELS
        ){

            require(ggplot2)
            require(magrittr)
            require(ggthemes)

            my_select_yr <- select_yr
            my_select <- select

            if( is.null(select) ){ my_select <- private$ALL_CODES }
            if( is.null(select_yr)) { my_select_yr <- c(1900,2050) }

            my_data <- NULL
            my_ylab <- NULL

            if(!is_growth) {

                my_data <- self$get_agg_df(ops = ops, select = my_select, select_yr = my_select_yr)
                my_data$value <- my_data$value / skale
                my_ylab <- skale_title

            }else{

                my_data <- self$get_growth_df(ops = ops, select = my_select, select_yr = my_select_yr)
                my_ylab <- "Growth (%)"

            }

            my_data$smart <- my_data$name

            if( !is.null( smart_labels ) ){

                for( name in names(smart_labels)){

                    my_data$smart[ which( my_data$name == name )] <- smart_labels[[ name ]]

                }
            }

            #return(my_data)

            my_data$data_days <- with(my_data,yr*372+mth*32)


            gmin <- my_data %>% dplyr::group_by(name) %>% dplyr::filter( data_days == min(data_days) )
            gmax <- my_data %>% dplyr::group_by(name) %>% dplyr::filter( data_days == max(data_days) )
            gtxt <- rbind(gmin,gmax)


            x_range <- c(min(gmin$date) , max(gmax$date))
            if(!(x_delta[1] == 0)){
                x_min <- min(gmin$date)



                lubridate::month(x_min) <- lubridate::month(x_min) - x_delta[1]
                x_range[1] <- x_min
            }

            if(!(x_delta[2] == 0)){

                x_max <- max(gmax$date)

                lubridate::month(x_max) <- lubridate::month(x_max) + x_delta[2]

                x_range[2] <- x_max
            }

            g <- ggplot(my_data, aes(x = date, y= value)) +
                 geom_line( size = private$LINE_SIZE, colour = beamaColours::get_corporate_blue() ) +
                 facet_wrap(~smart)

            g <- g + xlab("") + ylab( my_ylab )
            if( !is.null( title) ){
                g <- g + ggtitle( title )
            }




            g <- g + geom_point( data=gtxt, aes( x = date, y = value), size=5, colour = line_col)
            g <- g + geom_point( data=gtxt, aes( x = date, y = value), colour = beamaColours::get_pink(),size=3)
            g <- g + geom_text(  data=gtxt, aes( x = date, y = value, label=beamaUtils::set_decimal( value , dp)), vjust= -0.8, hjust = 0.4, size = 4, colour= beamaColours::get_pink() )

            g <- g + theme(legend.position="none", strip.text = element_text( size= strip_txt_size ))

            if(is_themed){
                g <- g + theme_igray()
                g <- g + scale_colour_tableau("colorblind10")
                g <- g + theme(
                     strip.background = element_rect(colour = strip_col, fill = strip_col),
                     strip.text.x = element_text(colour = strip_fcol, size= strip_txt_size),
                     legend.position = "none",
                     legend.title = element_blank()

                )
            }


            g_range <- ggplot_build(g)$layout$panel_ranges[[1]]$y.range

            y_range <- g_range

            #cat("grange = c(", g_range[1]," , ", g_range[2],")\n")
            #cat("ydelta = c(", y_delta[1]," , ", y_delta[2],")\n")

            if( !( y_delta[1] == 0) ){

                if( g_range[1] < 0 ) {
                    y_range[1] <- g_range[1] + y_delta[1]
                }else{
                    y_range[1] <- g_range[1] - y_delta[1]
                }

            }

            if(!( y_delta[2] == 0)){
                if( g_range[2] < 0 ) {
                    y_range[2] <- g_range[2] - y_delta[2]
                }else{
                    y_range[2] <- g_range[2] + y_delta[2]
                }
            }

            #cat("yrange = c(", y_range[1]," , ", y_range[2],")\n")

            if( (y_range[1]*y_range[2]) < 0 ){
                g <- g + geom_hline(yintercept = 0)
            }

            if( !( sum(y_delta) == 0) ) {
                g <- g + ylim( y_range )
            }

            if( !( sum(x_delta) == 0 ) ){
                g <- g + xlim( x_range )
            }

            if(brexit_mode){
              g <- g + geom_vline( aes(xintercept = as.numeric(as.Date( BREXIT_POINT )) ), colour = beamaColours::get_pink(), linetype = 'dashed', size= brexit_mode_line )
            }
            print(g)
            return(gtxt)

        }

        # #### spider ###
         ,plot_spider = function(
             fx='m', k = 12, title= NULL,
             y1 = lubridate::year(Sys.Date())-2, y2=lubridate::year(Sys.Date()) ,
             verbose =FALSE, strip_txt_size = 12, is_themed = T,
             line_col = beamaColours::get_corporate_blue(),
             strip_col = beamaColours::get_blue(),
             strip_fcol = 'white',y_delta=c(0,0),x_delta=c(0,0)
         ){

             require("magrittr")
             require("ggplot2")
             require("ggthemes")


             frq <- self$data_freq
             data_desc <- NULL
             if( is.null(title)){
                data_desc <- self$data_code
             }else{
                 data_desc <- title
             }

             x_label <- NULL
             y_label <- NULL

             if( frq == 12 ){

                 x_label <- "1-monthly % change"

                 k <- 12

                 y_label <- paste0( frq,"-month % change")

             }

             if( frq == 4  ){

                 x_label <- "1-quarter % change"

                 k <- 4
                 y_label <- paste0( frq,"-quarter % change")


             }




             abc <- self$get_data_df()


             left_shift <- function(x,delta=1){ tp_utils$new()$vec_shift_left(x,delta)}

             abc$mom <- quantmod::Delt( abc$value, k = 1)[ , 1 ] * 100
             abc$yoy <- quantmod::Delt( abc$value ,k = frq)[ , 1 ] * 100

             abc$mome <- left_shift( abc$mom )
             abc$yoye <- left_shift( abc$yoy )



             gdata <-  dplyr::filter(abc, yr >= y1)
             gdata$data_desc <- data_desc
             gdata$lbl <- ""
             xy_scale <- 1
             delta <- 0.2



             gdata$data_days <- with( gdata, yr * 372 + mth * 32  )
             gmin <- dplyr::filter( gdata, !is.na( yoy ))  %>% dplyr::filter( data_days == min( data_days ))
             gmax <- gdata  %>% dplyr::filter( data_days == max( data_days ) )
             gtxt <- rbind( gmin, gmax )


             g <- ggplot( gdata, aes( x = mom, y = yoy, label = lbl))
             g <- g + geom_point( colour = beamaColours::get_pink(), size = 3 )
             g <- g + geom_hline( yintercept = 0 )  + geom_vline( xintercept = 0)
             g <- g + geom_segment( aes( xend = mome, yend = yoye ), size = 1.0, colour = line_col,arrow=grid::arrow(angle=15, type = "closed", length = grid::unit(0.18, "inches")))
             g <- g + labs( y = y_label, x = x_label )

             g <- g + geom_text( aes( label = paste0( month.abb[ mth ], yr %% 100 ), vjust = -0.8, hjust = 0.5, size = 5))
             g <- g + geom_point( data = gtxt, aes( x = mom, y = yoy), size = 5, colour = line_col )
             g <- g + geom_point( data = gtxt, aes( x = mom, y = yoy, colour = factor( data_days ) ), size = 4 )

             g <- g + facet_wrap( ~ data_desc )


             y_range <- g_range <- ggplot_build(g)$layout$panel_ranges[[1]]$y.range

             if( !( y_delta[1] == 0) ){
                 y_range[1] <- g_range[1] - y_delta[1]
             }

             if(!( y_delta[2] == 0)){
                 y_range[2] <- g_range[2] + y_delta[2]
             }

             if( !( sum(y_delta) == 0) ) {
                 g <- g + ylim( y_range )
             }

             x_range <- g_range <- ggplot_build(g)$layout$panel_ranges[[1]]$x.range

             if( !( x_delta[1] == 0) ){
                 x_range[1] <- g_range[1] - x_delta[1]
             }

             if(!( x_delta[2] == 0)){
                 x_range[2] <- g_range[2] + x_delta[2]
             }

             if( !( sum(x_delta) == 0) ) {
                 g <- g + xlim( x_range )
             }

             if(is_themed){
                 g <- g + theme_igray()
                 g <- g + scale_colour_tableau("colorblind10")
                 g <- g + theme(
                     strip.background = element_rect(colour = strip_col, fill = strip_col),
                     strip.text.x = element_text(colour = strip_fcol, size= strip_txt_size),
                     legend.position = "none",
                     legend.title = element_blank()

                 )
             }


             print( g )

             if( verbose ) {  return( abc )}
         }



        #### spider ends ###

    ),
    private = list(

        #aggregate fxns
        SUM =1, AVG=2,

        frq_MTH = 12, frq_QTR = 4, frq_YR = 1, frq_DAILY = 270,
        frqs = c(1,4,12,270),

        #growth rates
        MM1 =1,MM3=2,MM12=3, QQ1=4, QQ4=5,YY1=6,MAT1=7,MQT1=8,MQT4=9, YTD12=10, YTD4=11, NOTHING=12,

        #plot constants
        LINE_SIZE = 1.1,
        LINECOLOUR = beamaColours::get_line_colour(),
        SMOOTHCOLOUR = beamaColours::get_pink(),
        ALL_CODES = 'MAT,MTH,QTR,YR,YTD,MAT1,MAT12,MM1,MM12,MM3,QQ1,QQ4,YTD12,YTD4,YY1'

    )
)


### NON CLASS ITEMS

#' only for internal use
#' run a query on local persistent data store

run_sql = function(qry){
    return(sqldf::sqldf( qry ,dbname =  'R:/shiny/beama/bmonitor/bss.sqlite'))
}

tg.split_str <- function (s = "CHAY,CHAW,D7BT"){

    return(gsub(",", "','", s))
}

tg.get_db <- function(src='trends'){

    my_db <- switch(src,
          trends = 'R:/shiny/beama/bmonitor/bss.sqlite',
          indx = 'R:/packages/bindices/beama_indices.sqlite',
          fx = 'R:/packages/beamafx/inst/extdata/beamafx.sqlite',
          btrends='R:/packages/btrends/btrends.sqlite'
    )
    return(my_db)
}

tg.get_data <- function(code="CHAW,CHAY",y1=1990,y2=2020,m1=1,m2=12, d1=0,d2=0 ,db='trends', basis='2010=100', fx='m', op='SUM',...){

    #code="m_elec,m_mech";y1=1990;y2=2020;m1=1;m2=12; d1=0;d2=0 ;db='trends'; basis='2010=100'; fx='m'
    my_data <- NULL
    my_db <- tolower(db)# my_db <- 'indx';code='m_elec'
    dd1 <- ( y1 * 12 + m1) * 31 + d1
    dd2 <- ( y2 * 12 + m2) * 31 + d2

    if( my_db == 'trends'){

        qry_name <- paste0( 'trends_get_data_', fx)
        my_code <- tg.split_str( code )
        SQ <- storedQry::SQ$new( tg.get_db( db ) )$set_name( qry_name )
        my_qry <- SQ$set_params(
            list(
                `@s_code`= my_code, `@i_y1`= dd1  ,`@i_y2`= dd2, `@i_op` = op
            )
        )
        #cat('qry = ', my_qry$params_replace(),'\n')

        my_data <- my_qry$qry_exec()

        #cat('rows = ', nrow(my_data ),"\n")





    }

    if( my_db == 'indx'){

        SQ <- storedQry::SQ$new( tg.get_db( my_db ) )$set_name('beama_index_get_trends')
        my_data <- SQ$set_params(
                        list(
                            `@i_index_name`= beamaUtils::split_str(code), `@s_index_base_to`= basis,
                            `@i_y1`=y1  ,`@i_y2`=y2
                        )
                   )$qry_exec()

        #cat('qry = ', my_qry$params_replace(),'\n')
        #my_data <- my_qry



    }

    return( my_data )
}

tg.sync_desc <- function(code, description){

    #first update the meta table
    SQ <- storedQry::SQ$new( tg.get_db( 'trends' ) )$set_name( 'trends_meta_update_desc' )
    my_qry <- SQ$set_params(
        list(
            `@s_data_code`=code, `@s_data_desc`= description
        )
    )$qry_exec()

    #then update table
    SQ <- storedQry::SQ$new( tg.get_db( 'trends' ) )$set_name( 'trends_data_update_desc' )
    my_qry <- SQ$set_params(
        list(
            `@s_data_code`=code, `@s_data_desc`= description
        )
    )$qry_exec()


}

tg.get_unit <- function(code){


    SQ <- storedQry::SQ$new( tg.get_db( 'trends' ) )$set_name( 'trends_meta_get_unit' )
    my_unit <- SQ$set_params(
        list(
            `@s_code`= code
        )
    )$qry_exec()

   if(nrow( my_unit ) > 0){

       return( my_unit$unit)
   }

   return('')

}


tg.plot_trends<- function(

    code = 'CHAW', y1 = 2013, y2 = 2020, dp = 1,
    grp = FALSE,title = "", lgpos = list( x=0.5, y=0.5, ylab= NULL ),
    add_intercept = FALSE, verbose = FALSE, xaxis_fmt="", xaxis_prd="years", scale=1, is_themed=FALSE,
    y_zero = FALSE, y_manual = c(0,0), strip_txt_size = 12,line_col = beamaColours::get_corporate_blue(),
    strip_col = beamaColours::get_blue(), strip_fcol = 'white', db='trends', fx = 'm',op='SUM', x_delta =c(0,0), y_delta = c(0,0),
    line_size = 1.3, geom_txt_size = 5, geom_txt_col = beamaColours::get_pink(),
    select = NULL #'MAT1,MAT12,MM1,MM12,MM3,QQ1,QQ4,YTD12,YTD4,YY1'

){

    require(ggthemes)
    require(magrittr)
    require(ggplot2)

    ###
    #code='bbt_combined,bbt_beama,bbt_ons'; plot_type = "tracker";scale=1;k=12;db='trends';fx='m'
    #y1=2013;y2=2020;grp=FALSE;title="";y_zero = FALSE; y_manual = c(0,100);grp = FALSE
    #add_intercept=FALSE;verbose=FALSE;xaxis_fmt="";xaxis_prd="years";scale=1;smooth=FALSE

    my_code <- toupper( code )
    my_ylab <- NULL
    my_data <- NULL
    my_wrap <- c('data_desc')

    if(!is.null( select )){

       my_data <- tg.get_growth_data(code = code, select = select, select_yr = c(y1,y2))
       my_data <- dplyr::filter(my_data,yr >= y1, yr<= y2)
       my_ylab <- 'Growth (%)'
       my_wrap <- c('data_desc', 'smart')

    }else{

       my_data <- tg.get_data( code = my_code, y1 = y1, y2 = y2, db = db, fx = fx, op = op )
       my_wrap <- c('data_desc', 'data_unit')
       my_ylab <- lgpos$ylab

       if( is.null(lgpos$ylab) ){

           my_ylab <- ""

       }
    }

    #return(my_data)


    my_data_count <- nrow(my_data)
    if( my_data_count == 0){cat("Aborting process: 0 records"); return(NULL)}

    if(is.null( my_data$date) ){
        my_data$date <- as.Date( sprintf("%s-%s-01", my_data$yr, my_data$mth))
    }

    g <- NULL
    if(!grp){


        my_data$data_days <- my_data$yr*372+my_data$mth*32

        gmin <- my_data %>% dplyr::group_by(data_desc) %>% dplyr::filter(data_days==min(data_days))
        gmax <- my_data %>% dplyr::group_by(data_desc) %>% dplyr::filter(data_days==max(data_days))
        gtxt <- rbind(gmin,gmax)

        x_range <- c(min(gmin$date) , max(gmax$date))

        if(!(x_delta[1] == 0)){
            x_min <- min(gmin$date)



            lubridate::month(x_min) <- lubridate::month(x_min) - x_delta[1]
            x_range[1] <- x_min
        }

        if(!(x_delta[2] == 0)){

            x_max <- max(gmax$date)

            lubridate::month(x_max) <- lubridate::month(x_max) + x_delta[2]

            x_range[2] <- x_max
        }

        g <- ggplot(my_data,aes(x=date,y=value))
        g <- g + geom_line(colour= beamaColours::get_corporate_blue(),stat = "identity",size= line_size)
        g <- g + facet_wrap( my_wrap)

        g <- g + geom_point(data=gtxt, aes(x=date, y=value),size=5,colour=line_col)
        g <- g + geom_point(data=gtxt, aes(x=date, y=value,colour=factor(data_days)),size=4)
        g <- g + geom_text( data=gtxt, aes(x=date, y=value,label= beamaUtils::set_decimal( value,dp)),vjust=-0.8,hjust=0.4,size=geom_txt_size ,colour= geom_txt_col)
        g <- g + theme(legend.position="none", strip.text = element_text( size = strip_txt_size ))



        g_range <- ggplot_build(g)$layout$panel_ranges[[1]]$y.range

        y_range <- g_range

        #cat("grange = c(", g_range[1]," , ", g_range[2],")\n")
        #cat("ydelta = c(", y_delta[1]," , ", y_delta[2],")\n")

        if( !( y_delta[1] == 0) ){

            if( g_range[1] < 0 ) {
                y_range[1] <- g_range[1] + y_delta[1]
            }else{
                y_range[1] <- g_range[1] - y_delta[1]
            }

        }

        if(!( y_delta[2] == 0)){
            if( g_range[2] < 0 ) {
                y_range[2] <- g_range[2] - y_delta[2]
            }else{
                y_range[2] <- g_range[2] + y_delta[2]
            }
        }

        #cat("yrange = c(", y_range[1]," , ", y_range[2],")\n")

        if( (y_range[1]*y_range[2]) <0 ){
            g <- g + geom_hline(yintercept = 0)
        }

        if( !( sum(y_delta) ==0) ) {
            g <- g + ylim( y_range )
        }

        if( !( sum(x_delta) == 0 ) ){
            g <- g + xlim( x_range )
        }

        g <- g + theme( legend.position="none" )

    }else{
        g <- ggplot(my_data,aes(x=date,y=value,colour=data_desc))
        g <- g + geom_line(aes(group=code_desc),size= line_size)
        g <- g+ theme(

            legend.position = c(lgpos$x, lgpos$y),
            legend.background = element_rect(fill = NA, colour = NA),#lgpos$fill
            legend.title=element_blank(),
            text = element_text(12),
            legend.position = "bottom"
        )

    }

    g <- g + labs(title=title,x="", y = my_ylab )

    if(add_intercept){
        g <- g + geom_hline(aes(yintercept=0))
    }

    if(is_themed){
        g <- g + theme_igray()
        g <- g + scale_colour_tableau("colorblind10")
        g <- g + theme(
            strip.background = element_rect(colour = strip_col, fill = strip_col),
            strip.text.x = element_text(colour = strip_fcol, size = strip_txt_size),

            legend.title = element_blank(),
            legend.position ="none"

        )
    }



    print(g)
    if(verbose) {   return(my_data) }
}

tg.get_growth_data <- function(
    code='m_elec,k646', ops = 'avg',
    select = 'MAT,MTH,QTR,YR,YTD,MAT1,MAT12,MAT4,MM1,MM12,MM3,QQ1,QQ4,YTD1,YTD12,YTD4,YY1',
    select_yr = c(2010,2020)
){

    #code <- "m_elec"

    my_codes <- code
    my_split <- strsplit(my_codes,",")[[1]]
    my_df <- NULL

    if( length( my_split ) > 1){

        my_df <- tg$new( my_split[1] )$get_growth_df( ops = ops, select = select, select_yr = select_yr)
        for(j in 2:length(my_split)){
            my_sub_df <- tg$new( my_split[ j ] )$get_growth_df( ops = ops, select = select, select_yr = select_yr)
            my_df <- rbind (my_df, my_sub_df)
        }

    }else{
        my_df <- tg$new( my_split[1] )$get_growth_df( ops = ops, select = select, select_yr = select_yr)
    }

    my_df$smart <- my_df$name
    for( name in names(SMART_LABELS)){

        my_df$smart[ which( my_df$name == name )] <- SMART_LABELS[[ name ]]

    }


    return(my_df)
}

tg.sync_currency <- function(src_code = 'USD', dst_code ='USDM', yr =1980){

    cat('Syncing started ',src_code,' ...\n')
    SQ <- storedQry::SQ$new( tg.get_db( 'trends' ) )$set_name( 'trends_data_sync_daily_to_month' )
    my_upd <- SQ$set_params(
        list(
            `@s_src_code` = toupper(src_code),
            `@s_dst_code` = toupper(dst_code),
            `@i_yr` = yr
        )
    )$qry_exec()



    cat('All done - syncing completed !!!\n')
}

tg.sync_currencies <- function( yr = lubridate::year(Sys.Date()) ){

    tg.sync_currency('USD','USDM', yr = yr)
    tg.sync_currency('EUR','EURM', yr = yr)
    tg.sync_currency('JPY','JPYM', yr = yr)
    tg.sync_currency('CNY','CNYM', yr = yr)
    tg.sync_currency('AUD','AUDM', yr = yr)
    tg.sync_currency('INR','INRM', yr = yr)
    tg.sync_currency('BRLM','BRLM', yr = yr)

    SQ <- storedQry::SQ$new( tg.get_db( 'trends' ) )$set_name("trends_update_periods")$qry_exec()
}

tg.get_info <- function(q='ABMI',is_code = F, is_link = F){

    qry <- paste0("select data_code,data_desc,data_frq from trends_meta where data_desc like '%",q,"%'")
    if(is_code){
        qry <- paste0("select * from trends_meta where data_code like '%",q,"%'")
    }

    if(is_link){
        qry <- paste0("select data_code,data_desc,data_src,data_src_url from trends_meta where data_code in ",beamaUtils::split_str(q) )

    }

    my_data <- tp_utils$new()$run_sql( qry )

    return(my_data)
}

tg.get_group_links <- function(grp){

    tg.get_info( tp_data$new( grp )$get_group(T) , is_link = T)

}

tg.get_brexit_soup_data <- function(){
    my_data <- tp_utils$new()$run_sql( "select * from brexit_soup where x not null" )
    return(my_data)
}

tg.plot_spider_ons <- function(
    code = 'ABMI',
    title= code,
    y1 = lubridate::year(Sys.Date())-2, y2=lubridate::year(Sys.Date()) ,
    verbose =FALSE, strip_txt_size = 12, is_themed = T,
    line_col = beamaColours::get_corporate_blue(),
    strip_col = beamaColours::get_blue(),
    strip_fcol = 'white',y_delta=c(0,0),x_delta=c(0,0)

){
    my_data <- onsR2::download(code)
    m_data <- my_data$m_data
    q_data <- my_data$q_data

    s_data <- m_data
    if(is.null( m_data)) { s_data <- q_data}

    tg$new(s_data)$plot_spider(
        title = title, y1= y1, y2 = y2, verbose = verbose,
        strip_txt_size = strip_txt_size, is_themed = is_themed,
        line_col = line_col, strip_col = strip_col,
        strip_fcol = strip_fcol, y_delta= y_delta, x_delta= x_delta
    )
}
