#extrafont::font_import()

ESMART_LABELS =  list(

    USDM="Exchange - US Dollar",
    EURM="Exchange - Euro",
    `ABMI-UKEA`="Gross Domestic Product",
    ABMI="Gross Domestic Product",
    K646="PPI - Input Prices",
    D7BT = "Consumer Price Index",
    IKBH = "Exports - Value",
    IKBI = "Imports - Value",
    BQKU = 'Exports - Volume Index',
    BQKR = 'Exports - Price Index',
    K222 = 'Index of Production',
    NPEL = 'Business Investment',
    `CT2AM-AW` = 'Construction Output',
    `CT2AM-ANW` = 'Construction New Work',
    `CT2AM-ARM` = 'Construction Repairs',
    S2KU = 'Index of Services',
    JVZ7 = 'PPI - Output Prices',
    K22A = 'Index of Manufacturing',
    CHAW = 'Retail Price Index',
    `OECD/MEI_CLI_LOLITOAA_GBR_M`='OECD Leading Indicator',
    J5EK = 'Retail Sales Index (volume)',
    KAB9 = 'Average Weekly Earnings',
    BQKS = 'Imports Price Index',
    L87S = 'Goods Exports to EU',
    L87U = 'Goods Imports from EU',
    L87M = 'Goods Exports to NonEU',
    L87O = 'Goods Imports from NonEU',
    DYDC = 'Employment',
    MGSX = 'Unemployment',
    JT27 = 'Manufacturing Output',
    `RPQM-UKEA` = 'Household & NPHIS Expenditure',
    `HAYO-UKEA` = 'Non-profit Ins. serving households',
    `NMRY-UKEA` = 'General Government expenditure',
    `NPQT-UKEA` = 'Gross Fixed Capital Formation',
    `CAFU-UKEA` = 'Inventories changes',
    `IKBK-UKEA` = 'Exports of Goods & Services',
    `IKBL-UKEA` = 'Imports of Goods & Services',
    `ABJR-UKEA` = 'Household Final Consumption'

)
GLANCE_INDICATORS = "USDM,EURM,K646,D7BT,IKBH,K222,BQKU,CT2AM-AW,BQKR,S2KU,JVZ7,K22A,KAB9,ABMI-UKEA,NPEL,J5EK,CHAW,BQKS,L87S,L87U,DYDC,JT27,CT2AM-ANW,CT2AM-ARM,OECD/MEI_CLI_LOLITOAA_GBR_M"

#' R6 class for Trends plot
#' td = Trends Plot
#'
tp <- R6::R6Class(
#    'tp',
    inherit = tp_utils,

    public = list(

        #data variables
        y1= 2010,
        y2=2020,
        m1=1,
        m2=12,
        d1=1,
        d2=31,
        code = NULL,
        df = NULL, #must have columns (yr, mth, data_code, pc, dy, data_desc)
        fx = 'm',
        fx_list = c('d','m','q','y','mt','qt','yt','ms','qs','ys','mc','qc','yc'),
        fx_level = 2,
        pc_list = c('0','1','3','4','12'),
        pc = '0',
        dtd1 = NULL,
        dtd2 = NULL,
        is_avg = FALSE,

        #### plot variables
        is_group = FALSE,
        legend_x = 0.5,
        legend_y = 0.5,
        title = NULL ,
        ylab = ' % Change ',
        colour = beamaColours::get_stats()[1],
        dt_breaks= NULL,
        dt_breaks_format= NULL,
        is_smooth=FALSE,
        dt_desc = '',
        freq = NULL,
        freq_default = 0,
        delta_x = 0,
        delta_y = c(0,0),
        skale =1,
        caption_size = 4,
        point_size = c(2,4),
        line_size = 1.3,
        stripe_text_size = 12,
        brexit_mode = FALSE,
        dp = 0, #decimals
        facet_cols = NULL,
        show_min_max = T,
        db_limit = list(yr=NULL, mth= NULL),
        yintercept = 0,
        #PAIRED = c("#377EB8","#E41A1C"),


        y_lim = numeric(2),

        initialize = function(code , db_name = NULL){

            super$initialize( db_name )

            if(!is.data.frame( code )){

                self$set_codes(code)

            }else{

                self$self_df(code)

            }

            self$set_brexit_text()

        }

        ,set_scale = function(value){

            if(!missing(value) && !is.null(value)){
                self$skale <- value
            }
            invisible(self)

        }

        ,set_delta_x = function(value){

            if(!missing(value) && !is.null(value)){
                self$delta_x <- value
            }
            invisible(self)

        }

        ,set_db_limit = function(value){

            if(!missing(value) && !is.null(value)){
                self$db_limit <- value
            }

            invisible(self)

        }

        ,set_caption_size = function(value){

            if(!missing(value) && !is.null(value)){
                self$caption_size <- value
            }
            invisible(self)

        }

        ,set_facet_cols = function(value){

            if(!missing(value) && !is.null(value)){
                self$facet_cols <- value
            }
            invisible(self)

        }

        ,set_min_max = function(value){

            if(!missing(value) && !is.null(value)){
                self$show_min_max <- value
            }
            invisible(self)

        }

        ,get_min_max = function(){

            return( self$show_min_max)
        }

        ,set_point_size = function(value){

            if(!missing(value) && !is.null(value)){
                self$point_size <- value
            }
            invisible(self)

        }

        ,set_dp = function(value){

            if(!missing(value) && !is.null(value)){
                self$dp <- value
            }
            invisible(self)

        }

        ,set_line_size = function(value){

            if(!missing(value) && !is.null(value)){
                self$line_size <- value
            }
            invisible(self)

        }

        ,set_stripe_size = function(value){
            if(!missing(value) && is.null(value)){
                self$stripe_text_size <- value
            }
            invisible( self )
        }

        ,set_brexit_mode = function(value){
            if(!missing(value) && !is.null(value)&& is.logical(value) ){
                self$brexit_mode <- value
            }
            invisible( self )
        }

        ,set_avg = function(value){

            if(!missing(value) && !is.null(value)){
                self$is_avg <- value
            }
            invisible(self)

        }

        ,set_fx = function(value){
            if(!missing(value) && !is.null(value)){
                self$fx <- value
                self$set_freq_default( value )
            }
            invisible(self)
        }

        ,set_date1 = function(value){

            if(!missing(value) && !is.null(value)){
                my_date <- as.Date(value)

                self$set_y1( lubridate::year( my_date))
                self$set_m1( lubridate::month( my_date))
                self$set_d1( lubridate::day( my_date))
                self$set_data_days(1)
            }
            invisible(self)
        }

        ,set_date2 = function(value){
            if(!missing(value) && !is.null(value)){
                my_date <- as.Date(value)

                self$set_y2( lubridate::year( my_date))
                self$set_m2( lubridate::month( my_date))
                self$set_d2( lubridate::day( my_date))
                self$set_data_days(2)
            }
            invisible(self)
        }

        ,set_date_range = function(value1,value2){

            if(!missing(value1) && !is.null(value1)){
                self$set_date1(value1)
            }

            if(!missing(value2) && !is.null(value2)){
                self$set_date2(value2)
            }
            invisible(self)
        }

        ,set_data_days = function(value){

            if(value==1){
                self$dtd1 <- beamaUtils::ddays( self$y1, self$m1, self$d1, d360=31 )
            }else{
                self$dtd2 <- beamaUtils::ddays( self$y2, self$m2, self$d2, d360=31 )
            }
            invisible(self)
        }

        ,set_y1 = function(value){
            if(!missing(value) && !is.null(value)){
                self$y1 <- value
            }
            invisible(self)
        }

        ,set_y2 = function(value){
            if(!missing(value) && !is.null(value)){
                self$y2 <- value
            }
            invisible(self)
        }

        ,set_m1 = function(value){
            if(!missing(value) && !is.null(value)){
                self$m1 <- value
            }
            invisible(self)
        }

        ,set_m2 = function(value){
            if(!missing(value) && !is.null(value)){
                self$m2 <- value
            }
            invisible(self)
        }

        ,set_d1 = function(value){
            if(!missing(value) && !is.null(value)){
                self$d1 <- value
            }
            invisible(self)
        }

        ,set_d2 = function(value){
            if(!missing(value) && !is.null(value)){
                self$d2 <- value
            }
            invisible(self)
        }

        ,set_codes = function(value){

            if(!missing(value) && !is.null(value)){
                self$code <- private$split_str( value )
            }
            invisible(self)

        }

        ,set_df = function(value){

            if(!missing(value) && !is.null(value)){

                self$df <- value

            }
            invisible(self)
        }


        ,set_pc = function(value){
            if(!missing(value) && !is.null(value)){
                self$pc <- value
            }
            invisible(self)
        }

        ,set_yintercept = function(value){
            if(!missing(value) && !is.null(value)){
                self$yintercept <- value
            }
            invisible(self)
        }

        ,build_sql = function(){

            my_fx <- self$fx
            my_avg <- self$is_avg
            my_sql <- NULL
            my_group <- NULL
            my_order <- NULL


            if(my_fx=='q'){

                if(my_avg){

                    my_sql <- "select a.yr, a.qtr, a.data_code, b.data_desc, avg(a.data_value)  as value from trends_data a, trends_meta b "
                    my_group <- " group by yr,qtr, a.data_code"
                    my_order <- " order by yr,qtr, a.data_code, b.data_desc "

                }else{

                    my_sql <- "select a.yr, a.qtr, a.data_code, b.data_desc, sum(a.data_value)  as value from trends_data a, trends_meta b "
                    my_group <- " group by yr,qtr, a.data_code "
                    my_order <- " order by yr,qtr, a.data_code, b.data_desc "

                }

            }else if(my_fx=='m'){

                if(my_avg){

                    my_sql <- "select a.yr, a.mth, a.data_code, b.data_desc, avg(a.data_value)  as value from trends_data a, trends_meta b "
                    my_group <- "group by yr,mth,a.data_code"
                    my_order <- " order by yr,mth,a.data_code, b.data_desc "

                }else{

                    my_sql <- "select a.yr, a.mth, a.data_code, b.data_desc,sum(a.data_value)  as value from trends_data a, trends_meta b "
                    my_group <- " group by yr,mth, a.data_code"
                    my_order <- " order by yr,mth, a.data_code, b.data_desc "

                }

            }else if(my_fx=='y'){

                if(my_avg){

                    my_sql <- "select a.yr,1 as mth, a.data_code, b.data_desc, avg(a.data_value)  as value from trends_data a, trends_meta b "
                    my_group <- " group by yr, a.data_code "
                    my_order <- " order by yr, a.data_code, b.data_desc "

                }else{

                    my_sql <- "select a.yr, 1 as mth, a.data_code, b.data_desc, sum(a.data_value)  as value from trends_data a, trends_meta b"
                    my_group <- " group by yr, a.data_code "
                    my_order <- " order by yr, a.data_code, b.data_desc "

                }

            }else   if(my_fx=='qt'){

                my_sql <- "select yr,qtr,sum(data_value)  as value from trends_data "
                my_group <- " group by yr,qtr "
                my_order <- " order by yr,qtr "

            }else if(my_fx=='mt'){

                my_sql <- "select yr,mth,sum(data_value)  as value from trends_data "
                my_group <- " group by yr,mth"
                my_order <- " order by yr,mth "

            }else if(my_fx=='yt'){

                my_sql <- "select yr,sum(data_value)  as value from trends_data "
                my_group <- " group by yr "
                my_order <- " order by yr "

            }else if(my_fx=='d'){

                my_sql <- "select a.yr, a.mth, a.dy, a.data_code, b.data_desc, a.data_value  as value from trends_data a, trends_meta b "
                my_order <- " order by yr,mth,dy, a.data_code, b.data_desc "

            }else if (my_fx=='ms'){

                my_sql <- paste0("SELECT yr, mth,substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
                my_group <- " group by yr,mth,wrap "

            }else if (my_fx=='qs'){

                my_sql <- paste0("SELECT yr, qtr,substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
                my_group <- " group by yr,qtr,wrap "

            }else if (my_fx=='ys'){

                my_sql <- paste0("SELECT yr,1 as mth, substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
                my_group <- " group by yr,wrap "

            }else if (my_fx=='mc'){

                my_sql <- paste0("SELECT yr, mth,(substr(data_code,instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data")
                my_group <- " group by yr,mth,wrap "

            }else if (my_fx=='qc'){

                my_sql <- paste0("SELECT yr, qtr,(substr(data_code, instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data ")
                my_group <- " group by yr,qtr,wrap "

            }else if (my_fx=='yc'){

                my_sql <- paste0("SELECT yr,1 as mth, ( substr(data_code, instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data")
                my_group <- " group by yr,wrap "

            }


            my_prd_len <- nchar(my_fx)
            qry_where <-" where "
            qw_code <- qw_yr <- qw_mth <- sql_where <- ""
            TRENDS_DATA_WHERE = " and a.data_code = b.data_code "

            if(my_prd_len > 1){

                qw_code <- base::paste0(" data_code in ", self$code )
                qw_yr <- base::paste0(" and (yr between ", self$y1," and ", self$y2,")")
                qw_mth <- base::paste0(" and (mth between ", self$m1," and ", self$m2,")")
                sql_where <- base::paste0( qry_where, qw_code, qw_yr, qw_mth )

            }else{

                qw_code <- base::paste0(" a.data_code in ", self$code )
                qw_yr <- base::paste0(" and (a.yr between ", self$y1," and ", self$y2 ,")")
                qw_mth <- base::paste0(" and (a.mth between ", self$m1," and ", self$m2,")")
                sql_where <- base::paste0( qry_where, qw_code, qw_yr, qw_mth, TRENDS_DATA_WHERE)

            }

            # q_code <- paste0(" data_code in ", self$code )
            # q_yr <- paste0(" and (yr between ", self$y1, " and ", self$y2, ")")
            # q_mth <- paste0(" and (mth between ", self$m1 ," and ", self$m2, ")")
            # q_where <- paste0(" where ",q_code,q_yr,q_mth)


            my_sql <- paste0( my_sql, sql_where, my_group, my_order)
            return(my_sql)
        }

        ,get_data = function( encode = T){

            if(! is.null( self$code)){
                    my_data <- self$run_sql( self$build_sql() )
                    my_fx <- self$fx
                    my_pc <- my_k <- as.numeric(self$pc)


                    if(nrow(my_data)>0){

                        my_data$value <- round(as.numeric(as.character(my_data$value)),4)

                        if((my_fx=="m") || (my_fx=='mt')|| (my_fx=='ms')|| (my_fx=='mc')){
                            my_data$dy <- 1
                            if(my_fx=='mt'){
                                my_data$data_code <- self$code
                                my_data$data_desc <- 'dummy-desc'
                            }
                            if((my_fx=="ms")|| (my_fx=="mc")){
                                my_data$data_code <- my_data$wrap
                                my_data$data_desc <- my_data$data_code
                            }
                        }
                        if((my_fx=="q") || (my_fx=="qt")|| (my_fx=="qs")|| (my_fx=="qc")){
                            my_data$dy <- 1
                            my_data$mth <- my_data$qtr*3
                            if(my_fx=='qt'){
                                my_data$data_code <- self$code
                                my_data$data_desc <- 'dummy-desc'
                            }

                            if((my_fx=="qs")|| (my_fx=="qc")){
                                my_data$data_code <- my_data$wrap
                                my_data$data_desc <- my_data$data_code
                            }
                        }

                        if((my_fx=="y")||(my_fx=="yt")||(my_fx=="ys")||(my_fx=="yc")){
                            my_data$dy <- 1
                            my_data$mth <-1
                            if(my_fx=='yt'){
                                my_data$data_code <- self$code
                                my_data$data_desc <- 'dummy-desc'
                            }
                            if((my_fx=="ys")|| (my_fx=="yc")){
                                my_data$data_code <- my_data$wrap
                                my_data$data_desc <- my_data$data_code
                            }
                        }

                        my_data<- dplyr::arrange(my_data,yr,mth,dy,data_desc)



                        my_data$pc <- NULL
                        if(my_pc > 0){

                            #my_k <- self$freq_default

                            #if(!is.null(self$freq)){ my_k <- self$freq}

                            my_data$pc <- with(
                                my_data,
                                ave(
                                    value,
                                    data_code,
                                    FUN=function(x){quantmod::Delt(x,k= my_k)}
                                )
                            )*100

                        }else{

                            my_data$pc <- my_data$value
                        }

                    }#nrow

                    self$df <- my_data
            }

            if(encode && !is.null( self$df )) {

                self$df <- private$en_code(  self$df )

            }

            return( self$df  )


            #

        }#get_data

        ,set_freq = function(value){

            if(!missing(value) && !is.null(value)){

                self$freq <- value

            }
            invisible(self)
        }

        ,set_freq_default = function(value){

            if(!missing(value) && !is.null(value)){
                self$freq_default <- switch(value,
                                            'd' =1,
                                            'm' = 12,
                                            'q' = 4,
                                            'y'=1)
            }
            invisible(self)
        }

        ,set_group = function(value){
            if(!missing(value) && !is.null(value)){
                self$is_group <- value
            }
            invisible(self)
        }

        ,set_title = function(value){
            if(!missing(value) && !is.null(value)){
                self$title <- value
            }
            invisible(self)
        }

        ,set_legend_xy = function(x,y){
            if(!missing(x) && !is.null(x)){
                self$legend_x <- x
            }
            if(!missing(y) && !is.null(y)){
                self$legend_y <- y
            }
            invisible(self)
        }

        ,set_ylab = function(value){
            if(!missing(value) && !is.null(value)){
                self$ylab <- value
            }
            invisible(self)
        }

        #' set_colour
        #'
        #' @param value
        #'
        #' @return
        #' @export
        #'
        #' @examples
        ,set_colour = function(value){
            if(!missing(value) && !is.null(value)){
                self$colour <- value
            }
            invisible(self)
        }

        ,set_breaks = function(value ){
            if(!missing(value) && !is.null(value)){
                self$dt_breaks <- value
            }
            invisible(self)
        }

        ,set_breaks_fmt = function(value){
            if(!missing(value) && !is.null(value)){
                self$dt_breaks_format <- value
            }
            invisible(self)
        }

        ,set_smooth = function(value){
            if(!missing(value) && !is.null(value)){
                self$is_smooth <- value
            }
            invisible(self)
        }

        ,set_data_desc = function(value){
            if(!missing(value) && !is.null(value)){
                self$dt_desc <- value
            }
            invisible(self)
        }

        ,set_ylim = function(y1,y2){

            if(!missing(y1) && !is.null(y1)){
                self$y_lim[1] <- y1
            }
            if(!missing(y2) && !is.null(y2)){
                self$y_lim[2] <- y2
            }

            invisible(self)
        }

        ,set_delta_y = function(y1,y2){

            if(!missing(y1) && !is.null(y1)){
                self$delta_y[1] <- y1
            }
            if(!missing(y2) && !is.null(y2)){
                self$delta_y[2] <- y2
            }

            invisible(self)
        }

        ,get_line_colours = function(stats_first = FALSE){
            if(stats_first){
                c(
                    beamaColours::get_stats()
                    ,RColorBrewer::brewer.pal(9,"Set1")[-c(6)]
                    ,RColorBrewer::brewer.pal(8,"Set2")
                )
            }else{

                c(
                    RColorBrewer::brewer.pal(9,"Set1")[-c(6)]
                    ,RColorBrewer::brewer.pal(8,"Set2")
                )

            }
        }

        ,get_pc_ylab = function(){

            my_ylab <- NULL
            my_pc <- as.numeric(self$pc)
            my_frq <- self$freq
            #cat("Freq=",my_frq,"\n")

            if(!(my_pc ==0 )){

                if(self$fx=="d"){

                    my_ylab <- paste0(k,' day ',self$ylab)

                }else if((self$fx=="m") || (self$fx=='mt')){

                    my_ylab <- paste0(my_frq,' month ',self$ylab)

                }else if((self$fx=="q") || (self$fx=='qt')){

                    my_ylab <- paste0(my_frq,' quarter ',self$ylab)

                }else if((self$fx=="y") || (self$fx=='yt')){

                    my_ylab <- " Yearly % change "

                }
            }
            return(my_ylab)
        }

        ,plot_pc = function(brewer_set = "Set1", ytitle=NULL, dazzle=FALSE, encode = T,
                            is_themed = T,
                            strip_col = beamaColours::get_blue(),
                            strip_fcol = 'white', is_smart = F, smart_labels = NULL,
                            title_font_size = 14, min_max_days = 30, return_data = F, return_plot= F,
                            by_year = FALSE,set_dy_null = FALSE, font_text = "Museo 300", font_title = "Museo 500"
                ){

            require(ggthemes)
            require(magrittr)
            require(ggplot2)

            my_data <- NULL


            my_data<- self$get_data( encode = encode)


             #return(my_data)

            my_pc <- as.numeric(self$pc)
            my_frq <- self$freq
            my_ylab <- self$get_pc_ylab()

            if( !is.null( ytitle) ) {
                my_ylab <- ytitle
            }


            if( (trimws(my_data$data_desc[1])=='dummy-desc') && (nchar(self$dt_desc)>0)){
                my_data$data_desc <- self$dt_desc
            }

            my_data$smart <- my_data$data_desc

            #return(my_data)

            if(is_smart){


                if( !is.null( smart_labels ) ){

                    for( name in names(smart_labels)){

                        my_data$smart[ which( my_data$data_code == name )] <- smart_labels[[ name ]]

                    }
                }
            }

            my_data <- dplyr::filter( my_data, !is.na(pc) )
            my_data$date <- as.Date( paste( my_data$yr, my_data$mth, my_data$dy, sep="-"))
            my_data$pc <- my_data$pc/self$skale

            mytext <- dplyr::filter( my_data, yr==self$y2, mth==self$m2)

            gmin <- vmin <- NULL
            gmax <- vmax <- NULL

            g <- NULL

            # moved from is_group = FALSE

            dy_is_null <- is.null( my_data$dy)
            if(set_dy_null){

                dy_is_null <- TRUE
            }

            #return( my_data)

            if(dy_is_null){

                my_data$data_days <- with( my_data, beamaUtils::ddays(yr = yr , mth = mth , d360 = 31 ))

            }else{

                my_data$data_days <- with( my_data, beamaUtils::ddays( yr = yr, mth = mth , dy = dy, d360 = 31 ) )

            }

            if( !is.null(self$db_limit$yr) ){

                ### test test test
                #cat('db_limit: yr=',self$db_limit$yr,", mth = ", self$db_limit$mth,"\n")

                my_data <- dplyr::filter(
                    my_data, data_days <= beamaUtils::ddays( self$db_limit$yr, self$db_limit$mth,d360=31)
                )

                ### test test test
                #return( my_data)

            }

            gmin <-  dplyr::filter(
                dplyr::group_by(my_data ,data_code) ,
                data_days==min(data_days)
            )
            gmax <-  dplyr::filter(
                dplyr::group_by(my_data,data_code),
                data_days==max(data_days)
            )


            vmin <-  dplyr::filter(
                dplyr::group_by(my_data ,data_code) ,
                pc== min( pc )
            )
            vmin <- dplyr::distinct(vmin, data_code, .keep_all = TRUE)


            vmax <-  dplyr::filter(
                dplyr::group_by(my_data,data_code),
                pc==max( pc )
            )

            vmax <- dplyr::distinct(vmax,data_code, .keep_all = TRUE)

            # if(nrow(vmax) > 1)
            # {
            #     vmax <- vmax[ 1, ]
            # }

            gtxt <- rbind(gmin,gmax)
            vtxt <- rbind(vmin,vmax)
            ## ends is_group = FALSE

            if(!self$is_group){




                #return(gtxt)
                if(!dazzle){

                    g <- ggplot(my_data,aes(x=date,y=pc))
                    g <- g + geom_line(size=self$line_size,colour = self$colour)



                }else{
                    g <- ggplot(my_data,aes(x=date,y=pc,colour=factor(data_desc)))

                    g <- g + geom_line(size = self$line_size, aes(colour=data_desc))
                    g <- g + scale_color_manual(values= self$get_line_colours())
                    g <- g + guides(colour=FALSE)
                }

                if( is.null(self$facet_cols) ){

                    g <- g + facet_wrap( ~ smart)

                }else{

                    g <- g + facet_wrap( ~ smart, ncol = self$facet_cols )

                }


                g <- g + geom_point( data=gtxt, aes( x = date, y = pc ) , size = self$point_size[2], colour = beamaColours::get_line_colour())
                g <- g + geom_point( data=gtxt, aes( x = date, y = pc ) , size = self$point_size[1], colour = beamaColours::get_pink())
                g <- g + geom_text(  data=gtxt, aes( x = date, y = pc, label = private$set_decimal( pc, self$dp) ),vjust=-0.8,hjust=0.4,size= self$caption_size,colour = beamaColours::get_smooth_colour())

                g <- g + geom_point( data=vtxt, aes( x = date, y = pc ) , size = self$point_size[2]*0.75, colour = beamaColours::get_line_colour())
                g <- g + geom_point( data=vtxt, aes( x = date, y = pc ) , size = self$point_size[1]*0.75 , colour = beamaColours::get_gray())




                if( self$show_min_max ){

                    min_days <- gmin$data_days[ 1 ]
                    max_days <- gmax$data_days[ 1 ]

                    vmin$min <- abs( vmin$data_days - min_days )
                    vmin$max <- abs( vmin$data_days - max_days )

                    vmax$min <- abs( vmax$data_days - min_days )
                    vmax$max <- abs( vmax$data_days - max_days )

                    vmin <- dplyr::filter( vmin, min > min_max_days, max > min_max_days  )
                    vmax <- dplyr::filter( vmax, min > min_max_days, max > min_max_days )

                    g <- g + geom_text(  data=vmin, aes( x = date, y = pc, label = private$set_decimal( pc, self$dp) ),vjust= 1.4,hjust=0.6,size= self$caption_size,colour = beamaColours::get_smooth_colour())
                    g <- g + geom_text(  data=vmax, aes( x = date, y = pc, label = private$set_decimal( pc, self$dp) ),vjust=-0.6,hjust=0.4,size= self$caption_size,colour = beamaColours::get_smooth_colour())

                }


                g <- g + theme(legend.position="none", strip.text = element_text(size = self$stripe_text_size ) )


            }else{



                g <- ggplot(my_data, aes(x=date,y=pc,colour=data_code))


                g <- g + geom_line( aes(group=data_code),size = self$line_size)

                g <- g+ theme(

                    legend.position = c(self$legend_x, self$legend_y),
                    legend.background = element_rect(fill = NA, colour = NA),#lgpos$fill
                    legend.title=element_blank(),
                    text = element_text(size= self$stripe_text_size)

                )
                g <- g + scale_colour_brewer( palette = brewer_set )



            }

            if(!is.null( self$title )){

                g <- g + ggtitle( self$title )
            }

            g <- g + labs( x="", y = my_ylab)
            g <- g + geom_hline( aes(yintercept= self$yintercept) )


            if( !(self$delta_x == 0) ){

                min_date <- as.Date( paste( gmin$yr[1], gmin$mth[1], 28,sep='-'))
                max_date <- as.Date( paste( gmax$yr[1], gmax$mth[1], 28,sep='-'))


                lubridate::month(max_date) <- lubridate::month(max_date) + self$delta_x
                lubridate::month(min_date) <- lubridate::month(min_date) - self$delta_x

            }

            if(! is.null(self$dt_breaks)  ){
                #cat('dt_breaks not null')
                if(! is.null(self$dt_breaks_format)){

                    if(!(self$delta_x == 0) ){
                        #cat("self$delta_x not zero")
                        g <- g + scale_x_date(date_breaks = self$dt_breaks, date_labels = self$dt_breaks_format, limits = c(min_date,max_date))
                    }else{
                        g <- g + scale_x_date(date_breaks = self$dt_breaks, date_labels = self$dt_breaks_format)
                    }

                }else{

                    if(!(self$delta_x == 0) ){
                        g <- g + scale_x_date(date_breaks = self$dt_breaks, limits = c(min_date,max_date))
                    }else{
                        g <- g + scale_x_date(date_breaks = self$dt_breaks)
                    }
                }

            }else{
                #cat('dt_breaks is null')


                if(!(self$delta_x == 0) ){
                    #cat("self$delta_x not zero")
                    g <- g + scale_x_date( limits = c(min_date,max_date))
                }


            }


            if(self$brexit_mode == T){
                #cat("brexit mode true")
                g <- g + geom_vline( aes(xintercept = as.numeric(as.Date( BREXIT_POINT )) ), colour = beamaColours::get_pink(), linetype = 'dashed', size= 1  )

            }else{
                #cat("brexit mode false")
            }

            if( self$brexit_label == T){

                bxl <- self$get_brexit_text()

                #g <- g + geom_text(data = bxl, aes(x = x, y= y, label = label, angle = angle, size = size), colour = beamaColours::get_pink())
                g <- g + annotate(
                    geom = 'text',
                    x = bxl$x, y = bxl$y, label = bxl$label ,
                    color = beamaColours::get_pink(),
                    angle = bxl$angle,
                    size = bxl$size
                )
            }

            g_range <- ggplot_build(g)$layout$panel_ranges[[1]]$y.range

            y_range <- g_range


            if( !( self$delta_y[1] == 0) ){

                if( g_range[1] < 0 ) {
                    y_range[1] <- g_range[1] + self$delta_y[1]
                }else{
                    y_range[1] <- g_range[1] - self$delta_y[1]
                }

            }

            if(!( self$delta_y[2] == 0)){
                if( g_range[2] < 0 ) {
                    y_range[2] <- g_range[2] - self$delta_y[2]
                }else{
                    y_range[2] <- g_range[2] + self$delta_y[2]
                }
            }

            if( !( sum(self$delta_y) == 0) ) {
                g <- g + ylim( y_range )
            }

            ##


            if(is_themed){
                g <- g + theme_igray()
                g <- g + scale_colour_tableau("colorblind10")
                g <- g + theme(

                    strip.background = element_rect(colour = strip_col, fill = strip_col),

                    legend.position = "none",
                    legend.title = element_blank(),
                    text = element_text(family= font_text, face="plain"),
                    plot.title = element_text(family = font_title, face="plain", size = title_font_size),
                    strip.text.x = element_text(family= font_text, face="plain", colour = strip_fcol, size= self$stripe_text_size)

                )
            }

            if(self$is_group){
                g <- g + theme(legend.position="bottom")
            }
            print(g)

            if(return_plot){
                return( g )
            }

            if( return_data ){
                return(list (my_data = my_data, gmax = gmax, gmin = gmin, vmin = vmin, vmax = vmax) )
            }
        }



        ,plot_dt= function(ytitle='',brewer_set = "Set1", encode=T){


            mycolour <- brewer_set

            mydata<- self$get_data( encode = encode)
            myfx <- self$fx
            my_ylab <- NULL

            if(myfx=="d"){
                my_ylab <- paste0(ytitle , '(daily)')
            }else if((myfx=="m") || (myfx=='mt')){

                my_ylab <- paste0(ytitle, ' - monthly ')

            }else if((myfx=="q") || (myfx=='qt')){
                my_ylab <- paste0(ytitle,' - quarterly ')

            }else if((myfx=="y") || (myfx=='yt')){

                my_ylab <- paste0(ytitle,' - yearly ')

            }

            my_scale <- self$skale
            mydata$value <- as.numeric(mydata$value)/my_scale
            yscale <-''

            if(my_scale==1e3){
                yscale <- paste0('(thousands)')
            }
            else if(my_scale==1e6){
                yscale <- paste0('(millions)')
            }
            else if(my_scale==1e9){
                yscale <- paste0('(billions)')
            }

            mydata$date <- as.Date(paste(mydata$yr,mydata$mth,mydata$dy,sep="-"))

            is_brewer <- (length(grep('#',mycolour))==0)



            g <- NULL
            if(!self$is_group){

                mydata$data_days <- with(mydata, beamaUtils::ddays(yr,mth,dy, d360=31 ))
                gmin <-  dplyr::filter( dplyr::group_by(mydata,data_code), data_days==min(data_days))
                gmax <-   dplyr::filter(dplyr::group_by(mydata,data_code),data_days==max(data_days))
                gtxt <- rbind(gmin,gmax)


                g <- ggplot2::ggplot(mydata,ggplot2::aes(x=date,y=value))
                g <- g+ggplot2::facet_wrap( ~ data_desc)
                g <- g+ggplot2::guides(colour=FALSE)

                # g <- g+ geom_point(data=gtxt, aes(x=date,y=pc) ,size=4, colour = beamaColours::get_line_colour())
                # g <- g+ geom_point(data=gtxt, aes(x=date,y=pc) ,size=2, colour = beamaColours::get_pink())
                # g <- g+ geom_text(data=gtxt, aes(x=date,y=pc,label=private$set_decimal(pc,1)),vjust=-0.8,hjust=0.4,size=4,colour = beamaColours::get_smooth_colour())

                g <- g +ggplot2::geom_line(size=1.3,colour=self$colour)

                g <- g + ggplot2::geom_point( data = gtxt, ggplot2::aes( x = date, y = value), size=4,colour=beamaColours::get_line_colour())
                g <- g + ggplot2::geom_point( data = gtxt, ggplot2::aes( x = date, y = value, colour = factor( data_days ) ),size=2, colour = beamaColours::get_pink())
                g <- g + ggplot2::geom_text( data = gtxt, ggplot2::aes( x = date, y = value, label = private$set_decimal(value, self$dp)), vjust = -0.8, hjust = 0.4, size = 4, colour = beamaColours::get_smooth_colour())
                g <- g + ggplot2::theme( legend.position = "none" )


                # if(is_brewer){
                #
                #   g<- g+geom_line(size=1.4,aes(colour=data_code))
                #   g <- g + scale_colour_brewer(palette=mycolour)
                #
                # }else{

                # }

            }else{
                g <- ggplot2::ggplot(mydata,ggplot2::aes(x=date,y=value,colour=data_code))
                g <- g + ggplot2::geom_line( ggplot2::aes(group=data_code),size=1.3)
                g <- g + ggplot2::scale_colour_brewer(palette=mycolour)
                g <- g + ggplot2::theme(
                    legend.position = c(lgpos$x, lgpos$y),
                    legend.background = element_rect(fill = NA, colour = NA),#lgpos$fill
                    legend.title=element_blank(),
                    text = element_text(12)
                )

            }
            #
            #     if( !(self$y_lim[1] == 0) ){
            #       g <- g + ylim( self$y_lim )
            #     }

            if( !(self$delta_x == 0) ){

                min_date <- as.Date( paste( gmin$yr[1], gmin$mth[1], 28,sep='-'))
                max_date <- as.Date( paste( gmax$yr[1], gmax$mth[1], 28,sep='-'))

                #return(list(min_date,max_date))

                lubridate::month(max_date) <- lubridate::month(max_date) + self$delta_x
                lubridate::month(min_date) <- lubridate::month(min_date) - self$delta_x



                g <- g + ggplot2::xlim( min_date,max_date )
            }
            g <- g + ggplot2::labs(title=self$title, x="",y=paste(my_ylab,yscale))
            if(self$is_smooth){
                g <- g + ggplot2::geom_smooth(method='lm',colour='red')
            }

            if(!is.null(self$yintercept)){
                g <- g + geom_hline(aes(yintercept=self$yintercept))
            }

            if(BREXIT_MODE){

                g <- g + geom_vline( aes(xintercept = as.numeric(as.Date( BREXIT_POINT )) ), colour = beamaColours::get_pink(), linetype = 'dashed', size= 1  )

            }

            print(g)
            return(mydata)
        }

        ,plot_bar = function(yr, mth , y_size=10, show_title=TRUE, verbose=TRUE,flip=TRUE){
            require(ggplot2)

            my_data <- self$get_data()
            my_yr <- yr
            my_mth <- mth
            my_data <- dplyr::filter( my_data, yr == my_yr, mth == my_mth)
            my_data <- dplyr::arrange(my_data, pc)
            my_data$data_desc <- factor( my_data$data_desc, as.character(my_data$data_desc)  )

            my_data$col <- ( my_data$pc >=0)
            my_data$lcol <- "#0072B2"
            my_data[ my_data$pc < 0, "lcol"] <- "red"

            fill_colour <- NULL
            if(nrow( dplyr::filter(my_data,col==FALSE) ) ==0){
                fill_colour <-  c("#0072B2","#0072B2")
            }else{
                fill_colour <- c("#FF9999","#0072B2")
            }


            pc_max <- 1.2*max(my_data$pc)#+0.5
            pc_min <- min( my_data$pc )


            g <- ggplot( my_data, aes( x=data_desc, y=pc, fill=col))+ geom_bar(stat='identity')
            g <- g + geom_text( aes(label=sprintf("%.1f",pc) ),vjust=0.5,hjust=0)
            g <- g + scale_fill_manual(values=fill_colour,guide=FALSE)

            g <- g + theme(
                axis.text.y =element_text( colour = my_data$lcol, size = y_size),
                plot.title  =element_text( size = 14, face = "bold")
            )

            if(flip){
                g <- g + coord_flip()
            }

            y_range_is_zero <- ((self$yrange[1] + self$yrange[2]) == 0)

            if( F ){ #!length(y_range_is_zero)== 0

                y_min <- my_min <- min( my_data$pc )
                y_max <- my_max <- max( my_data$pc )

                if(my_min >0){

                    y_min <- y_min * ( 100 - self$yrange[1] ) / 100

                }else{

                    y_min <- y_min * ( 100 + self$yrange[1] ) / 100

                }

                if(my_max >0){

                    y_max <- y_max * ( 100 + self$yrange[2] ) / 100

                }else{

                    y_max <- y_max * ( 100 - self$yrange[2] ) / 100

                }

                #cat("setting ylim y_min =",y_min," y_max = ", y_max,"\n")


                g <- g + ylim( y_min, y_max )

            }



            g <- g + ylab("YOY % change") + xlab("")

            #if(show_title){g <- g + ggtitle(my_title)}

            print(g)
            if(verbose){return(my_data)}


        }

        ,add_to_db = function(grp,df){

            if(missing(df)){
                cat("Please supply data frame \n")
                return(NULL)
            }
            my_df <- df
            my_df$grp <- grp

            my_sql <- sprintf(
                "insert into trends_groups (grp,code,description,freq) values ('%s','%s','%s','%s');",
                my_df$grp, my_df$code, my_df$desc, my_df$freq
            )

            self$run_sql(my_sql)


        }

        ,to_clipboard = function( x, row.names=FALSE, col.names=TRUE, ...) {
            write.table( x,"clipboard", sep="\t", row.names=row.names, col.names=col.names, ...)
        }

    )
    ,private = list(



    )

)

### NON CLASS FUNCTIONS ###

#' tp.view_ons_code
#'
#' @param code = code of series to plot, multiple codes is a single string with commas between codes eg "ABC,CDE,FGH"
#' @param is_growth = logical indicating growth plot or actual series data
#' @param select  =
#' @param select_yr
#' @param selec
#'
#' @return
#' @export
#'
#' @examples
tp.view_ons_code <- function( code='ABMI', is_growth = F, select_yr=c(2010,2020), ops = 'avg', is_themed = T,
                              select = 'MAT,MTH,QTR,YR,YTD,MAYTD,MAT1,MQT1,MAT12,MAT4,MM1,MM12,MM3,QQ1,QQ4,YTD1,YTD12,YTD4,YY1,MAYTDM1',
                              title_font_size = 14){
    #source('global.R')

    my_data <- onsR2::download( code = code)
    md <- my_data$m_data
    qd <- my_data$q_data
    yd <- my_data$y_data

    dt <- md

    if( is.null(dt)){
        if(!is.null(qd)){
            dt <- qd
        }else{
            dt <- yd
        }
    }

    bt <- tg$new(x=dt)

    bt$plot( is_growth = is_growth, title = paste0( trimws( my_data$title ),' (',code,')' ), select = select , select_yr = select_yr, ops = ops, is_themed = is_themed, title_font_size = title_font_size)

}

#' View Code
#'
#' @param code
#' @param is_growth
#' @param select
#' @param select_yr
#'
#' @return
#' @export
#'
#' @examples
tp.view_code <- function( code='ABMI', is_growth = F, select=NULL, select_yr=c(2010,2020), is_themed = T, ops = 'avg', title_font_size = 14, title = NULL, dp = 1){
    #source('global.R')

    bt <- tg$new(x=code)
    my_title <- title
    if(is.null( title)){ my_title <- code }
    bt$plot( is_growth = is_growth, title = my_title, select = select , select_yr = select_yr, is_themed = is_themed, ops = ops, title_font_size = title_font_size, dp = dp)

}

tp.fred_view_steel_code <- function( row_id ){

    require(fredr)
    fredr_key('2b51779f15571ec088a3f4b158054d0a')
    abc <- fredr::fredr_search(search_text = "steel")
    aaa <- sqldf::sqldf("select * from abc where title like '%steel%'")
    bbb <- sqldf::sqldf("select * from aaa where title not like '%discontinued%'")
    ccc <- sqldf::sqldf("select * from bbb where title like '%price%'")
    ddd <- sqldf::sqldf("select * from ccc where observation_end like '2017%' and frequency='Monthly'")

    n_rows <- nrow(ddd)
    my_row_id <- row_id
    if(row_id> n_rows){ my_row_id <- n_rows}

    cde <- fredr::fredr_series(
        series_id = ddd$id[ my_row_id ],
        observation_start = ddd$observation_start[ my_row_id ],
        frequency = 'm'
    )
    tg$new(cde)$plot( is_growth = T,select="MM1,MM12,MAT12,YTD12",title = ddd$title[ my_row_id ],y_delta=c(0,1))
    return( ddd$id[ my_row_id ])
}#tp.fred_view_steel_code(1)

tp.view_ons_spider<- function(
    code='ABMI',
    y1 = lubridate::year(Sys.Date())-2,
    y2=lubridate::year(Sys.Date()),
    k = NULL, fx = NULL,
    y_delta = c(0,0),
    show_growth_caps = F,
    caps_x = c(1, 1, -1, -1),
    caps_y = c(1,-1, -1,  1),
    caps_lbl = c('EXPANSION','RECOVERY','CONTRACTION','SLOWDOWN'),
    caps_col = gray.colors(10)[7],
    caps_size = 8


){
    my_data <- onsR2::download( code = code)
    my_ts <- my_data$m_data
    my_k <- k
    my_fx <- fx


    if( is.null( my_ts)){
        my_ts <- my_data$q_data
    }

    my_plot <- tg$new( my_ts )

    if(is.null( k )){
        my_k <- my_plot$data_freq
    }

    if( is.null( fx )){

        if(my_k == 12){
            my_fx <- 'mth'
        }else if(my_k == 4){
            my_fx <- 'qtr'
        }

    }

    my_plot$plot_spider(
        title=my_data$title, y1= y1, y2 = y2, k = my_k,
        fx = tolower( my_fx ), y_delta = y_delta,
        show_growth_caps = show_growth_caps,
        caps_x = caps_x,
        caps_y = caps_y,
        label = caps_lbl,
        size = caps_size,
        colour = caps_col
    )
}

tp.view_spider<- function(
    code='ABMI',
    title = NULL,
    y1 = lubridate::year(Sys.Date())-2,
    y2=lubridate::year(Sys.Date()),
    k = NULL,fx = NULL,
    strip_col = beamaColours::get_blue(),
    is_brexit = FALSE,
    brexit_colour = beamaColours::get_pink(),
    point_colour = beamaColours::get_pink(),
    y_delta = c(0,0),
    db = tp_utils$new()$get_db(),
    show_growth_caps = F,
    caps_x = c(1, 1, -1, -1),
    caps_y = c(1,-1, -1,  1),
    caps_lbl = c('EXPANSION','RECOVERY','CONTRACTION','SLOWDOWN'),
    caps_col = gray.colors(10)[7],
    caps_size = 8


){
    my_title <- title
    if( is.null( title )){
        my_title <- code
    }
    my_plot <- tg$new( code, db_name = db )
    my_k <- k
    my_fx <- fx

    if(is.null( k )){
        my_k <- my_plot$data_freq
    }

    if( is.null( fx )){

      if(my_k == 12){
          my_fx <- 'mth'
      }else if(my_k == 4){
          my_fx <- 'qtr'
      }

    }

    my_plot$plot_spider(
        title= my_title, y1= y1, y2 = y2, k = my_k,
        fx = tolower( my_fx ),
        verbose = T, strip_col = strip_col,
        is_brexit = is_brexit,
        brexit_colour = brexit_colour,
        point_colour = point_colour,
        y_delta = y_delta,
        show_growth_caps = show_growth_caps,
        caps_x = caps_x,
        caps_y = caps_y,
        caps_lbl = caps_lbl,
        caps_size = caps_size,
        caps_col = caps_col

    )

}


tp.view_data <- function( code='CHAW', format = 'ts', y1 = NULL, dp = NULL ){

    x <- tg$new(x = code)$data_ts

    if(!( tolower(format) == 'ts') ){

       my_data <- tp_utils$new()$to_df( x )
       if(!is.null(dp)){ my_data$value <- round( my_data$value, digits = dp) }
       if( is.null( y1 )){

            return( my_data )

       }else{


           return(

               dplyr::filter(
                    my_data,
                    yr >= y1
                )
           )

       }

    }else{

        my_data <- x

        if(!is.null(dp)){ my_data <- round( my_data, digits = dp) }

        if( is.null( y1 )){

            return( my_data )

        }else{

            return(
                stats::window( my_data , start = c( y1, 1 ) )
            )
        }
    }
}

tp.view_ons_data <- function(code = 'CHAW', grp = NULL ){
  onsR2::download(code = code, grp = grp)
}

tp.get_value_raw <- function(code, yr, mth, value_only = T, dy = 1, use_dy = FALSE){

    #code = 'k646'; yr = 2016; mth= 6;fx='mm12'
    my_sql <- NULL

    if(use_dy){
        my_sql <- sprintf("select data_value as value from trends_data where data_code = '%s' and yr=%s and mth=%s and dy=%s", code, yr, mth, dy)
    }else{
        my_sql <- sprintf("select data_value as value from trends_data where data_code = '%s' and yr=%s and mth=%s", code, yr, mth)
    }

    my_data <- tp_utils$new()$run_sql( my_sql )

    if( nrow(my_data) > 0){
        if(value_only){
            return( my_data$value )
        }else{
            return( my_data)
        }
    }

    return( NULL)
}

tp.get_pc <- function(
     code,
     select_yr = c(2017,2017),
     select_mth = c(1, 1),
     fx ='MAT1,MAT12,MQT1,MAT4,MM1,MM3,MM12,QQ1,QQ4,YTD1,YTD12,YTD4,YY1,MAYTDM1',
     ops = 'avg',
     value_only = FALSE,
     db = NULL
){

    #code = 'k646'; yr = 2016; mth= 6;fx='mm12'
    y1 <- select_yr[1]
    y2 <- select_yr[ length(select_yr) ]

    m1 <- select_mth[1]
    m2 <- select_mth[ length(select_mth) ]

    my_fx <- toupper( fx )
    my_data <- tg.get_growth_data( code = code, select = fx, select_yr = c(y1,y2), ops = ops, db = db)

    my_data_n <- nrow( my_data )

    if(my_data_n > 0){



        if( value_only ){

            return(
                dplyr::filter( my_data,yr>=y1, yr<=y2, mth >= m1,   mth <= m2)$value
            )

        }else{

            return(
                dplyr::filter( my_data,yr>=y1, yr<=y2, mth >= m1,   mth <= m2)
            )
        }
    }
}#tp.get_pc('m_elec')

tp.get_value <- function(
    code,
    select_yr = c(2017,2017),
    select_mth = c(1, 1),
    fx ='MTH,QTR,MAT,YTD,MAYTD',
    ops = 'avg',
    value_only = FALSE,
    db = NULL
){

    #code = 'k646'; yr = 2016; mth= 6;fx='MAT'
    y1 <- select_yr[1]
    y2 <- select_yr[ length(select_yr) ]

    m1 <- select_mth[1]
    m2 <- select_mth[ length(select_mth) ]

    my_fx <- toupper( fx )
    my_data <- tg.get_agg_data( code = code, select = fx, select_yr = c(y1,y2), ops = ops, db = db)

    my_data_n <- nrow( my_data )

    if(my_data_n > 0){



        if( value_only ){

            return(
                dplyr::filter( my_data,yr>=y1, yr<=y2, mth >= m1,   mth <= m2)$value
            )

        }else{

            return(
                dplyr::filter( my_data,yr>=y1, yr<=y2, mth >= m1,   mth <= m2)
            )
        }
    }
}

tp.get_trend <- function(
    code,
    select_yr = c(2017,2017),
    select_mth = c(1, 1),
    fx ='MAT1,MAT12,MQT1,MAT4,MM1,MM3,MM12,QQ1,QQ4,YTD1,YTD12,YTD4,YY1',
    ops = 'avg',
    value_only = FALSE, is_plot = FALSE, plot_title = NULL, short_code = F, db = NULL
) {

    is_present <- function( x ){ length( grep(x, fx) ) > 0 }
    is_growth <- is_present('1') || is_present('2') || is_present('3') || is_present('4')
    my_fxn <- NULL

    if( is_growth ){

        my_fxn <- tp.get_pc

    }else{

        my_fxn <- tp.get_value
    }

    my_trend <- my_fxn( code = code, select_yr = select_yr, select_mth = select_mth, fx = fx, ops = ops, value_only = value_only, db = db )

    if( nrow(my_trend)>0 ){
        if(is_plot){

            my_x <- my_trend$name
            if(!short_code){ my_x <- my_trend$smart }

            df <- data.frame(
                x = factor( my_x),
                y = my_trend$value,
                code = my_trend$name,
                lbl_in = my_trend$value
            )

            my_title <- plot_title

            if(is.null( plot_title)){

                my_info <- tg.get_info( code, is_code = T, db = db )
                my_name <- code

                if(nrow(my_info) > 0){
                   my_name <- my_info$data_desc[ 1 ]
                }

                if(is_growth){
                    my_title <- sprintf("Growth - %s", my_name)
                }else{
                    my_title <- sprintf("Values - %s", my_name)
                }

            }

            tp.plot_bar_flip(df, title = my_title, subtitle = sprintf("%s %s", month.name[ select_mth[ 1 ]], select_yr[ 1 ]))

        }
    }
    return(
        my_trend
    )

}#tp.get_trend('l_elec')

tp.update_tdi <- function( data_code, data_icon = 'line-chart', y1 = NULL){
    #data_code <- 'MGSX'


    # if(!is.null( y1 )){
    #     my_data <- dplyr::filter(my_data, yr >= y1)
    # }

    my_y1 <- 2000
    tpu <- tp_utils$new()
    SQ <- storedQry::SQ$new(  tg.get_db( 'trends' ) )

    if(!is.null(y1)){

        my_y1 <- y1
    }
    data_fields <- 'a.yr, a.mth, a.dy, a.data_code, b.data_unit, b.data_frq, b.data_src, b.data_desc, a.data_value '
    data_from <- 'trends_data a, trends_meta b'
    data_sort <- 'data_days desc limit 1'

    my_sql <- sprintf(
        "select %s from %s where a.data_code='%s' and a.data_code = b.data_code order by %s",
        data_fields, data_from, data_code, data_sort
    )

    my_data <- tpu$run_sql( my_sql )


    my_data_max <- tpu$run_sql( sprintf("select max(data_value) as value from (select * from trends_data where lower(data_code) ='%s' and yr>= %s)", tolower(data_code), my_y1))
    my_data_min <- tpu$run_sql( sprintf("select min(data_value) as value from (select * from trends_data where lower(data_code) ='%s' and yr>= %s)", tolower(data_code), my_y1))

    code_df <- tpu$run_sql( sprintf( "select id from tdi where data_code='%s'", data_code ))
    code_exists <- ( nrow( code_df ) > 0)

    if( code_exists){

        SQ$set_name( "tdi_update_indicator" )$set_params(
            list(
                `@i_yr` = my_data$yr[ 1 ],
                `@i_mth` = my_data$mth[ 1 ],
                `@i_dy` = my_data$dy[ 1 ],
                `@s_data_code` = my_data$data_code[ 1 ],
                `@i_data_value` = my_data$data_value[ 1 ],
                `@i_data_lowest` = my_data_min$value[ 1 ],
                `@i_data_highest` = my_data_max$value[ 1 ]
            )
        )$qry_exec()

    }else{

        my_data_freq <- switch(
            as.character( my_data$data_frq),
            "12" = "Monthly",
            "4" = "Quarterly",
            "1" = "Yearly",
            "Daily"
        )

        SQ$set_name( "tdi_add_indicator" )$set_params(

            list(
                `@i_yr` = my_data$yr[ 1 ],
                `@i_mth` = my_data$mth[ 1 ],
                `@i_dy` = my_data$dy[1],
                `@s_data_code` = my_data$data_code[ 1 ],
                `@i_data_value` = my_data$data_value[ 1 ],
                `@s_data_unit` = my_data$data_unit[ 1 ],
                `@s_data_desc` = my_data$data_desc[ 1 ],
                `@s_data_src` = my_data$data_src[ 1 ],
                `@s_tdi_freq` = my_data_freq,
                `@s_display_icon`= data_icon,
                `@i_data_lowest` = my_data_min$value[ 1 ],
                `@i_data_highest` = my_data_max$value[ 1 ]
            )

        )$qry_exec()

    }#if

}

#tp.update_tdi('K646')
tp.tdi_get_growth_info <- function( data_code, pc='MM12', ops = 'avg'){


    my_data_raw <- beamaTrends::tg$new( data_code )
    my_data_ts <- my_data <- NULL

    if( toupper(pc[ 1 ]) %in% c('MTH','QTR','MAT','YTD') ){

        my_data <- my_data_raw$get_agg_df( ops='avg', select = toupper( pc ))

    }else{

        my_data <- my_data_raw$get_growth_df( ops = ops, select = toupper( pc ))

    }

    return( my_data )
}

tp.update_tdi_pc <- function( data_code, pc='MM12', data_icon = 'line-chart', y1 = NULL, data_unit = '%', ops = 'avg'){
    #data_code <- 'MGSX'

    # my_data_raw <- beamaTrends::tg$new( data_code )
    # my_data_ts <- my_data <- NULL
    #
    # if( toupper(pc) %in% c('MTH','QTR','MAT','YTD') ){
    #
    #     my_data <- my_data_raw$get_agg_df( ops='avg', select = toupper( pc ))
    #
    # }else{
    #
    #     my_data <- my_data_raw$get_growth_df( ops = ops, select = toupper( pc ))
    #
    # }

    my_data_ts <- NULL
    my_data <- tp.tdi_get_growth_info(data_code = data_code, pc = pc, ops = ops)

    my_unit <- data_unit

    if(!is.null( y1 )){
        my_data <- dplyr::filter(my_data, yr >= y1)
    }


    my_data_rows <- nrow( my_data )

    data_fields <- 'data_code, data_unit, data_frq, data_src, data_desc'
    data_from <- 'trends_meta'
    my_sql <- sprintf(  "select %s from %s where data_code='%s' ",  data_fields, data_from, data_code )

    tpu <- tp_utils$new()

    my_info <- tpu$run_sql( my_sql )

    my_data_max <- sqldf::sqldf( "select max(value) as value from my_data")
    my_data_min <- sqldf::sqldf( "select min(value) as value from my_data")

    my_data_code <- sprintf("%s_%s", toupper(data_code), toupper( pc ))

    code_df <- tpu$run_sql( sprintf( "select id from tdi where data_code='%s'", my_data_code ))
    code_exists <- ( nrow( code_df ) > 0)

    SQ <- storedQry::SQ$new(  tg.get_db( 'trends' ) )

    if( code_exists){

        SQ$set_name( "tdi_update_indicator" )$set_params(
            list(
                `@i_yr` = my_data$yr[ my_data_rows ],
                `@i_mth` = my_data$mth[ my_data_rows ],
                `@i_dy` = 1,
                `@s_data_code` = my_data_code,
                `@i_data_value` = my_data$value[ my_data_rows ],
                `@i_data_lowest` = my_data_min$value[ 1 ],
                `@i_data_highest` = my_data_max$value[ 1 ]
            )
        )$qry_exec()

    }else{

        my_data_freq <- switch(
            as.character( my_info$data_frq),
            "12" = "Monthly",
            "4" = "Quarterly",
            "1" = "Yearly",
            "Daily"
        )

        SQ$set_name( "tdi_add_indicator" )$set_params(

            list(
                `@i_yr` = my_data$yr[ my_data_rows ],
                `@i_mth` = my_data$mth[ my_data_rows ],
                `@i_dy` = 1,
                `@s_data_code` = my_data_code,
                `@i_data_value` = my_data$value[ my_data_rows ],
                `@s_data_unit` = '%',
                `@s_data_desc` = my_info$data_desc[ 1 ],
                `@s_data_src` = my_info$data_src[ 1 ],
                `@s_tdi_freq` = my_data_freq,
                `@s_display_icon`= data_icon,
                `@i_data_lowest` = my_data_min$value[ 1 ],
                `@i_data_highest` = my_data_max$value[ 1 ]
            )

        )$qry_exec()

    }#if

}
#tp.update_tdi_pc('K646')

tp.plot_regression <- function (x, y, xlab="", ylab="", ret=FALSE, is_themed = T, sig_fig = 4) {

    require(ggplot2)
    require(ggthemes)

    y_n <- length(y)
    x_n <- length(x)

    if( !(y_n == x_n) ){
        stop("Arrays are not of equal length")
    }

    mydf <- data.frame( y=y , x=x)



    linear_model  <- lm(y ~ x, data = mydf)

    linear_adj_r2 <- summary( linear_model )$adj.r.squared
    linear_intercept <- linear_model$coef[[1]]
    linear_slope <- linear_model$coef[[2]]
    linear_pvalue <- summary( linear_model )$coef[2,4]

    g <- ggplot( linear_model$model, aes_string(x = names(linear_model$model)[2], y = names(linear_model$model)[1]))
    g <- g +  geom_point( colour = beamaColours::get_corporate_blue())
    g <- g +  stat_smooth( method = "lm", col = beamaColours::get_pink())
    g <- g + labs(
        title = paste(
            "Adj R2 = ",     signif( linear_adj_r2,    sig_fig ),
            "; Intercept =", signif( linear_intercept, sig_fig ),
            "; Slope =",     signif( linear_slope,     sig_fig ),
            "; P-value =",   signif( linear_pvalue,    sig_fig )
        )
    )

    if(!(xlab=="")){g <- g+labs(x=xlab)}
    if(!(ylab=="")){g <- g+labs(y=ylab)}

    if( ( min(x) < 0 ) && (max(x) > 0) ){
        g <- g + geom_vline( xintercept = 0, colour = beamaColours::get_grayblue() )
    }

    if( (min(y) < 0 ) && (max(y) > 0) ){
        g <- g + geom_hline( yintercept = 0, colour = beamaColours::get_grayblue() )
    }

    if(is_themed){

        g <- g + theme_igray()
        g <- g + scale_color_manual(
            values = c(
                beamaColours::get_corporate_blue(),
                beamaColours::get_pink() )
        )#scale_colour_tableau("colorblind10")#

        g <- g + theme( text = element_text( size = 12) )
    }

    if(!ret){

        print(g)

    }else{

        return(g)
    }
}# tp.plot_regression(x=rnorm(100),y=runif(100))

tp.plot_regression_code <- function (x = 'D7BT', y='CHAW', fx='mth', k=c('0','1','3','4','12'), select_yr = c(2010,2020), xlab="", ylab="", is_themed = T, sig_fig = 4, db = tpu.get_db() ) {

    dx <- tg$new( x, db_name = db )
    dy <- tg$new( y, db_name = db )

    x_frq <- dx$data_freq
    y_frq <- dy$data_freq

    my_k <- as.numeric( match.arg( k ) )


    my_xlab <- ifelse( nchar(xlab) == 0, x, xlab)
    my_ylab <- ifelse( nchar(ylab) == 0, y, ylab)
    my_fx <- tolower( fx )

    if( my_fx == 'mth' ){

        if(!(x_frq == y_frq ) ){
            cat(sprintf("Mismatch frequencies, %s freq = %s and %s freq = %s ", x, x_frq, y, y_frq), "\n" )
            return( NULL)
        }

        if((x_frq == 12) && (y_frq == 12) ){
            my_start <- c( select_yr[ 1 ], 1  )
            my_end <-   c( select_yr[ 2 ], 12 )
            my_x <- my_y <- NULL

            if( my_k == 0){

                my_x <- c( stats::window(dx$data_ts, start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$data_ts, start = my_start, end = my_end ) )

            }else if(my_k == 1 ){

                my_x <- c( stats::window(dx$get_mm1(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_mm1(), start = my_start, end = my_end ) )

            }else if(my_k == 3){

                my_x <- c( stats::window(dx$get_mm3(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_mm3(), start = my_start, end = my_end ) )

            }else if(my_k == 12){

                my_x <- c( stats::window(dx$get_mm12(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_mm12(), start = my_start, end = my_end ) )

            }

            if(length( my_x) == length(my_y) ){

               tp.plot_regression( my_x, my_y, my_xlab, my_ylab, sig_fig = sig_fig )

            }

        }else{

            cat("Frequency of series are not monthly \n")

        }

    }else if( my_fx == 'qtr'){

        if( (x_frq %in% c(4,12) ) && (y_frq %in% c(4,12) ) ){

            my_start <- c( select_yr[ 1 ], 1  )
            my_end <-   c( select_yr[ 2 ], 4 )
            my_x <- my_y <- NULL

            if(my_k == 0){

                my_xts <- switch( as.character( x_frq) ,
                     '4'  = dx$data_ts,
                     '12' = dx$set_agg('qtr','avg')$get_agg()
                )

                my_x <- c( stats::window( my_xts , start = my_start, end = my_end  ) )


                my_yts <- switch( as.character( y_frq) ,
                     '4'  = dy$data_ts,
                     '12' = dy$set_agg('qtr','avg')$get_agg()
                )
                my_y <- c( stats::window( my_yts , start = my_start, end = my_end   ))

            }else if(my_k == 1){

                my_x <- c( stats::window(dx$get_qq1(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_qq1(), start = my_start, end = my_end ) )

            }else if(my_k == 4){

                my_x <- c( stats::window(dx$get_qq4(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_qq4(), start = my_start, end = my_end ) )

            }

            if(length( my_x) == length(my_y) ){

                tp.plot_regression( my_x, my_y, my_xlab, my_ylab, sig_fig = sig_fig )

            }else{

                cat( sprintf("Different lengths: x = %s and y=%s \n", length( my_x ), length( my_y )) )
            }

        }else{

            cat("Frequency of series are not monthly or quarterly \n")

        }

    }else if( fx %in% c( 'yr','mat','ytd','mqt' ) ){

        my_start <- c( select_yr[ 1 ], 1  )
        my_end <-   c( select_yr[ 2 ], 1 )
        my_x <- my_y <- NULL

        if(my_k == 0){

            my_x <- c(
                stats::window(
                    dx$set_agg( fx ,'avg')$get_agg(),
                    start = my_start,
                    end = my_end
                )
            )

            my_y <- c(
                stats::window(
                    dy$set_agg( fx ,'avg')$get_agg(),
                    start = my_start,
                    end = my_end
                )
            )

        }else if( my_k == 1 ){

            if(fx == 'yr'){
                my_x <- c( stats::window(dx$get_yy1(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_yy1(), start = my_start, end = my_end ) )

            }else if (fx %in% c('mat','mqt')){

                my_x <- c( stats::window(dx$get_mat1(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_mat1(), start = my_start, end = my_end ) )

            }else if (fx == 'mqt'){

                my_x <- c( stats::window(dx$get_mqt1(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_mqt1(), start = my_start, end = my_end ) )

            }else{

                cat("Operation not applicable\n")
            }

        }else if( my_k == 4 ){

         if (fx == 'mat'){

                my_x <- c( stats::window(dx$get_mat4(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_mat4(), start = my_start, end = my_end ) )

            }else if (fx == 'ytd'){

                my_x <- c( stats::window(dx$get_ytd4(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_ytd4(), start = my_start, end = my_end ) )

            }else{

                cat("Operation not applicable\n")
            }

        }else if( my_k == 12 ){

            if (fx == 'mat'){

                my_x <- c( stats::window(dx$get_mat12(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_mat12(), start = my_start, end = my_end ) )

            }else if (fx == 'ytd'){

                my_x <- c( stats::window(dx$get_ytd12(), start = my_start, end = my_end ) )
                my_y <- c( stats::window(dy$get_ytd12(), start = my_start, end = my_end ) )

            }else{

                cat("Operation not applicable\n")
            }
        }


        if(length( my_x) == length(my_y) ){
            tp.plot_regression( my_x, my_y, my_xlab, my_ylab, sig_fig = sig_fig )
        }


    }else{
       cat( sprintf('Unknown function :%s', fx ),'\n')
    }

}

tp.snip_tdi_gauge <- function(
    file_name ="sample",r = list(), is_big= FALSE, file_dir = "R:/shiny/beama/bmonitor/www",
    ryg = c("#b63e97","#d5c40a","#30a4dc")
){

    my_rect <- r
    r_empty <-  ( is.list(r) & length(r) == 0 )
    computer <- toupper(Sys.info()["nodename"])

    if( r_empty ){
        if(computer == "BEAMAPC") {
            cat("I am in BEAMAPC")
            my_rect <- list( left = 15, top = 110, width = 285, height = 280, diameter = 300) #x=left, y=top,

        }else if(computer == "BEAMAHS6M212"){

            cat("I am in BEAMAWORK")

            if(!is_big){
                my_rect <- list( left = 19, top = 86, width = 287, height = 287, diameter = 245)
            }else{
                my_rect <- list( left = 19, top = 86, width = 325, height = 325, diameter = 350)
            }

        }else if(computer == "WBSERVER"){

            cat("I am in WBSERVER")

            if(!is_big){
                my_rect <- list( left = 19, top = 80, width = 225, height = 225, diameter = 245)
            }else{
                my_rect <- list( left = 19, top = 80, width = 325, height = 325, diameter = 350)
            }

        }else {

            cat("I am in NO MANS LAND")
            my_rect <- list( left = 15, top = 110, width = 285, height = 280, diameter = 300)

        }
    }

    tp.snip_plot( file_name, r = my_rect, file_dir = file_dir)

}

tp.plot_tdi_gauge <- function(data_code = 'USD', delta = 25, label=NULL ,
                           rev=FALSE, bc = TRUE, dp =1, file_name ="sample",
                           is_live=FALSE, y1 = 2016, y2 = 2016, m1=1, m2=6, d1 = 1, d2=23,
                           r = list(), is_big= FALSE, file_dir = "R:/shiny/beama/bmonitor/www",
                           ryg = c("#b63e97","#d5c40a","#30a4dc"),
                           snap_delay = 2
){
    require(googleVis)

    my_data <- NULL
    g_data <- g_range <- g_lowest <- g_highest <- x1 <- x2 <-  NULL
    tpu <- tp_utils$new()

    my_rect <- r
    r_empty <-  ( is.list(r) & length(r) == 0 )
    computer <- toupper(Sys.info()["nodename"])

    if( r_empty ){
        if(computer == "BEAMAPC") {
            cat("I am in BEAMAPC")
            my_rect <- list( left = 15, top = 110, width = 285, height = 280, diameter = 300) #x=left, y=top,

        }else if(computer == "BEAMAHS6M212"){

            cat("I am in BEAMAWORK")

            if(!is_big){
                my_rect <- list( left = 19, top = 86, width = 287, height = 287, diameter = 245)
            }else{
                my_rect <- list( left = 19, top = 86, width = 325, height = 325, diameter = 350)
            }

        }else if(computer == "WBSERVER"){

            cat("I am in WBSERVER")

            if(!is_big){
                my_rect <- list( left = 19, top = 80, width = 225, height = 225, diameter = 245)
            }else{
                my_rect <- list( left = 19, top = 80, width = 325, height = 325, diameter = 350)
            }

        }else {

            cat("I am in NO MANS LAND")
            my_rect <- list( left = 15, top = 110, width = 285, height = 280, diameter = 300)

        }
    }


    if(!is_live){

        my_data <- tpu$run_sql( sprintf("select * from tdi where data_code = '%s'", data_code) )

        g_range <-  my_data$data_highest[ 1 ] - my_data$data_lowest[ 1 ]
        x1 <- round(my_data$data_lowest[ 1 ] + g_range*( 100 - delta )/200, dp)
        x2 <- round( x1 + delta * g_range / 100, dp)
        g_lowest <- round( my_data$data_lowest[ 1 ], dp)
        g_highest <- round( my_data$data_highest[ 1 ], dp)




    }else{

        dd1 <- 372*y1 + 31*m1 + d1
        dd2 <- 372*y2 + 31*m2 + d2


        my_data <- tpu$run_sql( sprintf("select data_desc, data_value from trends_data where data_code='%s' and data_days <= %s  order by data_days desc limit 1", data_code, dd2))


        my_fx_max <- tpu$run_sql( sprintf("select max(data_value) as value from trends_data where data_code='%s' and data_days between %s and %s", data_code, dd1, dd2))$value[1]
        my_fx_min <- tpu$run_sql( sprintf("select min(data_value) as value from trends_data where data_code='%s' and data_days between %s and %s", data_code, dd1, dd2))$value[1]

        g_range <-  my_fx_max$value[ 1 ] - my_fx_min$value[ 1 ]
        x1 <- round(my_fx_min$value[ 1 ] + g_range*( 100 - delta )/200, dp)
        x2 <- round( x1 + delta * g_range / 100, dp)
        g_lowest <- round( my_fx_min$value[ 1 ], dp)
        g_highest <- round( my_fx_max$value[ 1 ], dp)



    }


    g_data <- data.frame(
        label = ifelse( is.null(label), my_data$data_desc, label),
        value = round( my_data$data_value[ 1 ], dp)
    )


    g <- NULL

    if(rev){
        if(bc){#BEAMA_COLOURS
            g <- gvisGauge(g_data,
                           options=list(
                               min = g_lowest, max = g_highest,
                               redFrom= x2,  redTo = g_highest,
                               yellowFrom = x1, yellowTo = x2,
                               greenFrom = g_lowest, greenTo = x1,
                               width = my_rect$diameter, height=500,
                               redColor = ryg[1],
                               yellowColor = ryg[2],
                               greenColor = ryg[3]
                           )
            )
        }else{
            g <- gvisGauge(g_data,
                           options=list(
                               min = g_lowest, max = g_highest,
                               redFrom = x2,  redTo = g_highest,
                               yellowFrom = x1, yellowTo = x2,
                               greenFrom = g_lowest, greenTo = x1,
                               width = my_rect$diameter, height = 500
                           )
            )

        }

    }else{
        if(bc){
            g <- gvisGauge(g_data,
                           options=list(
                               min = g_lowest, max=g_highest,
                               greenFrom = x2,  greenTo = g_highest,
                               yellowFrom = x1, yellowTo = x2,
                               redFrom = g_lowest, redTo = x1,
                               width = my_rect$diameter, height = 500,
                               redColor = ryg[1],
                               yellowColor = ryg[2],
                               greenColor = ryg[3]
                           )
            )
        }else{
            g <- gvisGauge(g_data,
                           options=list(
                               min = g_lowest, max = g_highest,
                               greenFrom = x2,  greenTo = g_highest,
                               yellowFrom = x1, yellowTo = x2,
                               redFrom = g_lowest, redTo = x1,
                               width = my_rect$diameter, height = 500
                           )
            )
        }
    }

    g$html$footer <- NULL
    g$html$jsFooter <- NULL
    g$html$caption <- NULL



    plot(g)



    tp.snip_plot( file_name, r = my_rect, file_dir = file_dir, snap_delay = snap_delay)

}

tp.snip_plot <- function(
    file_name = "sample1",
    r = list( left = 15, top = 110, width = 285, height = 280),
    file_dir = "R:/shiny/beama/bmonitor/www",
    snap_delay = 2){


    cmd <- sprintf( "snip_gauge.exe %s %s %s %s %s/%s", r$left, r$top, r$width, r$height,  file_dir=file_dir, file_name = file_name)

    write(cmd,"snip.bat")
    Sys.sleep( snap_delay)
    system("snip.bat")

}

tp.plot_donut <- function(
    df, col_cat = 'category', col_value = 'value', col_label = 'category',
    skale = 1, skale_units = 'units', fill_cols = NULL,
    radius = 4, text_size = 6, width = 2, lbl_text_size = 6, lbl_dp = 1,
    lbl_top_bottom = c(NULL, NULL)
){

    require( ggplot2)

    my_df <- data.frame(cat = df[, col_cat ], val = df[, col_value ], lbl = df[ , col_label ] )
    total <- sum(my_df$val)/skale

    my_df$ymax <- cumsum(my_df$val / sum(my_df$val))
    my_df$ymin <-  c(0, head(my_df$ymax, n=-1) )
    my_df$pos <-   (my_df$ymin + my_df$ymax) / 2
    my_df$x <- 1:nrow(my_df)


    # Make the plot
    g <-  ggplot(my_df, aes(fill=cat, ymax=ymax, ymin=ymin, xmax= radius ,xmin =(radius - width) ))
    g <- g + geom_rect( colour = beamaColours::get_gray())
    g <- g + coord_polar(theta="y")
    g <- g +  xlim(c(0, radius))

    if( is.null( fill_cols ) ){

        g <- g + scale_color_brewer(type = 'div', palette = 'Spectral', direction = 1)

    }else{

        g <- g + scale_fill_manual(  values = fill_cols, name="")

    }

    g <- g + theme(
        #legend.text=element_text(size=10),
        legend.position="none",
        axis.ticks=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank(),
        panel.grid=element_blank(),
        panel.border=element_blank(),
        panel.background = element_blank()
    )

    g <- g + annotate(
        geom  = "text", x = 0, y = 0,
        label = sprintf(
            "%s %s",
            beamaUtils::set_decimal( total , lbl_dp),
            skale_units
        )
        , size = text_size
    )


    g <- g + annotate(
        geom  = "text",
        x = rep( (radius - width / 2) , nrow( my_df )),
        y = my_df$pos,
        label = my_df$lbl,
        size = lbl_text_size
    )

    if( !is.null( lbl_top_bottom[ 1 ])){

        g <- g + annotate(
            geom  = "text", x = c( radius/3), y = c(0,0),
            label = c( lbl_top_bottom[ 1 ]),
            size = 3
        )

    }


    if( !is.null( lbl_top_bottom[ 2 ])){

        g <- g + annotate(
            geom  = "text", x = c( radius/5), y = c(0,0),
            label = c( lbl_top_bottom[ 2 ]),
            size = 3
        )

    }

    g <- g + labs(title="")

    print( g )
    my_df

}

tp.run_sql <- function(qry){

    return(
        tp_utils$new()$run_sql( qry )
    )

}

#' Note that functinons tp.run_msql and tp.copy_ms_table will only run on BEAMA networks with
#' a computer connect to the BEAMALTD domain. Any attempt to run this outside the specified
#' domain will fail.
#'
#' tp.run_mssql connect to BEAMAstatistic DB on WBSERVER and perform run the supplied query
tp.run_msql <- function(qry){

    return(
        beamaDB::run_qry( qry )
    )

}

#' tp.copy_ms_table copy table from BEAMAstatics DB on WBSERVER to supplied sqlite db tbl_db
tp.copy_ms_table <- function(
     tbl_name = NULL,
     tbl_db = tp_utils$new()$get_db(),
     tbl_sql = sprintf("select * from %s", tbl_name),
     is_append = F,
     is_overwrite = T
){

    if( !( is.null( tbl_name) || is.null( tbl_db )) ){

        tbl_data <- tp.run_msql( tbl_sql )

        if( nrow( tbl_data) > 0){

            RSQLite::dbWriteTable(

                conn = RSQLite::dbConnect( RSQLite::SQLite(), dbname = tbl_db),
                name = tbl_name,
                value = tbl_data,
                append = is_append,
                overwrite = is_overwrite

            )

        }

    }

}

tp.append_meta <- function( code,  frq, dst_db = 'R:/packages/bistats/inst/extdata/bistats.sqlite' ){

    beamaTrends::tp.copy_ms_table(
        tbl_name = 'trends_meta',
        tbl_db = dst_db,
        tbl_sql = paste0("select distinct data_code, data_desc, data_src, data_unit, 2017 as last_pub_yr,", frq , " as data_frq from trends_data where data_code like '",  code , "%'" ),
        is_append = T,
        is_overwrite = F
    )

}

# tp.append_data('uhma')
# tp.append_data('tacma')
tp.append_data <- function( code, dst_db = 'R:/packages/bistats/inst/extdata/bistats.sqlite'){


    beamaTrends::tp.copy_ms_table(
        tbl_name = 'trends_data',
        tbl_db = dst_db,
        tbl_sql = paste0("select yr,mth,dy,data_unit,data_value,data_code,data_src, data_desc from trends_data where data_code like '",code,"_%' "),
        is_append = T,
        is_overwrite = F
    )

}

tp.add_tbl <- function(tbl, dst_db = 'R:/packages/bistats/inst/extdata/bistats.sqlite'){

    if( tolower(tbl) %in% c('trends_meta', 'trends_data') ){

        cat('What meanest thou, sleeper? I cannot proceed with this outrageous request.\n')
        cat("Are you trying to wipe out the universe or are you a taking a piss?\n")
        cat("You need to restart your life all over again.\n")
        cat("I am aborting now ....but have a good life\n")

    }else{

        beamaTrends::tp.copy_ms_table(tbl_name = tbl , tbl_db = dst_db)
    }

}

tp.update_point <- function(code, yr, mth, value, dy = 1, d360 = 31){
    #code='k646';yr=2018;mth=12;value=25;dy=1;d360=31
    sql <- sprintf("update trends_data set data_value=%s where lower(data_code) = '%s' and data_days=%s", value, tolower(code), beamaUtils::ddays(yr = yr,mth = mth, dy = dy , d360 = d360 ))
    abc <- beamaTrends::tp.run_sql( sql )
    return(sql)
}

tp.get_euro <- function(){

    c(sort(sample(1:50,5)),sort(sample(1:12,2)))
}

tp.get_group <- function(grp, code_only = F, raw = F){

    tp_data$new(grp)$get_group(code_only = code_only, raw = raw)

}

tp.view_groups <- function(){

    tp.run_sql("select distinct grp from trends_groups ")
}

tp.seasonal_factor <- function( code , m = 1, y1=2015, y2 = y1+100, is_ratio = F, is_filter = F, as_pc = F ){
    #code='topsi_turnover_25' ; m = c(8:12); y1=1998; y2 = y1+100; is_ratio = F; is_filter = F
    abc <- tp.view_data( code )

    if(length( abc ) > 0){

       bcd <- abc
       if(is_filter){

           frq <- frequency( abc )
           bcd <- window( abc, start = c(y1,1), end=c(y2,frq))

       }
       a <- subset( bcd,   cycle(bcd) %in% m)
       b <- bcd

       if(is_ratio){

         b <-  subset( bcd, !(cycle(bcd) %in% m) )

       }

       mean_a <- mean( a )
       mean_b <- mean( b )


       if( !(mean_b == 0) ){

          my_ratio <- mean_a / mean_b
          my_factor <- ifelse(as_pc, 100*(my_ratio - 1 ), my_ratio)
          return( my_factor )

       }
    }
   NULL
}

#'Expected colums in dataframe
#' x = factor,
#' y = numeric,
#' lbl_in = numeric,
#' lbl_out = numeric,
#' code = factor (may be same as x)
#' example columns:  x=LETTERS[1:10]; y=rnorm(10); code = x; lbl_in = y; lbl_out = runif(10)
#' example function: df = data.frame( x = x, y= y, code = code ); tp.plot_bar_flip(df)
#' example function: df = data.frame( x = x, y= y, code = code, lbl_in = lbl_in ); tp.plot_bar_flip(df)
#' example function: df = data.frame( x = x, y= y, code = code, lbl_in = lbl_in, lbl_out= round(lbl_out,2) ); tp.plot_bar_flip(df, out_gap = 8)
#'
tp.plot_bar_flip <- function(
    df, ytitle = '', title = 'Boring chart', dp = c(1,1), out_gap = 5, is_smart = T, smart = ESMART_LABELS,
    colours = c(beamaColours::get_blue(), beamaColours::get_pink()), in_out_units = c('%',''), subtitle = NULL
){

    require(ggplot2)
    require(ggthemes)

    my_data <- tibble::as.tibble(  df )

    my_data <- dplyr::arrange( my_data, y )
    my_data$smart <- as.character( my_data$x )

    my_max <- max( abs(my_data$y))
    my_sign <- (my_data$y > 0 )
    my_mult <- ifelse( my_sign, 1, -1)




    my_data$lbl_in_pos <- my_data$y / 2
    my_data$lbl_out_pos <- my_data$y + out_gap * my_max /100 * my_mult

    if(!is.null( my_data$lbl_in)){
            my_data$lbl_in <- paste0( beamaUtils::set_decimal(my_data$lbl_in, dp[ 1 ]), in_out_units[1] )
    }

    if(!is.null( my_data$lbl_out)){

            if(nchar(in_out_units[2]) > 0 ){
                my_data$lbl_out <- paste0( beamaUtils::set_decimal(my_data$lbl_out, dp[ 2 ]), in_out_units[2] )
            }
    }


    #return(my_data)

    if(is_smart){


        if( !is.null( smart ) ){

            names_smart <- names(smart)
            for( name in names_smart){

                my_data$smart[ which( as.character(my_data$code) == name) ] <- smart[[ name ]]

            }

        }
    }


    my_data$smart <- factor( my_data$smart, levels = my_data$smart)

    my_data$col <- ( my_data$y > 0 )

    fill_colour <- NULL
    if(sum(my_data$col) == 0 || (nrow( my_data) == sum(my_data$col) ) ){

        fill_colour <-  colours

    }else{

        fill_colour <- rev( colours )

    }

    my_title <- title
    my_ytitle <- ytitle

    g <- ggplot( my_data,  aes(x = factor(smart), y= y, fill = col) )
    g <- g + geom_bar(stat = "identity")
    g <- g + scale_fill_manual( values = fill_colour, guide = FALSE, name = "")
    g <- g +  coord_flip() + labs (x = '', y= my_ytitle )
    g <- g + ggtitle( my_title, subtitle = subtitle)

    if(!is.null( my_data$lbl_in )){
        g <- g + geom_text( aes(x = factor( smart ), y = lbl_in_pos, label = lbl_in) , colour= "gray85",  size = 4, position=position_dodge(width=1) )
    }

    if(!is.null( my_data$lbl_out) ){
        g <- g + geom_text( aes(x = factor( smart ), y = lbl_out_pos, label = lbl_out) , colour= "gray70",  size = 3, position=position_dodge(width=1) )
    }

    g <- g + theme_igray()
    g <- g + scale_colour_tableau("colorblind10")
    g <- g + theme(

        legend.position = "none",
        legend.title = element_blank(),
        text = element_text(family="Museo 300", face="plain", size = 14),
        plot.title = element_text(family = "Museo 500", face="plain", size= 14)

    )
    print(g)
}

# tp.plot_brexit_indicators()
tp.plot_brexit_indicators <- function(
    indicators = "USDM,EURM,K646,D7BT,IKBH,K222,BQKU,CT2AM-AW,BQKR,S2KU,JVZ7,K22A,KAB9,ABMI-UKEA,NPEL,J5EK,CHAW,BQKS,L87S,L87U,DYDC,JT27,CT2AM-ANW,CT2AM-ARM,OECD/MEI_CLI_LOLITOAA_GBR_M"
    ,out_gap = 15
    ,dt1 = c(2016,6)
    ,dt2 = NULL
    ,ytitle = paste0('Growth since ',month.name[ dt1[2]],' ' , dt1[ 1 ])
    ,title = 'UK Economic Indicators'
    ,is_brexit = T
    ,is_smart = T
    , smart_labels = ESMART_LABELS
    ,colours = c(beamaColours::get_blue(), beamaColours::get_pink() )

){

    df <- td.get_brexit_indicators( indicators =  indicators, dt1 = dt1, dt2 = dt2, is_brexit = is_brexit)
    tp.plot_bar_flip(
        df, ytitle = ytitle, title = title, out_gap = out_gap, smart = smart_labels, colours=colours
    )

}

# tp.plot_xy(yr=2015)
# tp.plot_xy(fx='QQ1,QQ4')
# tp.plot_xy(fx='MAT1,MAT12')
# tp.plot_xy(yr=2015, mth = 9,fx='MAT1,MAT12')
# tp.plot_xy( code = "m_elec,m_mech,m_fb,m_dt,m_lpt,m_iron,m_ind,m_bispa,m_composite,l_elec,l_mech")
# tp.plot_xy(yr=2017 ,mth=9, code = "m_elec,m_mech,m_fb,m_dt,m_lpt,m_iron,m_ind,m_bispa,m_composite,l_elec,l_mech")

# PG_SMART <- list(
#
#     `PG-NEWEY` =	'Newey',
#     `PG-WFSENATE` =	'WF Senate',
#     `PG-EDMUNDSON` =	'Edmundson',
#     `PG-FEGIME` =	'Fegime',
#     `PG-SCREWFIX` =	'ScrewFix',
#     `PG-DENMANS` =	'Denmans',
#     `PG-AWEBB` =	'Awebb',
#     `PG-ELECTRICCENTRE` =	'Electric Centre',
#     `PG-OTHERS` =	'Other Wholesalers',
#     `PG-ANEW` =	'Anew',
#     `PG-CEF` =	'CEF',
#     `PG-WILTS` =	'Wilts',
#     `PG-RETAIL` =	'Other Retailers',
#     `PG-YESSS` =	'Yesss',
#     `PG-TOTAL` =	'Plus Grouip Total',
#     `PG-NLB` =	'Plus Gorup - Others',
#     `PG-LIA` =	'Plus Group - LIA',
#     `PG-BEAMA` =	'Plus Group - BEAMA'
#
# )
#
# PG_CODES_PACKED <- 'PG-NEWEY,PG-WFSENATE,PG-EDMUNDSON,PG-FEGIME,PG-SCREWFIX,PG-DENMANS,PG-AWEBB,PG-ELECTRICCENTRE,PG-OTHERS,PG-ANEW,PG-CEF,PG-WILTS,PG-RETAIL,PG-YESSS,PG-TOTAL,PG-NLB,PG-LIA,PG-BEAMA'
# PG_DB <- "R:/packages/bistats/inst/extdata/bistats.sqlite"

tp.plot_xy <- function(code='USDM,EURM,K646,D7BT', yr = 2017, mth = 9, fx ='MM1,MM12', db = NULL,
                       show_title =T, is_shaded = T, shades_size = c(4,2.5),
                       shades = c(
                           beamaColours::get_pink(), beamaColours::get_green(),
                           beamaColours::get_darkyellow(), beamaColours::get_limegreen()
                       ),
                       is_smart = F,
                       smart_labels = NULL,
                       show_growth_caps = F,
                       caps_x = c(1, 1, -1, -1),
                       caps_y = c(1,-1, -1,  1),

                        caps_col = gray.colors(10)[7],
                        caps_size = 8,
                        caps_labels = c('EXPANSION','RECOVERY','CONTRACTION','SLOWDOWN')

){
# code = PG_CODES_PACKED;db=PG_DB; yr = 2017; mth = 9; fx ='MM1,MM12'; show_title =T; is_shaded = T; shades_size = c(4,2.5) ;
# shades = c(  beamaColours::get_pink(), beamaColours::get_green(),   beamaColours::get_darkyellow(), beamaColours::get_limegreen()    )
    require(ggplot2)
    require(ggthemes)

    codes <- strsplit(code,",")[[ 1 ]]
    df1 <-  beamaTrends::tp.get_trend(  code ,  yr , mth , fx, db = db )

    df <- df1[,c('data_desc','data_code','name','value')]
    dfs <- tidyr::spread( df, name, value )

    fxp <- codes <- strsplit(fx,",")[[ 1 ]]

    names(dfs)[ names( dfs ) == fxp[ 1 ] ] <- 'x'
    names(dfs)[ names( dfs ) == fxp[ 2 ] ] <- 'y'
    names(dfs)[ names( dfs ) == 'data_desc' ] <- 'xd'
    names(dfs)[ names( dfs ) == 'data_code' ] <- 'xc'


    dfs$col <- 'expansion'
    dfs[ which( dfs$x < 0 & dfs$y < 0) ,'col'] <- 'contraction'
    dfs[ which( dfs$x < 0 & dfs$y > 0) ,'col'] <- 'slowdown'
    dfs[ which( dfs$x > 0 & dfs$y < 0) ,'col'] <- 'recovery'

    n_contraction <- nrow( dplyr::filter( dfs,col == 'contraction'))
    n_slowdown <- nrow( dplyr::filter( dfs,col == 'slowdown'))
    n_recovery  <- nrow( dplyr::filter( dfs,col == 'recovery'))
    n_expansion  <- nrow( dplyr::filter( dfs,col == 'expansion'))

    my_shades <- NULL
    if( n_contraction > 0 ){  my_shades <- shades[ 1] }
    if( n_expansion > 0 ){  my_shades <- c(my_shades, shades[ 2 ]) }
    if( n_recovery > 0 ){  my_shades <- c(my_shades, shades[ 3 ]) }
    if( n_slowdown > 0 ){  my_shades <- c(my_shades, shades[ 4 ]) }

    xxt <- dplyr::filter( df1,name == fxp[1])$smart[ 1 ]
    yyt <- dplyr::filter( df1,name == fxp[2])$smart[ 1 ]


    # if(is_smart){
    #
    #
    #     if( !is.null( smart_labels ) ){
    #
    #         for( name in names(smart_labels)){
    #
    #             dfs$smart[ which( my_data$data_code == name )] <- smart_labels[[ name ]]
    #
    #         }
    #     }
    # }
    #
    g <- ggplot( dfs, aes(x = x, y=y ) )

    if( is_shaded ){

        g <- g + geom_point(colour = beamaColours::get_gray() , size = shades_size[ 1 ])
        g <- g + geom_point( aes(colour = factor(col) ), size = shades_size[ 2 ])
        g <- g + scale_color_manual( values = my_shades   )

    }else{

        g <- g + geom_point( size = shades_size[ 1 ], colour=beamaColours::get_corporate_blue())

    }

    if(show_growth_caps){

        g <- g + annotate(
            'text',
            x = caps_x,
            y = caps_y,
            label = caps_labels,
            size = caps_size,
            colour = caps_col
        )

    }

    g <- g + geom_hline(yintercept = 0) + geom_vline(xintercept = 0)
    g <- g + labs( x = xxt, y = yyt)

    if( show_title){

        g <- g + ggtitle( label = 'Growth Cycle', subtitle= sprintf("%s %s", month.name[ mth], yr))

    }
    g <- g + geom_text( data=dfs, aes( x = x, y = y, label = xd),size =3, vjust=-0.8,hjust=0.4)


    g <- g + theme_igray()
    #g <- g + scale_colour_tableau("colorblind10")
    g <- g + theme(

        legend.position = "none",
        legend.title = element_blank()

    )

    print(g)
    return( dfs )

}

# tp.get_productivity( 'prod_25')
# tp.get_productivity( 'prod_26' )
# tp.get_productivity( 'prod_27' )
# tp.get_productivity( 'prod_28' )
# tp.get_productivity( 'prod_29' )
# tp.get_productivity( 'prod_30' )
# tp.get_productivity( 'prod_33' )

tp.get_productivity <- function(
    code = 'prod_26', title = 'Productivity', fx='qtr', select_yr =c(2012,2020),
    is_plot = TRUE, plot_fx = 'QTR,YR,YTD,MAT'
){

    grp <- beamaTrends::tp_data$new( code )$get_group()
    tov_code <- dplyr::filter(grp, tolower(description) == 'turnover' )$code
    emp_code <- dplyr::filter(grp, tolower(description)  == 'employment' )$code

    tov_data <- tg$new( tov_code )$set_agg( fx )$get_agg()
    emp_data <- tg$new( emp_code )$set_agg( fx )$get_agg()

    tov_start <- stats::start( tov_data)
    tov_end <- stats::end( tov_data )

    emp_start <- stats::start( emp_data)
    emp_end <- stats::end( emp_data )

    tov_start_dd <- beamaUtils::ddays(tov_start[ 1 ], tov_start[ 2 ] )
    tov_end_dd <- beamaUtils::ddays(tov_end[ 1 ], tov_end[ 2 ] )

    emp_start_dd <- beamaUtils::ddays(emp_start[ 1 ], emp_start[ 2 ] )
    emp_end_dd <- beamaUtils::ddays(emp_end[ 1 ], emp_end[ 2 ] )

    pp_start <- tov_start
    if(tov_start_dd < emp_start_dd ){
        pp_start <- emp_start
    }

    pp_end <- tov_end
    if(tov_end_dd  > emp_end_dd){
        pp_end <- emp_end
    }

    pp_up <- stats::window(tov_data, pp_start, pp_end)*10e6
    pp_dow <- stats::window( emp_data, pp_start, pp_end)*10e3

    pp <- stats::ts(pp_up / pp_dow, start=pp_start, frequency = frequency( pp_up))

    if(is_plot){
        tg$new( pp )$plot(  is_themed = T, select_yr = select_yr, select = plot_fx )
    }
    return( pp )
}

tp.sync_productivity <- function(){

    my_codes <- c(
        'PROD-SIC-25', 'PROD-SIC-26', 'PROD-SIC-27', 'PROD-SIC-28',
        'PROD-SIC-29', 'PROD-SIC-30', 'PROD-SIC-33','PROD-SIC-C,', 'PROD-SIC-F'
    )
    prod_code <- function( x ){ sprintf("prod_%s", substr( x, 10,11) ) }

    for(i in 1: length( my_codes)){

        cur_code <- my_codes[ i ]
        my_ts <- tp.get_productivity( prod_code( cur_code ), fx = 'qtr', is_plot = F)
        tg_ons$new( cur_code )$set_delay_update( T )$add_trends_data( x= my_ts)
    }

    tp_data.update_periods()

    # cur_code <- my_codes[ 8]
    # my_ts <- tp.get_productivity( prod_code( cur_code ), fx = 'qtr', is_plot = F)
    # tg_ons$new( cur_code )$set_delay_update( F )$add_trends_data( x= my_ts)


}

#
#examples
# dst_db <- 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# tp.copy_ms_table(tbl_name = 'plus_group', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'plus_group_gauge', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'plus_group_indx', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'plus_grp_race', tbl_db = dst_db)

# tp.copy_ms_table(tbl_name = 'dwta', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'dwta_boilers', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'tmv', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'mve', tbl_db = dst_db)

# tp.copy_ms_table(
#     tbl_name = 'plus_group_indx',
#     tbl_db = dst_db,
#     tbl_sql = "select yr,mth,indx,wholesaler,qtr,hy,dt from plus_group_indx where yr > 2009",
#     is_append = T,
#     is_overwrite = F
# )

# tp.copy_ms_table(
#     tbl_name = 'plus_group',
#     tbl_db = dst_db,
#     tbl_sql = "select yr,mth,member_company,wholesaler,sale_value,member_association,qtr,hy,dt from plus_group where yr > 2009",
#     is_append = T,
#     is_overwrite = F
# )

##### COPY FOR THE FIRST TIME ########
# dst_db <- 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# tp.copy_ms_table(tbl_name = 'sppg', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'sppg_qtr', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'hmrc_countries', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'hmrc_uk_trade', tbl_db = dst_db, tbl_sql = "select * from hmrc_uk_trade where sitc = '77258'")
# tp.copy_ms_table(tbl_name = 'trends_data', tbl_db = dst_db, tbl_sql = "select * from trends_data where data_code in ('SPPG-DWD','SPPG-DCP') order by yr, mth")
# tp.copy_ms_table(tbl_name = 'tppg', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'espg', tbl_db = dst_db)
# tp.copy_ms_table(tbl_name = 'pes', tbl_db = dst_db
# beamaTrends::tp.copy_ms_table(tbl_name = 'lvsg', tbl_db = dst_db)
# beamaTrends::tp.copy_ms_table(tbl_name = 'powertrack', tbl_db = dst_db)
# beamaTrends::tp.copy_ms_table(tbl_name = 'mve', tbl_db = dst_db)
# beamaTrends::tp.copy_ms_table(
#      tbl_name = 'trends_meta',
#      tbl_db = dst_db,
#      tbl_sql = "select distinct data_code, data_desc, data_src, data_unit, 2017 as last_pub_yr, 12 as data_frq from trends_data where data_code like 'WMA%gbp'",
#      is_append = T,
#      is_overwrite = F
# )

# dst_db <- 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# beamaTrends::tp.copy_ms_table(
#     tbl_name = 'trends_data',
#     tbl_db = dst_db,
#     tbl_sql = "select yr,mth,dy,data_unit,data_value,data_code,data_src, data_desc from trends_data where data_code like 'wma_equip%' ",
#     is_append = T,
#     is_overwrite = F
# )

# beamaTrends::tp.copy_ms_table(
#      tbl_name = 'trends_meta',
#      tbl_db = 'R:/packages/bistats/inst/extdata/bistats.sqlite',
#      tbl_sql = "select distinct data_code, data_desc, data_src, data_unit, 2017 as last_pub_yr, 4 as data_frq from trends_data where data_code like 'uhma_%'",
#      is_append = T,
#      is_overwrite = F
# )

# dst_db <- 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# beamaTrends::tp.copy_ms_table(
#     tbl_name = 'trends_data',
#     tbl_db = dst_db,
#     tbl_sql = "select yr,mth,dy,data_unit,data_value,data_code,data_src, data_desc from trends_data where data_code like 'SPPG_QTR_%' ",
#     is_append = T,
#     is_overwrite = F
# )

# dst_db <- 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# beamaTrends::tp.copy_ms_table(
#      tbl_name = 'trends_meta',
#      tbl_db = dst_db,
#      tbl_sql = "select distinct data_code, data_desc, data_src, data_unit, 2017 as last_pub_yr, 12 as data_frq from trends_data where data_code like 'tacma%'",
#      is_append = T,
#      is_overwrite = F
# )

# dst_db <- 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# beamaTrends::tp.copy_ms_table(
#      tbl_name = 'trends_data',
#      tbl_db = dst_db,
#      tbl_sql = "select distinct data_code, data_desc, data_src, data_unit, 2017 as last_pub_yr, 12 as data_frq from trends_data where data_code like 'tacma%'",
#      is_append = T,
#      is_overwrite = F
# )

##### COPY TRADE TABLE - OVERWRITE ########
# dst_db <- 'R:/data/lite/uktrade.sqlite'
# tp.copy_ms_table(tbl_name = 'hmrc_uk_trade', tbl_db = dst_db)

# dst_db <- 'R:/data/lite/uktrade.sqlite'
# beamaTrends::tp.copy_ms_table(
#      tbl_name = 'hmrc_uk_trade',
#      tbl_db = dst_db,
#      tbl_sql = "select * from hmrc_uk_trade where yr = 2017 and mth =8",
#      is_append = T,
#      is_overwrite = F
# )

