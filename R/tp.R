
#' R6 class for Trends plot
#' td = Trends Plot
#'
tp <- R6::R6Class(
  'tp',
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
    title ='',
    ylab = ' % Change ',
    colour = beamaColours::get_stats()[1],
    dt_breaks="3 months",
    dt_breaks_format="%b %Y",
    is_smooth=FALSE,
    dt_desc = '',
    freq = NULL,
    freq_default = 0,
    delta_x = 0,
    skale =1,
    caption_size = 4,
    point_size = c(2,4),
    line_size = 1.3,
    stripe_text_size = 12,
    brexit_mode = FALSE,
    dp = 0, #decimals
    #PAIRED = c("#377EB8","#E41A1C"),


    y_lim = numeric(2),

    initialize = function(code){

      self$set_codes(code)

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

    ,set_caption_size = function(value){

      if(!missing(value) && !is.null(value)){
        self$caption_size <- value
      }
      invisible(self)

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
      if(!missing(value) && is.null(value) && is.logical(value)){
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
        self$dtd1 <- 372 * self$y1 + 31 * self$m1 + self$d1
      }else{
        self$dtd2 <- 372 * self$y2 + 31 * self$m2 + self$d2
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

    ,set_pc = function(value){
      if(!missing(value) && !is.null(value)){
        self$pc <- value
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

          my_sql <- "select yr,qtr,data_code,data_desc,avg(data_value)  as value from trends_data "
          my_group <- " group by yr,qtr,data_code"
          my_order <- " order by yr,qtr,data_code,data_desc "

        }else{

          my_sql <- "select yr,qtr,data_code,data_desc,sum(data_value)  as value from trends_data "
          my_group <- " group by yr,qtr,data_code "
          my_order <- " order by yr,qtr,data_code,data_desc "

        }

      }else if(my_fx=='m'){

        if(my_avg){

          my_sql <- "select yr,mth,data_code,data_desc,avg(data_value)  as value from trends_data "
          my_group <- "group by yr,mth,data_code"
          my_order <- " order by yr,mth,data_code,data_desc "

        }else{

          my_sql <- "select yr,mth,data_code,data_desc,sum(data_value)  as value from trends_data "
          my_group <- " group by yr,mth,data_code"
          my_order <- " order by yr,mth,data_code,data_desc "

        }

      }else if(my_fx=='y'){

        if(my_avg){

          my_sql <- "select yr,data_code,data_desc,avg(data_value)  as value from trends_data "
          my_group <- " group by yr,data_code "
          my_order <- " order by yr,data_code,data_desc "

        }else{

          my_sql <- "select yr,data_code,data_desc,sum(data_value)  as value from trends_data"
          my_group <- " group by yr,data_code "
          my_order <- " order by yr,data_code,data_desc "

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

        my_sql <- "select yr,mth,dy,data_code,data_desc,data_value  as value from trends_data "
        my_order <- " order by yr,mth,dy,data_code,data_desc "

      }else if (my_fx=='ms'){

        my_sql <- paste0("SELECT yr, mth,substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
        my_group <- " group by yr,mth,wrap "

      }else if (my_fx=='qs'){

        my_sql <- paste0("SELECT yr, qtr,substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
        my_group <- " group by yr,qtr,wrap "

      }else if (my_fx=='ys'){

        my_sql <- paste0("SELECT yr, substr(data_code, 6, length(data_code) - ",fx_level+6,") AS wrap, sum(data_value) as value  FROM trends_data ")
        my_group <- " group by yr,wrap "

      }else if (my_fx=='mc'){

        my_sql <- paste0("SELECT yr, mth,(substr(data_code,instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data")
        my_group <- " group by yr,mth,wrap "

      }else if (my_fx=='qc'){

        my_sql <- paste0("SELECT yr, qtr,(substr(data_code, instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data ")
        my_group <- " group by yr,qtr,wrap "

      }else if (my_fx=='yc'){

        my_sql <- paste0("SELECT yr, ( substr(data_code, instr(data_code,'EXP')+instr(data_code,'IMP'), length(data_code))) AS wrap, sum(data_value) as value  FROM trends_data")
        my_group <- " group by yr,wrap "

      }


      q_code <- paste0(" data_code in ", self$code )
      q_yr <- paste0(" and (yr between ", self$y1, " and ", self$y2, ")")
      q_mth <- paste0(" and (mth between ", self$m1 ," and ", self$m2, ")")
      q_where <- paste0(" where ",q_code,q_yr,q_mth)

      my_sql <- paste0(my_sql,q_where,my_group,my_order)
      return(my_sql)
    }

    ,get_data = function( encode = T){

      my_data <- self$run_sql( self$build_sql() )
      my_fx <- self$fx
      my_pc <- as.numeric(self$pc)

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

                  my_k <- self$freq_default

                  if(!is.null(self$freq)){ my_k <- self$freq}

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
      if(encode) {

        return( private$en_code(my_data))

      }else{

        return( my_data )

      }
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


    ,set_breaks = function(value){
      if(!missing(value) && !is.null(value)){
        self$dt_breaks <- value
      }
      invisible(self)
    }

    ,set_breaks_fmt = function(value){
      if(!missing(value) && !is.null(value)){
        self$dt_breaks_fmt <- value
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
      cat("Freq=",my_frq,"\n")

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


    ,plot_pc = function(brewer_set = "Set1", ytitle=NULL, dazzle=FALSE, encode = T){
      require(ggplot2)
      my_data<- self$get_data( encode = encode)



      my_pc <- as.numeric(self$pc)
      my_frq <- self$freq
      my_ylab <- self$get_pc_ylab()

      if( !is.null( ytitle) ) {
        my_ylab <- ytitle
      }


      if( (trimws(my_data$data_desc[1])=='dummy-desc') && (nchar(self$dt_desc)>0)){
        my_data$data_desc <- self$dt_desc
      }

      my_data <- dplyr::filter( my_data, !is.na(pc) )
      my_data$date <- as.Date( paste( my_data$yr, my_data$mth, my_data$dy, sep="-"))
      my_data$pc <- my_data$pc/self$skale

      mytext <- dplyr::filter( my_data, yr==self$y2, mth==self$m2)

      gmin <- NULL
      gmax <- NULL
      g <- NULL

      if(!self$is_group){

        my_data$data_days <- with(my_data,yr*372+mth*31+31)

        gmin <-  dplyr::filter(
          dplyr::group_by(my_data ,data_code) ,
          data_days==min(data_days)
        )
        gmax <-  dplyr::filter(
          dplyr::group_by(my_data,data_code),
          data_days==max(data_days)
        )

        gtxt <- rbind(gmin,gmax)

        #return(gtxt)

        if(!dazzle){
          g <- ggplot(my_data,aes(x=date,y=pc))
          g <- g + geom_line(size=self$line_size,colour = self$colour)
        }else{
          g <- ggplot(my_data,aes(x=date,y=pc,colour=factor(data_desc)))

          g <- g + geom_line(size = self$line_size, aes(colour=data_desc))
          g <- g + scale_color_manual(values= self$get_line_colours())
        }

        g <- g + facet_wrap( ~ data_desc)
        g <- g + guides(colour=FALSE)
        # my_intercepts <- c(min(my_data$pc,na.rm=TRUE),max(my_data$pc,na.rm=TRUE))
        #

        g <- g+ geom_point( data=gtxt, aes( x = date, y = pc ) , size = self$point_size[2], colour = beamaColours::get_line_colour())
        g <- g+ geom_point( data=gtxt, aes( x = date, y = pc ) , size = self$point_size[1], colour = beamaColours::get_pink())
        g <- g+ geom_text(  data=gtxt, aes( x = date, y = pc, label = private$set_decimal( pc, self$dp) ),vjust=-0.8,hjust=0.4,size= self$caption_size,colour = beamaColours::get_smooth_colour())
        g <- g+ theme(legend.position="none", strip.text = element_text(size = self$stripe_text_size ) )

        #g <- g + geom_hline(yintercept=my_intercepts,colour=colour_set,linetype='dashed')
      }else{

        g <- ggplot(my_data, aes(x=date,y=pc,colour=data_code))
        g <- g + geom_line( aes(group=data_code),size = self$line_size)
        g <- g+ theme(
          legend.position = c(self$legend_x, self$legend_y),
          legend.background = element_rect(fill = NA, colour = NA),#lgpos$fill
          legend.title=element_blank(),
          text = element_text(12)
        )
        g <- g+ scale_colour_brewer( palette = brewer_set )

      }

      g <- g + labs(title= self$title,x="",y = my_ylab)
      g <- g + geom_hline(aes(yintercept=0))



      if( !(self$y_lim[1] == 0) ){
        g <- g + ylim( self$y_lim )
      }

      if( !(self$delta_x == 0) ){

        min_date <- as.Date( paste( gmin$yr[1], gmin$mth[1], 28,sep='-'))
        max_date <- as.Date( paste( gmax$yr[1], gmax$mth[1], 28,sep='-'))

        #return(list(min_date,max_date))

        lubridate::month(max_date) <- lubridate::month(max_date) + self$delta_x
        lubridate::month(min_date) <- lubridate::month(min_date) - self$delta_x

        ##test
        #return(list(min_date,max_date))
        ###

        g <- g + theme(strip.text = element_text( size= self$stripe_text_size ))

        g <- g + xlim( min_date,max_date )
      }

      if(self$brexit_mode){

        g <- g + geom_vline( aes(xintercept = as.numeric(as.Date( BREXIT_POINT )) ), colour = beamaColours::get_pink(), linetype = 'dashed', size= 1  )

      }

      print(g)
      return(my_data)
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

        mydata$data_days <- with(mydata,yr*372+mth*32+dy)
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
      #g <- g + geom_hline(aes(yintercept=0))
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
tp.view_ons_code <- function( code='ABMI', is_growth = F, select_yr=NULL, ops = 'avg', is_themed = T,
                              select = 'MAT,MTH,QTR,YR,YTD,MAT1,MQT1,MAT12,MAT4,MM1,MM12,MM3,QQ1,QQ4,YTD1,YTD12,YTD4,YY1'){
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

  bt$plot( is_growth = is_growth, title = paste0( trimws( my_data$title ),' (',code,')' ), select = select , select_yr = select_yr, ops = ops, is_themed = is_themed)

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
tp.view_code <- function( code='ABMI', is_growth = F, select=NULL, select_yr=NULL, is_themed = T){
    #source('global.R')

  bt <- tg$new(x=code)

  bt$plot( is_growth = is_growth, title = code, select = select , select_yr = select_yr, is_themed = is_themed)

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
}

tp.view_ons_spider<- function(
    code='ABMI',
    y1 = lubridate::year(Sys.Date())-2,
    y2=lubridate::year(Sys.Date())
){
    my_data <- onsR2::download( code = code)
    my_ts <- my_data$m_data
    if( is.null( my_ts)){ my_ts <- my_data$q_data}
    my_plot <- tg$new(my_ts)$plot_spider( title=my_data$title, y1= y1, y2 = y2)
}

tp.view_spider<- function(
    code='ABMI',
    y1 = lubridate::year(Sys.Date())-2,
    y2=lubridate::year(Sys.Date())
){

    my_plot <- tg$new( code )$plot_spider( title= code, y1= y1, y2 = y2)

}
