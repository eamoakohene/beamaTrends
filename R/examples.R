# abc <- onsR2::download('CHAW')$m_data
# abc <- 'USDM'
# cde <- beamaTrends::tg$new( onsR2::download('CHAW')$m_data )
# cde$plot()
# cde$plot(T)
# cde$get_data_freq()
# cde$get_data_start()
# cde$get_data_format()
# cde$get_data_ts()
# cde$set_agg('qtr')$get_agg()
# cde$set_agg('qtr','avg')$get_agg()
# cde$set_agg('yr')$get_agg()
# cde$set_agg('yr','avg')$get_agg()
# cde$set_agg('ytd')$get_agg()
# cde$set_agg('ytd','avg')$get_agg()

# cde$set_agg('mat')$get_agg()
# cde$set_agg('mat','avg')$get_agg()

# cde$set_agg('mqt')$data_agg
# cde$set_agg_default('qtr')$get_mat4()

# cde <- td$new('CHAW')
# cde <- beamaTrends::td$new('chaw')
# cde <- beamaTrends::tg$new('ABMI')
# cde <- beamaTrends::tg$new('K646',db_limit = list(yr=2016,mth=5))
# cde <- beamaTrends::td$new('K646',db_limit = list(yr=2016,mth=5))

# beamaTrends::tg$new('sitc-MENA-EXPORT-77')$plot(ops ='sum',select = 'MAT,YR,YTD', skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - Middle East & Africa')
# beamaTrends::tg$new('sitc-MENA-EXPORT-77')$plot(ops ='sum',select = 'MTH,QTR', skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - Middle East & Africa')
# beamaTrends::tg$new('sitc-EUEU-EXPORT-77')$plot(ops ='sum',select = 'YR', select_yr=c(2000,2015), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Electrical Machinery Exports - EU28')
# beamaTrends::tg$new('sitc-EUEU-EXPORT-77')$plot(T,select = 'MM12,YTD12,MAT12', select_yr=c(2000,2016), title = 'Electrical Machinery Exports - EU28')
# beamaTrends::tg$new('sitc-NANA-EXPORT-77')$plot(T, select_yr=c(2000,2016), title = 'Electrical Machinery Exports - North America')
# beamaTrends::tg$new('sitc-FR-EXPORT-77')$plot(T, select_yr=c(2000,2016), title = 'Electrical Machinery Exports - France')
# cde$plot( )
# cde$plot( T )
# cde$plot( T ,select = 'MAT12,MM12,YTD12')
# cde$plot( T ,select = 'MM1,MAT1,QQ1')
# cde$plot( T ,select = 'MM12,qq4')
# ts.plot(beamaTrends::tg$new( onsR2::download('CHAW')$m_data )$set_agg('ytd', 'avg')$get_agg(), ylab='Index(2010=100)')
# ts.plot(beamaTrends::tg$new( onsR2::download('ABMI')$q_data )$set_agg('ytd', 'avg')$get_agg(), ylab='GBP millions')
# ts.plot(beamaTrends::tg$new( onsR2::download('ABMI')$q_data )$get_growth_data('ytd', 'avg'), ylab='ytd % growth')

# ts.plot(beamaTrends::tg$new('ABMI')$get_growth_data('qq4'),ylab='Growth %')
# ts.plot(beamaTrends::tg$new('ABMI')$get_growth_data('mat4'),ylab='Growth %')

# cde$get_mm1()
# cde$get_mm3()
# cde$get_mm12()
# cde$get_qq1()
# cde$get_qq4()
# cde$get_yy1()
# cde$get_yy1(ops = 'avg')
# cde$get_mat1()
# cde$get_mat12()
# cde$get_mat4()
# cde$get_ytd4()
# cde$get_ytd12()

# cde <- tg$new(rnorm(100),c(2010,1),12)
# cde$get_data_freq()
# cde$get_data_start()
# cde$get_data_format()
# cde$get_data_ts()
# cde$get_data_df()
# cde$to_df(cde$get_ytd12(),'ytd4', T, T )

#cde <- beamaTrends::tg$new(data.frame( yr=rep(2010:2015,each=12), mth=rep(1:12,each=6),value=rnorm(72,mean=10,sd=1)) )
#cde$get_mat1()

#cde <- beamaTrends::tg$new(data.frame( yr=rep(2010:2015,each=12), mth=rep(1:12,each=6),value=rnorm(72,mean=10,sd=1)) )
#cde$get_mat1()

#cde <- beamaTrends::tg$new(data.frame( yr=rep(2010:2015,each=4), mth=rep(1:4,each=6),value=rnorm(24,mean=10,sd=1)) )
#cde$get_mat1()



# cde$get_data_freq()
# cde$get_data_start()
# cde$get_data_format()
# cde$get_data_ts()
#
# cde <- beamaTrends::tg$new(
#     data.frame(
#         yr=2000:2015,
#         value=rnorm(16)
#     )
# )
#
# cde$get_data_freq()
# cde$get_data_start()
# cde$get_data_format()
# cde$get_data_ts()
#
# aaa<- cde$set_growth_fx(1)
# aaa$get_growth_data()

# abc <- onsR2::download('CHAW')$m_data
# cde <- beamaTrends::td$new(onsR2::download('CHAW')$m_data)
# cde$data_period
# cde$data_end
# cde$data_end_date
# cde$data_end_date_prv

# ccc <- function(){
#     cde <- td$new('CHAW')
#     cde$plot(
#         is_theme = T
#         )
# }
#
# cct <- function(){
#     cde <- td$new('CHAW')
#     cde$plot(
#         is_growth = T,
#         is_theme = T,
#         select =c("MAT12,MM12,YTD12"),
#         select_yr =c(2010,2017)
#     )
# }


  # abc <- onsR2::download('K646')$m_data
  # cde <- beamaTrends::td$new(abc,x_name='input cost')

  # cde$get_successive()
  # cde$get_successive( is_yoy = FALSE)
  # cde$get_mtm()$desc
  # cde$get_yoy()$desc
  # beamaTrends::td$new('K646',x_name='input cost')$get_growth_desc()
  # beamaTrends::td$new('ABMI',x_name='gdp')$get_growth_desc()
  # beamaTrends::td$new('PLUSGROUP',x_name='Plusgroup')$get_growth_desc()
  # beamaTrends::td$new('CHAW',x_name='RPI')$get_growth_desc()
  # beamaTrends::td$new()
  # aa <- beamaTrends::td$new('PLUSGROUP',x_name='Plusgroup', x_start=c(2013,11), x_frq=12)$get_growth_desc()
  # bt <- beamaTrends::td$new('ABMI',x_name='gdp')
  # aa$get_mtm()
  # aa$get_yoy()

#smart_lbl <- list(mm1="1-month growth", mm3="3-month growth", mm12="12-month growth")
#"mm1" %in% names(smart_lbl)

#aaa <- tg.get_growth_data('m_')
#tg.get_unit('CHAW')
#tg.plot_trends('CHAW,D7BT', select = "QQ1,QQ4",  x_delta=c(0,0), y_delta=c(0,1), is_themed = T )

#tg.plot_trends('CHAW,D7BT', select = "MM12,MAT12", x_delta=c(0,0), y_delta=c(0,1), is_themed = T )

#tg.plot_trends('CHAW,D7BT',  x_delta=c(0,0), y_delta=c(0,5), is_themed = T )
#tg.plot_trends('CHAW,D7BT', fx='q', x_delta=c(0,0), y_delta=c(0,5), is_themed = T )
#tg.plot_trends('m_elec,m_mech',  x_delta=c(0,3), y_delta=c(0,0), is_themed = T )

# tg.plot_trends('m_elec,k646',  x_delta=c(0,3), y_delta=c(0,0), is_themed = T )
# aaa<- tg.plot_trends('k646', select ='YY1', y1=1990,  x_delta=c(0,3), y_delta=c(0,2), is_themed = T )
# tp.view_code()
# tp.view_code(is_growth = T)

#tg.plot_trends('k646', select ='MM12,MAT12,YTD12', y1=2010,  x_delta=c(0,3), y_delta=c(0,2), is_themed = T )

#tp.view_code('m_elec')
#tp.view_data('m_elec', dp=1)


#beamaTrends::tg$new('sitc-MENA-EXPORT-77')$plot(ops ='sum',select = 'MTH,QTR,MAT',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - Middle East & Africa (SITC 77)', is_themed = T)
#beamaTrends::tg$new('sitc-EUEU-EXPORT-77')$plot(ops ='sum',select = 'MTH,QTR,MAT',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - EU28 (SITC 77)', is_themed = T)
#tg$new('sitc-NANA-EXPORT-77')$plot(ops ='sum',select = 'MTH,QTR,MAT',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - North America (SITC 77)', is_themed = T, x_delta=c(3,4),y_delta=c(0,100), dp = 0 )
#tg$new('sitc-NANA-EXPORT-77')$plot(is_growth = T,ops ='sum',select = 'MM12,YTD12,MAT12',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (GBP million)',title = 'Exports - North America (SITC 77)', is_themed = T, x_delta=c(3,3),y_delta=c(0,2), dp = 1 )

#abc <- tg$new('USDM')$plot(is_growth = T,ops ='sum',select = 'MM12,YTD12,MAT12',select_yr = c(2014,2020), skale = 1e6, skale_title = 'Value (1 GBP)',title = 'US Dollar - Sterling Exchange Rate', is_themed = T, x_delta=c(3,3),y_delta=c(0,0.5), dp = 1, return_plot = T )
#tg$new('USDM')$plot(ops ='avg',select = 'MTH',select_yr = c(2015,2020), skale = 1, skale_title = 'Exchange Rate (per 1 GBP)',title = 'US Dollar - Sterling Exchange Rate', is_themed = T, x_delta=c(3,5),y_delta=c(0,0), dp = 4, brexit_mode=T )
#tg$new('EURM')$plot(ops ='avg',select = 'MTH,QTR,YR',select_yr = c(2015,2020), skale = 1, skale_title = 'Exchange Rate (per 1 GBP)',title = 'EURO - Sterling Exchange Rate', is_themed = T, x_delta=c(3,5),y_delta=c(0,0), dp = 4, brexit_mode=T )

# steel_codes_fred <- c(
#   "WPU1012","WPU1017","PCU331210331210","PCU331222331222P",
#   "PCU3312223312225","WPU101704","PCU3315123315120", "PCU3311103311107",
#   "PCU3312213312211","IR141","WPU101706","WPU107601","PCU33211133211112",
#   "PCU331222331222","WPU101211","PCU331110331110","PCU331221331221","WPU10121193",
#   "COOTHERZ3311","PCU331210331210P","PCU42993042993011","PCU429930429930113","PCU3312213312213",
#   "PCU3311103311103", "PCU3315133315131","PCU3315133315133", "PCU3321113321111","PCU3311103311105",
#
#)

###### orgalime vienna
#GRAPHIC_PATH <- 'M:/2017-03-09-vienna-orgalime/graphics/'

#1. TURNOVER

#1.1 Turnover - GBP-yr
# tp$new(
#    tp_data$new('topsi_orgalime')$get_group(T)
# )$set_y1( 2014 )$set_y2( 2016 )$set_avg(F)$set_fx('m')$set_scale(1000)$set_delta_x(4)$set_pc('0')$set_dp(1)$plot_pc(ytitle="Annual Turnover (GBP billion)")
# beamaTrends::tp_utils$new()$plot_save('turnover_gbp_yr',path = GRAPHIC_PATH)
#
# #1.2 Turnover - PC-yr
# beamaTrends::tp$new(
#     beamaTrends::tp_data$new('topsi_orgalime')$get_group(T)
# )$set_y1( 2011 )$set_y2( 2016 )$set_avg(F)$set_fx('y')$set_scale(1)$set_delta_x(4)$set_delta_y(0,3)$set_pc('1')$set_dp(1)$plot_pc(ytitle="Annual Growth (%)")
# beamaTrends::tp_utils$new()$plot_save('turnover_pc_yr',path = GRAPHIC_PATH)

# #1.3 Turnover - GBP-mth
# beamaTrends::tp$new(
#     beamaTrends::tp_data$new('topsi_orgalime')$get_group(T)
# )$set_y1( 2014 )$set_y2( 2016 )$set_avg(F)$set_fx('m')$set_scale(1000)$set_delta_x(4)$set_delta_y(0,0)$set_pc('0')$set_dp(1)$plot_pc(ytitle="Monthly Turnover (GBP billion)")
# beamaTrends::tp_utils$new()$plot_save('turnover_gbp_mth',path = GRAPHIC_PATH)

#1.4 Turnover - GBP-mth
# abc <- tp$new(
#     beamaTrends::tp_data$new('topsi_orgalime')$get_group(T)
# )$set_y1( 2014 )$set_y2( 2016 )$set_breaks( "1 year"
# )$set_breaks_fmt("%Y")$set_avg(F)$set_fx('m')$set_scale( 1
# )$set_delta_x(2)$set_delta_y(0,0)$set_pc('12')$set_dp( 1
# )$set_brexit_label(T)$set_brexit_mode(T)$set_brexit_label(T
# )$set_brexit_text(y=-20,x="2016-04-15")$plot_pc(ytitle="Monthly Growth(%)")

# beamaTrends::tp_utils$new()$plot_save('turnover_pc_mth',path = GRAPHIC_PATH)

# smart_labels =  list(
#
#     USDM="Exchange - US Dollar",
#     `ABMI-PN2`="Gross Domestic Product",
#     K646="PPI - Input Prices",
#     D7BT = "Consumer Price Index",
#     IKBH = "Exports - Value",
#     BQKU = 'Exports - Volume Index',
#     BQKR = 'Exports - Price Index',
#     K222 = 'Index of Production',
#     NPEL = 'Business Investment',
#     `CT2AM-AW` = 'Construction Output',
#     S2KU = 'Index of Services',
#     JVZ7 = 'PPI - Output Prices',
#     K22A = 'Index of Manufacturing',
#     CHAW = 'Retail Price Index',
#     `OECD/MEI_CLI_LOLITOAA_GBR_M`='OECD CLI - UK'
#
# )
#
# tp$new(
#     'USDM,K646,D7BT,IKBH,K222,BQKU,CT2AM-AW,BQKR,S2KU,JVZ7,K22A,OECD/MEI_CLI_LOLITOAA_GBR_M'
# )$set_y1( 2014 )$set_facet_cols(3)$set_y2( 2017 )$set_breaks("1 years")$set_breaks_fmt("%Y")$set_avg(F)$set_fx('m')$set_scale(1)$set_brexit_mode(T)$set_delta_x(3)$set_delta_y(0,2)$set_pc('4')$set_dp(1)$plot_pc(
#     ytitle="12-month % change", is_smart =T, smart_labels = smart_labels
# )
#

# my_labels = list(
#     K23Q = 'Index of Production',
#     JQG2 = 'Turnover',
#     JQF8 = 'Exports',
#     MC9A = 'Input Prices'
# )
#
# tp$new(
#     'K23Q,JQG2,JQF8,MC9A'
# )$set_y1( 2013 )$set_facet_cols(2)$set_y2( 2017 )$set_breaks("1 years")$set_breaks_fmt("%Y")$set_avg(T)$set_fx('m')$set_scale(1)$set_brexit_mode(T)$set_delta_x(3)$set_delta_y(0,2)$set_pc('12')$set_dp(1)$plot_pc(
#     ytitle="12-month % change", is_smart=T, smart_labels = my_labels , min_max_days= 90
# )


# abc <- tp$new(
#     'USDM,K646,D7BT,IKBH,K222,BQKU,CT2AM-AW,BQKR,S2KU,JVZ7,K22A,OECD/MEI_CLI_LOLITOAA_GBR_M'
# )$set_y1( 2014 )$set_facet_cols(3)$set_y2( 2017 )$set_breaks("1 years")$set_breaks_fmt("%Y")$set_avg(F)$set_fx('m')$set_scale(1)$set_brexit_mode(T)$set_delta_x(3)$set_delta_y(0,2)$set_pc('12')$set_dp(1)$plot_pc(
#     ytitle="12-month % change", is_smart =T, min_max_days= 70, return_data = T,
#     smart_labels = list(
#
#         USDM="Exchange - US Dollar",
#         `ABMI-PN2`="Gross Domestic Product",
#         K646="PPI - Input Prices",
#         D7BT = "Consumer Price Index",
#         IKBH = "Exports - Value",
#         BQKU = 'Exports - Volume Index',
#         BQKR = 'Exports - Price Index',
#         K222 = 'Index of Production',
#         NPEL = 'Business Investment',
#         `CT2AM-AW` = 'Construction Output',
#         S2KU = 'Index of Services',
#         JVZ7 = 'PPI - Output Prices',
#         K22A = 'Index of Manufacturing',
#         CHAW = 'Retail Price Index',
#         `OECD/MEI_CLI_LOLITOAA_GBR_M`='OECD CLI - UK'
#
#     )
# )

# get_indicator_pos <- function( code = 'IKBH' ){
#     sq <- storedQry::SQ$new('R:/shiny/beama/bmonitor/bss.sqlite')
#     abc<- sq$set_name('tdi_get_indicator_position_pc')$set_params(
#         list(`@s_code`= code)
#     )$qry_exec()
#     return( abc)
# }
#
# beamaUtils::get_fxn('tdi_indicator_pc')('K646_MM12')
#
# get_indicator_pos('IKBH_MM12')

#tp.plot_regression_code()
#tp.plot_regression_code( k = '12')
#tp.plot_regression_code( fx='qtr', sig_fig = 2)
#tp.plot_regression_code( fx='yr', sig_fig = 2)
#tp.plot_regression_code( fx='mat', sig_fig = 2)
#tp.plot_regression_code( fx='ytd', sig_fig = 2)
#tp.plot_regression_code( fx='mat',k='12', sig_fig = 2)

#tp.plot_regression_code( 'm_elec', 'l_elec', fx='mth',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'usdm',   'm_elec', fx='mth',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'usdm',   'm_elec', fx='mat',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'm_elec', 'usdm', fx='qtr',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'm_elec', 'm_elec', fx='qtr',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'l_elec', 'l_mech', fx='qtr',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'm_elec', 'k646', fx='mth',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'l_elec', 'k646', fx='mat',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'l_elec', 'k646', fx='mat',select_yr=c(2010,2016), sig_fig = 2,k='12')
#tp.plot_regression_code( 'l_elec', 'd7bt', fx='mat',select_yr=c(2010,2016), sig_fig = 2,k='12')
#tp.plot_regression_code( 'm_mech', 'k646', fx='mth',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'm_elec', 'k646', fx='mth', k='12',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'usdm',   'm_elec', fx='mth',k='12',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'usdm',   'm_elec', fx='mat',k='12',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'usdm',   'abmi', fx='qtr',k='4',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'd7bt',   'abmi', fx='qtr',k='4',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'd7bt',   'm_elec', fx='mth',k='12',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'chaw',   'm_composite', fx='mth',k='12',select_yr=c(2010,2016), sig_fig = 2)
#tp.plot_regression_code( 'usdm',   'eurm', fx='qtr',k='4',select_yr=c(2010,2017), sig_fig = 2)
#tp.plot_regression_code( 'eurm',   'usdm', fx='qtr',k='4',select_yr=c(2010,2017), sig_fig = 2)

#beamaTrends::tp.view_spider('D7BT')
#beamaTrends::tp.view_spider('D7BT', fx = 'qtr')
#tp.view_spider('D7BT', fx = 'qtr', k=2)
#tp.view_spider('D7BT', fx = 'qtr', y1=2007)
#tp.view_spider('D7BT', fx = 'mat', y1=2012,is_brexit=T)
#tp.view_spider('D7BT', fx = 'mat', y1=2014, k=3)
#tp.view_spider('D7BT', fx = 'mat', y1=2013, k=6,is_brexit=T)
#abc <- tp.view_spider('D7BT', fx = 'mat', y1=2014, k=12)
#tp.view_spider('D7BT', fx = 'mat', y1=2014, k=12,is_brexit=T)

#beamaTrends::tp.view_spider('D7BT')
#beamaTrends::tp.view_spider('D7BT', fx = 'qtr')
#tp.view_spider('D7BT', fx = 'qtr', k=2)
#tp.view_spider('D7BT', fx = 'qtr', y1=2007)
#tp.view_spider('D7BT', fx = 'mat', y1=2012,is_brexit=T)
#tp.view_spider('D7BT', fx = 'mat', y1=2014, k=3)
#tp.view_spider('D7BT', fx = 'mat', y1=2013, k=6,is_brexit=T)
#abc <- tp.view_spider('D7BT', fx = 'mat', y1=2014, k=12)
#tp.view_spider('D7BT', fx = 'mat', y1=2014, k=12,is_brexit=T)


#tp.view_spider('k646', fx = 'mat', y1=2014, k=3)
#tp.view_spider('k646', fx = 'mat', y1=2014, k=6)
#tp.view_spider('k646', fx = 'mat', y1=2010, k=12 ,is_brexit=T)

#tp.view_spider('EURM', fx = 'mat', y1=2014, k=3)
#tp.view_spider('EURM', fx = 'mat', y1=2014, k=6)
#tp.view_spider('EURM', fx = 'mat', y1=2014, k=9)
#tp.view_spider('EURM', fx = 'mat', y1=2007, k=12, is_brexit = T)
#tp.view_spider('USDM', fx = 'mat', y1=2007, k=12, is_brexit = T)
#tp.view_spider('eurm', fx = 'mat', y1=2014, k=12)
#tp.view_spider('CT2AM-AW', fx = 'mat', y1=2015, k=12)
#tp.view_spider('CT2AM-AW', fx = 'ytd', y1=2014, k=12)
#tp.view_spider('JT27', fx = 'mat', y1=2014, k=12,is_brexit=T)

#tp.view_spider('usdm', fx = 'ytd', y1=2014, k=3)
#tp.view_spider('usdm', fx = 'ytd', y1=2014, k=6)
#tp.view_spider('usdm', fx = 'ytd', y1=2014, k=12)
#tp.view_spider('usdm', fx = 'ytd', y1=2015, k=12)

# abc <- tg_ons$new('DYDC')$add_trends_data(x = window(dt,start=c(1990,1)) )

#tp.view_spider('dydc', fx = 'mat', y1=2012, k=4, is_brexit = T)
#tp.view_spider('mgsc', fx = 'mat', y1=2012, k=12, is_brexit = T)
#tp.view_spider('dydc', fx = 'qtr', y1=2014, k=4, is_brexit = T)
#tp.view_spider('abmi-ukea', fx = 'ytd', y1=2012, k=4, is_brexit = T)


# require(ggplot2)
# x <- c(1)
# y <- c(0)
# xmin <- c(1)
# xmax <- c(2)
# ymin<- c(0)
# ymax<- c(5)
# tx <- c( ( xmin+xmax)/2 )
# ty<- c( (ymin+ymax)/2 )
# tt <- c('my text')
# ta <- c(90)
#
#
# abc <- data.frame(
#     x = x, y= y,xmin = xmin,xmax=xmax, ymin=ymin,ymax=ymax,
#     tx= tx, ty=ty , ta=ta,tt=tt)
#
# ggplot2::ggplot(data=abc, aes(x=x, y=y,xmin=xmin,xmax=xmax,ymin=ymin,ymax=ymax))+ggplot2::geom_rect()
# +ggplot2::annotate(data=abc, aes(x=tx,y=ty,label=tt,angle = ta),colour=beamaColours::get_darkyellow())
#

# smart_fx <- list(
#     USD = 'US Dollar ($)',
#     EUR = 'Euro (€)'
# )
# tp$new(
#     "EUR,USD"
# )$set_y1( 2015 )$set_title("Sterling Exchange Rate")$set_y2( 2017 )$set_avg(T)$set_fx('d')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(F)$set_scale(1)$set_brexit_mode(
#         T)$set_colour(beamaColours::get_euris_blue_light())$set_delta_x(2)$set_delta_y(0,0)$set_pc('0')$set_dp(4)$plot_pc(
#             strip_col= beamaColours::get_euris_blue(), min_max_days = 100,
#             smart_labels = smart_fx, is_smart =T, ytitle = "Daily Rate (per 1 GBP)"
#         )

# smart_fx <- list(
#     USDM = 'US Dollar ($)',
#     EURM = 'Euro (€)'
# )
# tp$new(
#     "EURM,USDM"
# )$set_y1( 2015 )$set_title("Sterling Exchange Rate")$set_y2( 2017 )$set_avg(T)$set_fx('m')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(T)$set_scale(1)$set_brexit_mode(
#         T)$set_colour(beamaColours::get_euris_blue_light())$set_delta_x(2)$set_delta_y(0,0)$set_pc('1')$set_dp(1)$plot_pc(
#             strip_col= beamaColours::get_euris_blue(), min_max_days = 100,
#             smart_labels = smart_fx, is_smart =T, ytitle = "1-month % change"
#         )

#cde <- beamaTrends::td$new('K646')
#beamaTrends::td$new('K646')$get_growth_since(by='m',dt1='2003-01-01', is_brexit = F)
#beamaTrends::td$new('K646')$get_growth_since(by='m',dt1='2003-01-01',dt2='2015-01-01', is_brexit = F)
#beamaTrends::td$new('ABMI')$get_growth_since(by='q',dt1='2016-03-01', is_brexit = F)
#beamaTrends::td$new('ABMI-UKEA')$get_growth_since(by='q',dt1='2015-06-01', dt2='2016-06-01', is_brexit = F)
#beamaTrends::td$new('ABMI-UKEA')$get_growth_since(by='q',dt1='2016-06-01', dt2='2017-10-01', is_brexit = F)


#beamaTrends::td$new('K646')$get_hilow(k=12,is_low = T)
#beamaTrends::td$new('K646')$get_hilow(k=1,is_low = T)
#beamaTrends::td$new('K646')$get_hilow(k=1,is_low =F)
#beamaTrends::td$new('K646')$get_hilow(k=12,is_low =F)
#td.delt( onsR2::download('k646')$m_data, k = 1, percent = T, dp = 1)
#beamaTrends::td$new('abmi')$get_hilow(k=4,is_low =T)
#td$new('abmi')$get_hilows()

#abc <- td$new(x = 'SPPG-DWD', db_name = 'R:/packages/bistats/inst/extdata/bistats.sqlite')$get_hilows()

# smart_fx <- list(
#     `SPPG-DWD` = 'Domestic Wiring Devices',
#     `SPPG-DCP` = 'Domestic Circuit Protection'
# )
#
# tp$new(
#     "SPPG-DWD,SPPG-DCP", db_name = 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# )$set_y1( 2015 )$set_title("Single Phase Products")$set_y2( 2017 )$set_avg(T)$set_fx('m')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(T)$set_scale(1e6)$set_brexit_mode(
#         T)$set_colour(beamaColours::get_grayblue())$set_delta_x(2)$set_delta_y(5,0)$set_pc('0')$set_dp(1)$plot_pc(
#             strip_col= beamaColours::get_grayblue(), min_max_days = 100,
#             smart_labels = smart_fx, is_smart =T, ytitle = "Monthly Sales (Million GBP)"
#         )
#
# smart_fx <- list(
#     `SPPG-DWD` = 'Domestic Wiring Devices',
#     `SPPG-DCP` = 'Domestic Circuit Protection'
# )
#
# tp$new(
#     "SPPG-DWD,SPPG-DCP", db_name = 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# )$set_y1( 2015 )$set_title("Single Phase Products")$set_y2( 2017 )$set_avg(T)$set_fx('m')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(T)$set_scale(1e6)$set_brexit_mode(
#         T)$set_colour(beamaColours::get_grayblue())$set_delta_x(2)$set_delta_y(5,0)$set_pc('0')$set_dp(1)$plot_pc(
#             strip_col= beamaColours::get_grayblue(), min_max_days = 100,
#             smart_labels = smart_fx, is_smart =T, ytitle = "Monthly Sales (Million GBP)"
#         )
#
# smart_fx <- list(
#     `SPPG-DWD` = 'Domestic Wiring Devices',
#     `SPPG-DCP` = 'Domestic Circuit Protection'
# )
#
# tp$new(
#     "SPPG-DWD,SPPG-DCP", db_name = 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# )$set_y1( 2015 )$set_title("Single Phase Products")$set_y2( 2017 )$set_avg(T)$set_fx('m')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(T)$set_scale(1e6)$set_brexit_mode(
#         T)$set_colour(beamaColours::get_grayblue())$set_delta_x(2)$set_delta_y(5,0)$set_pc('0')$set_dp(1)$plot_pc(
#             strip_col= beamaColours::get_grayblue(), min_max_days = 100,
#             smart_labels = smart_fx, is_smart =T, ytitle = "Monthly Sales (Million GBP)"
#         )

# smart_fx <- list(
#     `SPPG-DWD` = 'Domestic Wiring Devices',
#     `SPPG-DCP` = 'Domestic Circuit Protection'
# )
#
# tp$new(
#     "SPPG-DWD,SPPG-DCP", db_name = 'R:/packages/bistats/inst/extdata/bistats.sqlite'
# )$set_y1( 2015 )$set_title("Single Phase Products")$set_y2( 2017 )$set_avg(T)$set_fx('m')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(T)$set_scale(1e6)$set_group(
#         T)$set_colour(beamaColours::get_grayblue())$set_delta_x(2)$set_delta_y(5,0)$set_pc('0')$set_dp(1)$plot_pc(
#             strip_col= beamaColours::get_grayblue(), min_max_days = 100,
#             smart_labels = smart_fx, is_smart =T, ytitle = "Monthly Sales (Million GBP)"
#         )

# abc <- tf$new('k646')
# gg <- tf$new('k646')$set_ahead(1)$get_fcs(avg_only = T)
#  tf$new('k646')$set_ahead(1)$plot_fcs(avg_excl = 'meanf,holt,holtw')

# y <- rpois(20,lambda=.3)
# fcast <- croston(y)
# plot(fcast)

# x <- bistats::pg.get_indx_ts('TOTAL')
# a <- window(x,c(2003,1),c(2015,12))
# b <- window(x,c(2016,1),c(2017,8))
# tfo <- tf$new(a)$set_ahead(2)$set_method_packed('arima,thetaf,mapa,bsts')
# fco <- tfo$get_fcs(avg_excl = 'x12', avg_join = b, is_spread = FALSE)
# tf$new(a)$plot_fcs( plot_excl = NULL, fcs_object = fco, avg_join = 1)
# head(tfo$recalc_forecast_avg(fco = fco, avg_only = T, is_spread = T), 20)
# head( tfo$recalc_forecast_avg(fco = fco,is_spread = T, avg_excl = 'rwf,bsts,meanf,holtw,holt', avg_only = T),20)


 # x <- beamaTrends::tp.view_data('ABMI-UKEA')
 # a <- window(x, c(2009,1), c(2015,4))
 # b <- window(x, c(2016,1), c(2017,4))
 #
 # tf <- beamaTrends::tf$new( a )$set_ahead(2)
 #
 # fco <- tf$get_fcs( avg_excl = NULL, avg_join = b, is_spread = FALSE)
 # tf$plot_fcs( plot_excl = 'rwf,meanf,thetaf', fcs_object = fco, avg_join = 1, excl_limits = T)


# (
#     total_fc <- tf$new(
#
#         bistats::pg.get_indx_ts('BEAMA')
#
#     )$set_ahead( 2 )$set_method_packed( 'x12,arima,mapa' )$set_x12_exe_path(
#
#           'C:/apps/WinX13/x13as/x13as.exe'
#
#     )$set_x12_output_path(
#
#        'C:/apps/WinX13/output/'
#
#      )$get_fcs( avg_only = T)
#
# )

# beamaTrends::tp.plot_brexit_indicators(
#
#     indicators =  'USDM,EURM,K646,D7BT'
#     ,out_gap = 15
#     ,dt1 = c(2016,6)
#     ,dt2 = c(2017,10)
#     ,title = 'UK Economic Indicators'
#     ,is_brexit = F
#     ,is_smart = T
#     ,smart = ESMART_LABELS
#     ,ytitle ='Growth since June 2016'
# )
#

# abc =  beamaTrends::tg$new(
#     beamaTrends::tp_data$new('topsi_orgalime')$get_group(T)
# )$plot(
#     select = 'YR',select_yr =c(2000,2017), x_delta = c(1,1), y_delta =c(0,0), skale =1000,
#     skale_title = "Monthly Turnover (GBP billion)", is_themed = T, x_breaks = '2 years', smart_labels = list(YR = 'Orgalime Turnover - NACE 25,26,27,28,33')
# )
#

# beamaTrends::tg$new(
#     beamaTrends::tp_data$new('topsi_orgalime')$get_group(T)
# )$plot(
#     T,select = 'YY1', select_yr =c(2013,2016), x_delta = c(1,1), y_delta =c(1,1), skale =1, ops = 'avg',
#     skale_title = "Annual Growth (%)", is_themed = T, x_breaks = '1 years', smart_labels = list(YY1 = 'Orgalime Turnover - NACE 25,26,27,28,33')
#     #,brexit_mode = T
# )

# beamaTrends::tg$new(
#     beamaTrends::tp_data$new('emp_orgalime_core')$get_group(T)
# )$plot(
#     is_growth = T,
#     select = 'YY1',select_yr =c(2013,2017), x_delta = c(1,1), y_delta =c(0,0.5), skale =1, ops = 'avg',
#     skale_title = "Growth - Employed Persons (yoy%)", is_themed = T, x_breaks = '1 years', smart_labels = list(YY1 = 'Orgalime Employment - NACE 25,26,27,28,33')
#     #,brexit_mode = T
# )
# beamaTrends::tp_utils$new()$plot_save('employment_yr_combined_pc',path = GRAPHIC_PATH, skale = 3)


# my_smart = list(
#      USD='US Dollar ($)'
#     ,EUR = 'Euro (€)'
#     ,JPY = 'Japanese (¥)'
#     ,CNY = 'Chinese Yuan Renminbi (¥)'
#     ,INR = 'Indian Rupee'
#     ,BRL = ' Brazillian Real (R$)'
# )
#
#
# tp$new(
#     'USD,EUR,JPY,CNY,BRL,INR'
# )$set_y1( 2014 )$set_y2( 2017 )$set_avg(T)$set_fx('m')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(F)$set_scale(1)$set_colour(
#         beamaColours::get_grayblue())$set_delta_x(2)$set_delta_y(0,0)$set_pc('12')$set_dp(1)$plot_pc(
#             strip_col= beamaColours::get_grayblue(), min_max_days = 100,
#             smart_labels = my_smart, is_smart =T, ytitle = "12-month % change"
#         )


# smart_caps_emp <- list(
#     `PROD-SIC-26` = '26 - Electronics',
#     `PROD-SIC-27` = '27 - Electrical',
#     `PROD-SIC-28` = '28 - Mechanical',
#     `PROD-SIC-25` = '25 - Fabricated Metals',
#     `PROD-SIC-33` = '33 - Repairs & Installation',
#     `PROD-SIC-29` = '29 - Motor Vehicles',
#     `PROD-SIC-30` = '30 - Other Transport',
#     `PROD-SIC-C`  = 'C - Total Manufacturing'
#
# )
# beamaTrends::tp$new(
#     "PROD-SIC-25,PROD-SIC-26,PROD-SIC-27,PROD-SIC-28,PROD-SIC-33,PROD-SIC-29,PROD-SIC-30,PROD-SIC-C" #beamaTrends::tp_data$new('emp_orgalime_core')$get_group(T)
# )$set_y1( 2000 )$set_y2( 2020 )$set_avg(F)$set_fx('q')$set_breaks(
#     "5 years")$set_breaks_fmt("%Y")$set_min_max(F)$set_scale(1)$set_brexit_mode(
#         T)$set_colour(beamaColours::get_euris_blue_light())$set_delta_x(2)$set_delta_y(0,0)$set_pc('4')$set_dp(1)$plot_pc(
#             ytitle="Output / Head (GBP)",strip_col= beamaColours::get_euris_blue(),
#             smart_labels = smart_caps_emp, is_smart =T
#)


# wh_all <- paste(whlist, sep="",collapse = ",")
# wh_top_list <-  c('EDMUNDSON','FEGIME','ANEW','OTHERS','ELECTRICCENTRE','NEWEY')
# wh_top <- paste(wh_top_list,sep="",collapse = ",")
# wh_bot_list <- c('AWEBB','CEF','DENMANS','WFSENATE','WILTS','YESSS','SCREWFIX','RETAIL')
# wh_bot <- paste(wh_bot_list, sep = "", collapse = ",")
# retail_list <- c('SCREWFIX','RETAIL')
# retail_all <- paste( retail_list, sep = '', collapse = ',')
#
# wh_smart <- list(
#     OTHERS = 'OTHER WHOLESALERS',
#     ELECTRICCENTRE = 'ELECTRIC CENTRE',
#     WFSENATE = 'WF SENATE',
#     RETAIL = 'OTHER RETAILERS'
# )
# beamaTrends::tp$new(
#     paste('PG', wh_top_list,sep='-',collapse = ','), db = bistats::bistats_utils$new()$get_db()
# )$set_yintercept(100)$set_y1( 2015 )$set_y2( 2020 )$set_avg(T)$set_fx('m')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(T)$set_scale(1)$set_brexit_mode(
#         F)$set_delta_x(2)$set_delta_y(0,0)$set_pc('0')$set_dp(0)$plot_pc(
#             min_max_days = 100,
#             smart_labels = wh_smart, is_smart =T, ytitle = "Index (Jan 2013 = 100)"
#         )

# df_share <- bistats::pg.get_data(y1 = 2014)
# df_share <- sqldf::sqldf("select yr,mth,wholesaler,sum(total) as total from df_share group by yr,mth,wholesaler ")
# df_share <- dplyr::mutate(
#     dplyr::group_by( df_share, yr,  mth),
#     pc = 100*( total / sum(total) ),
#     dy = 1,
#     data_desc = wholesaler
# )
#
# names(df_share) <- c('yr','mth','data_code','value','pc','dy','data_desc')
#
# beamaTrends::tp$new(
#     df_share
# )$set_y1( 2014 )$set_y2( 2020 )$set_avg(T)$set_fx('m')$set_breaks(
#     "1 year")$set_breaks_fmt("%Y")$set_min_max(T)$set_scale(1)$set_delta_x(
#         2)$set_delta_y(0,0)$set_pc('0')$set_dp(0)$plot_pc(
#             min_max_days = 100, is_smart =T, ytitle = "Market Share (%)"
#         )
