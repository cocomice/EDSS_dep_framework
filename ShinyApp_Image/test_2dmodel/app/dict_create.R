# Please run it within R under Unix-like environment to ensure
# all strings are encoded in UTF-8

rm(list = ls())
library(htmltools)

## ===== version info =====
versionInfo <- list(
  cn = HTML(c("版本号 5.2<br/>G.Pedrazzini (1), W. Kinzelbach (1), Y. Li (1), D.Jaeger (2)<br/>(1) ETH Zurich, IfU, (2) Geopraevent AG\n        <br/> <a href=\"mailto:yu.li@ifu.baug.ethz.ch\">联系我们</a> , 或者点击 <a href=\"http://www.ifu.ethz.ch/projects/china-groundwater-management-project/research/ogm.html\">这里</a> 查看更多相关信息.")),

  en = HTML(c("Version 5.2<br/>G.Pedrazzini (1), W. Kinzelbach (1), Y. Li (1), D.Jaeger (2)<br/>(1) ETH Zurich, IfU, (2) Geopraevent AG\n        <br/> <a href=\"mailto:yu.li@ifu.baug.ethz.ch\">Contact us</a> , or click <a href=\"http://www.ifu.ethz.ch/projects/china-groundwater-management-project/research/ogm.html\">here</a> for more information"))
)

linkInfo <- list(
  hdr = c("相关链接:", "Useful links:"),

  Database    = c("黑河数据平台", "Heihe data portal"),
  ProjectSite = c("项目主页", "Project website"),
  HydroApp    = c("Hydrosolution 公司软件", "Hydrosolution Apps")
)

## ===== district selector =====
dist_choice <- list(
  cn = list(
    "上三" = 1,
    "大满" = 2,
    "盈科" = 3,
    "乌江" = 4,
    "西干" = 5,
    "甘浚" = 6,
    "沙河" = 7,
    "骆驼城" = 8,
    "鸭暖" = 9,
    "板桥" = 10,
    "平川" = 11,
    "廖泉" = 12,
    "三清" = 13,
    "友联" = 14,
    "新华" = 15,
    "倪家营" = 16,
    "小屯" =17,
    "六坝" = 18,
    "大湖湾" = 19,
    "罗城" = 20,
    "肃南" = 21
  ),

  en = list(
    "Shangsan"    = 1,
    "Daman"       = 2,
    "Yingke"      = 3,
    "Wujiang"     = 4,
    "Xigan"       = 5,
    "Ganjun"      = 6,
    "Shahe"       = 7,
    "Luotuocheng" = 8,
    "Yanuan"      = 9,
    "Banqiao"     = 10,
    "Pingchuan"   = 11,
    "Liaoquan"    = 12,
    "Sanqing"     = 13,
    "Youlian"     = 14,
    "Xinhua"      = 15,
    "Nijiaying"   = 16,
    "Xiaotun"     = 17,
    "Liuba"       = 18,
    "Dahuwan"     = 19,
    "Luocheng"    = 20,
    "Sunan"       = 21
  )
)

## ===== Server file =====
Server_char_dict <- list(

  ## ===== __progress bar message =====
  progressBar = list(
    msg_main = c("更新中...", "Updating..."),
    msg_a    = c("计算初始数据...", "Calculating inputs..."),
    msg_b    = c("开始运行模型...", "Running model..."),
    msg_c    = c("提取结果...", "Extracting results..."),
    msg_d    = c("绘制结果...", "Plotting..."),
    msg_e    = c("完成!", "Done!")
  ),

  ## ===== __leaflet map gadgets =====
  map = list(
    group_bdy    = c("模型边界", "Boundary"),
    group_canals = c("灌区", "Channels"),
    legend       = c("水位变化 [米]", "Level changes [m]"),
    btReset      = c("重置地图", "reset map"),
    LblTxt       = matrix( c("最终水位变化(非稳态)", "Transient final head change",
                             "最终水位变化(稳态)", "Steady state head change"),
                           nrow=2)
  ),

  ## ===== __plot related texts =====
  plot = list(
    drawdown_title = c("水位变化 [米] (负值表示水位下降)", "change of water level [m] (negative as drawdown)"),

    drawdown_lab_x = c("时间 [年]", "time [year]"),

    bar_group = matrix( c("地表水", "surface water",
                          "地下水", "ground water"),
                        nrow = 2),

    bar_lab_y = c("耗水量 [万立方米/年]", "Consumption [x10\u2074 m\u00b3/year]"),

    heatmap_lab_x = matrix(c("一月", "Jan.",
                             "二月", "Feb.",
                             "三月", "Mar.",
                             "四月", "Apr.",
                             "五月", "May.",
                             "六月", "Jun.",
                             "七月", "Jul.",
                             "八月", "Aug.",
                             "九月", "Sep.",
                             "十月", "Oct.",
                             "十一月", "Nov.",
                             "十二月", "Dec."), nrow = 2),

    table_sta_colname = matrix( c("灌溉面积 [万亩]",   "Irrigation area [x10\u2074 mu]",
                                  "较2013年面积",     "w.r.t 2013's area",
                                  "地表水用量",        "Surface water usage",
                                  "较2013年地表水用量", "w.r.t 2013's SW usage",
                                  "地下水用量",        "Groundwater usage",
                                  "较2013年地下水用量", "w.r.t 2013's GW usage"),
                                nrow = 2),

    font_type = c("SimHei", "sans-serif")
  ),

  ## ===== __general text info. =====
  testInfo = list(
    note_info1 = c("使用说明", "How-to:"),
    note_info2 = matrix( c("选择一个灌区", "choose a district;",
                           "使用滑条改变灌溉参数", "specify the irrigation conditions using sliders;"),
                         nrow =2 ),
    note_info3 = matrix( c("下载模板文件", "download the template file;",
                           "修改文件内的灌溉参数", "modify its content using Excel;",
                           "重新上传文件", "upload it back;"),
                         nrow =2 ) )
)


## ===== UI file =====
UI_char_dict <- list(

  ## ===== __Shiny App title =====
  titleWeb = c("黑河在线地下水模拟平台", "Heihe Online Groundwater Modelling Platform"),

  ## ===== __district name =====
  dist_name = matrix( c( "上三", "Shangsan",
                         "大满",   "Daman",
                         "盈科",   "Yingke",
                         "乌江",   "Wujiang",
                         "西干",   "Xigan",
                         "甘浚",   "Ganjun",
                         "沙河",   "Shahe",
                         "骆驼城", "Luotuocheng",
                         "鸭暖",   "Yanuan",
                         "板桥",   "Banqiao",
                         "平川",   "Pingchuan",
                         "廖泉",   "Liaoquan",
                         "三清",   "Sanqing",
                         "友联",   "Youlian",
                         "新华",   "Xinhua",
                         "倪家营",  "Nijiaying",
                         "小屯",   "Xiaotun",
                         "六坝",   "Liuba",
                         "大湖湾", "Dahuwan",
                         "罗城",   "Luocheng",
                         "肃南",   "Sunan"), nrow = 2),

  ## ===== __button texts =====
  BT_str = list(
    reset    = c("重置", "RESET"),
    update   = c("更新", "UPDATE"),
    disp     = c("显示更多结果", "Show more info."),
    download = c("下载模板", "template"),
    upload   = list(
      bt_label  = c("浏览", "Browse"),
      bt_holder = c("请上传.xls格式文件", "Please upload .xls file")
    ),
    input_switch = list(
      cn = list(
        "按灌区设置"       = 1,
        "上传灌溉配置文件" = 2
      ),
      en = list(
        "using district selector" = 1,
        "upload scenario file"    = 2
      )
    ),

    yr_seltor = c("选择年份", "Choose Year"),

    map_switch = list(
      cn = list(
        "非稳态结果" = "tr",
        "稳态结果"  = "ss"
      ),
      en = list(
        "Transient"    = "tr",
        "Steady state" = "ss"
      )
    )
  ),

  ## ===== __header texts =====
  hd_str = list(
    dist     = c("灌区设置选项", "Irrigation Settings"),
    upload   = c("或者上传灌溉配置文件", "Or upload your own scenario file"),
    boundary = c("气候条件设置", "Climate Condition")
  ),

  ## ===== __tab header text =====
  tab_str = list(
    tab_a = c("地下水水位变化", "Change of Water Level"),
    tab_b = c("灌溉耗水量统计", "Statistics of Irrigation"),
    tab_e = c("相关链接", "Other links")
  ),

  ## ===== __slider text =====
  slider_str = list(
    area_label  = c("灌溉面积 [万亩]", "irrigation area [x10<sup>4</sup> mu]"),
    area_SWprec = c("地表水灌溉比例",  "surface irrigation ratio [%]"),
    area_watUse = c("灌溉用水需求 [立方米/亩/年]", "irrigation requirement [m<sup>3</sup>/mu/year]"),
    riv_label   = c("莺落峡来水量 [亿立方米/年]", "Yingluoxia inflow [x10<sup>8</sup> m<sup>3</sup>/year]"),
    bdry_label  = c("边界流量 [亿立方米/年]", "boundary flux [x10<sup>8</sup> m<sup>3</sup>/year]")
  ),

  ## ===== __sub-window header text =====
  modal_lab = list(
    disp_mod1 = c("各灌区灌溉耗水量 [万立方米/年]", "Irrigation consumption per district [x10<sup>4</sup> m<sup>3</sup>/year]")
  ),

  selecter_dist = c("请选择一个灌区", "please select a district")
)

## ===== save data =====
save.image("R_data/ShinyApp_char_dict.RData")
