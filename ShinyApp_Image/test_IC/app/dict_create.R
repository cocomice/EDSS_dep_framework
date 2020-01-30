# - This file generate the text for UI interface
# - Please run it within R under Unix-like environment to ensure
#   all strings are encoded in UTF-8

rm(list = ls())

library(htmltools)

## ==== Link and version info ====
versionInfo <- list(
  cn = HTML(c("版本号 1.0; <a href=\"mailto:yu.li@ifu.baug.ethz.ch\">联系我们</a> ")),
  en = HTML(c("Version 1.0; <a href=\"mailto:yu.li@ifu.baug.ethz.ch\">Contact us</a> "))
)


## ==== District Names ====
dictNames <- matrix(c(
  "路桥乡", "Luqiao",
  "魏僧寨", "Weisen",
  "南徐村", "Nanxu",
  "柴堡镇", "Chaibao",
  "寿山寺", "Shoushan",
  "馆陶镇", "Guantao",
  "房寨镇", "Fangzhai",
  "王桥乡", "Wangqiao"
), nrow = 2)

## ==== Progress bar message ====
ProgressBar <- list(
  msg_main = c("更新中...", "Updating..."),
  msg_a = c("计算初始数据...", "Calculating inputs..."),
  msg_b = c("开始运行模型...", "Running model..."),
  msg_c = c("提取结果...", "Extracting results..."),
  msg_d = c("绘制结果...", "Plotting..."),
  msg_e = c("完成!", "Done!")
)


## ==== Home Tab ====
UI_Home_Tab <- list(
  hdr_str = list(
    h_a = c("背景信息", "Background"),
    h_b = c("决策分析模块", "Decision Support Modules")
  ),

  tab_str = list(
    tab_a = c("主页", "Home"),
    tab_b3 = c("估算作物需水量", "Crop water demand estimation")
  ),

  bt_str = list(
    bt_a = c("前往此模块", "Go to the Module")
  ),

  module_str = list(

    module_c = list(
      cn = c("此模块内嵌了国际粮农组织的作物需水量模型，通过模型演算我们可以计算不同作物种类作物
              的月需水量。输入参数包括：<br><br>
              1. 作物名称; <br>
              2. 种植日期;"),
      en = c("This module runs a crop growth model based on FAO's AquaCrop to estimate
             the water demand of given crops under different climate conditions. Outputs are
             monthly crop water demand (CWD) of a given crop and its annual water requirement.
             Inputs from the user include: <br><br>
             1. crop type of interest;<br>
             2. planting date;")
    )
  ),


  intro_str = list(
    cn = c("像多数华北平原上的农业地区一样，馆陶县农业灌溉主要依赖于地下水。在这里80%的地下水用于农业灌溉, 
           剩余的则提供给工业和生活用水。馆陶县总面积为456平方公里，其中耕地面积为48万亩，总人口约36万人。
           目前主要水源分为三个：平均520毫米每年的降雨，地下水的开采，以及从魏运河和黄河调来的地表水资源。
           由于城市地区对山区径流的利用，只有少量的地表水通过河道流入华北平原地区。因此地下水成为农业灌溉
           的主要来源。当前馆陶地区的地下水水位埋深约46米，而逐年回落的水位导致当地每隔7、8年就必须重新打
           井。<br><br>
           在过去这里的农业以大豆和小米等单季作物为主，现如今转变成夏玉米加冬小麦的全年两季种植模式。而夏季
           降雨不足以满足全年800毫米的灌溉需求。当地农民表示大量的小农田种植并不能带来额外收益，而仅仅只是
           传统和生活习惯的驱使。这类问题在华北平原其他地方也同样存在，因此解决好馆陶的地下水超采问题所带来
           的方法和经验可以为整个华北平原的地下水治理提供重要的借鉴。"),

    en = c("The pilot site of Guantao County has the general features of a county in North China Plain (NPC):
           intensive irrigated agriculture mainly relying on groundwater. The county is using 80 percent
           of its water resources for agricultural irrigation. The rest is used by households and
           industry. It covers an area of 456 km2 including 480,000 ha of crop land and is home to a
           population of 360,000 people.
           The present water supply is depending mainly on 3 sources: the precipitation of about
           520 mm/a, the exploitation of shallow and deep groundwater layers, and some surface
           irrigation water provided from a reservoir via the Weiyun River and from the Yellow River.
           Due to retention of streams in the mountains for urban water supply, most rivers bring
           only very little water into the North China Plain. Irrigation has therefore been
           relying mainly on groundwater. Groundwater level is currently at about 46m below ground
           level and due to the falling water table agricultural wells need to be
           re-drilled every 7-8 years.<br><br>
           In the past the major crops used to be soybeans and millet in a single cropping system,
           nowadays they are maize and wheat in a double cropping system. Rain is basically sufficient
           to support a single summer grain crop per year. Today irrigation is needed as rainfall
           is about 500mm and the current double cropping system of winter wheat and summer maize
           requires at least 800 mm. Local partners claim the small scale farming is hardly profitable
           and the reason why many farmers still plant wheat is not the economic revenue, but rather
           the tradition and the habit to cultivate the land as they have done for generations.
           The problem of Guantao county is representative for a much larger area. A solution to
           it can in principle be scaled up to the whole North China Plain and possibly other places
           in North China.")
  )
)


## ==== IC Tab ====
UI_IC_Tab <- list(

  ## ==== __tab labels ====
  tab_str = list(
    tab_a = c("灌溉计算器", "Irrigation Calculator"),
    tab_b = c("用水定额表", "Irrigation Norms"),
    tab_c = c("模型计算", "Model Estimate"),
    tab_d = c("查询定额", "Irrigation Norm")
  ),

  hd_str = list(
    h1 = c("请选择参数", "Select Parameters")
  ),

  ## ==== __buttom ====
  BT_str = list(
    radioBT_label_a = c("作物类别", "Plant Category"),
    radioBT_label_b = c("水源", "Water Source"),
    radioBT_label_c = c("灌溉规模 [万亩]", "Irrigation Area [x10\u2074 mu]"),
    radioBT_label_d = c("土壤类型", "Soil Type"),
    radioBT_label_e = c("灌溉方式", "Irrigation Method"),
    date_label = c("种植日期", "Planting Date"),
    selector_crop_label = c("作物名称", "Crop Name"),
    selector_cropChoice1 = list(
      # TODO: number to be adapted according to the column index of input file
      cn = list(
        "玉米" = 4,
        "冬小麦" = 19,
        "棉花" = 2
      ),

      en = list(
        "Maize" = 4,
        "Winter wheat" = 19,
        "Cotton" = 2
      )
    ),

    selector_cropChoice2 = list(
      # TODO: number to be adapted according to the column index of input file
      cn = list(
        "甜瓜" = 13,
        "大豆" = 12,
        "绿豆" = 17,
        "春土豆" = 10
      ),

      en = list(
        "Sweet melon" = 13,
        "Soy bean" = 12,
        "Mung bean" = 17,
        "Spring potato" = 10
      )
    ),


    selector_cropChoice3 = list(
      # TODO: number to be adapted according to the column index of input file
      cn = list(
        "苹果" = 1,
        "梨" = 8,
        "葡萄" = 18,
        "桃子" = 6
      ),

      en = list(
        "Apple" = 1,
        "Pear" = 8,
        "Grape" = 18,
        "Peach" = 6
      )
    ),

    radioBT_irrMTD1 = list(
      cn = list(
        "渠道防渗" = 1,
        "地面灌溉" = 2
      ),
      
      en = list(
        "channel lining" = 1,
        "flooding"       = 2
      )
    ),
    radioBT_irrMTD2 = list(
      cn = list(
        "小畦灌溉" = 1,
        "管灌" = 2,
        "滴灌" = 3
      ),
      
      en = list(
        "board irrigation" = 1,
        "flooding irrigation" = 2,
        "drip irrigation" = 3
      )
    ),
    radioBT_soil = list(
      cn = list(
        "砂土" = 1,
        "壤土" = 2,
        "黏土" = 3
      ),

      en = list(
        "Sand" = 1,
        "Loam" = 2,
        "Clay" = 3
      )
    ),
    radioBT_crop = matrix(c(
      "谷物", "Grain",
      "蔬菜", "Vegetables",
      "水果", "Fruit"
    ), ncol = 3),
    radioBT_water = list(
      cn = list(
        "地表水" = 1,
        "地下水" = 2
      ),
      
      en = list(
        "surface water" = 1,
        "groundwater"   = 2
      )
    ),
    checkBT_drip = c("是否使用了滴灌", "with drip irrigation installed"),
    runIC_label = c("计算", "Calculate"),
    lookup_label = c("所有结果", "Check Results"),
    addResult_label = c("添加到结果", "Add to Planning Table")
  ),

  ## ==== __plots ====
  plt_str = list(
    bar_legend = matrix(c(
      "丰水年", "Wet Year",
      "平水年", "Normal Year",
      "枯水年", "Dry Year"
    ), ncol = 3),
    ylab = c("作物需水量 [毫米]", "Crop Water Deficit [mm]"),
    xlab = matrix(c(
      "一月份", "二月份", "三月份", "四月份", "五月份", "六月份",
      "七月份", "八月份", "九月份", "十月份", "十一月份", "十二月份",
      "Jan.", "Feb.", "Mar.", "Apr.", "May", "Jun.",
      "Jul.", "Aug.", "Sep.", "Oct.", "Nov.", "Dec."
    ), ncol = 2)
  ),

  ## ==== __tables ====
  table_str = list(
    IC_planTab = matrix(c(
      "作物名称", "Crop Name",
      "种植日期", "Planting Date",
      "丰水年需水量 [立方米/亩]", "Demand in wet year [m\u00b3/mu]",
      "平水年需水量 [立方米/亩]", "Demand in normal year [m\u00b3/mu]",
      "枯水年需水量 [立方米/亩]", "Demand in dry year [m\u00b3/mu]"
    ), ncol = 5),
    irrNorm1 = matrix(c(
      "作物名称", "Crop Name",
      "保证率 [%]", "Reliability Rate [%]",
      "土壤类型", "Soil Type",
      "用水定额 [立方米/亩]", "Quota [m\u00b3/mu]"
    ), ncol = 4),
    irrNorm2 = matrix(c(
      "作物名称", "Crop Name",
      "种植条件", "Planting Condition",
      "用水定额 [立方米/亩]", "Quota [m\u00b3/mu]"
    ), ncol = 3),
    irrNorm3 = matrix(c(
      "作物名称", "Crop Name",
      "保证率 [%]", "Reliability Rate [%]",
      "灌溉方式", "Irrigation Methods",
      "用水定额 [立方米/亩]", "Quota [m\u00b3/mu]"
    ), ncol = 4)
  )
)


## ==== save data ====
save.image("R_data/ShinyApp_char_dict.RData")