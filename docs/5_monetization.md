# Метрики монетизации pt.2 {#c5_monetization}

## Запись занятия {-}

<iframe width="560" height="315" src="https://www.youtube.com/embed/2DjR09pvky0?si=w4Cj2J9gj436byIz" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>


## Разбор домашнего задания

### level 2 (HNTR)

Постройте график накопительной конверсии с разбивкой по источнику пользователей.



```r
library(data.table)
library(plotly)

# импортируем данные
installs <- fread('https://gitlab.com/hse_mar/mar211f/-/raw/main/data/installs.csv') 
payments <- fread('https://gitlab.com/hse_mar/mar211f/-/raw/main/data/payments_custom.csv')

# перекодируем медиасорсы
installs[is.na(media_source), media_source := 'organic']
installs[media_source == 'other', media_source := 'organic']

# выбираем первый инсталл
installs <- installs[order(user_pseudo_id, dt)]
installs <- installs[, .SD[1], by = user_pseudo_id]

# берем платежи только новых пользователей
payments_new <- payments[user_pseudo_id %in% installs[, unique(user_pseudo_id)]]

# прикрепляем к ним медиасорсы
payments_new <- merge(
  payments_new,
  installs[, list(user_pseudo_id, dt, media_source)],
  by = 'user_pseudo_id', all.x = TRUE
)

# payments_new <- merge(
#   payments_new,
#   installs[, list(user_pseudo_id, dt, media_source)],
#   by = 'user_pseudo_id', all = FALSE
# )

# вычисляем лайфтайм
payments_new[, lifetime := pay_dt - dt]

# корректируем на окно лайфтайма
# as.Date('2022-07-31') - as.Date('2022-06-30')
payments_new <- payments_new[dt < '2022-07-01']
payments_new <- payments_new[lifetime <= 30]
payments_new <- payments_new[lifetime >= 0]


# считаем, когда пользователь сделал первый платеж (минимальный лайфтайм)
payments_new_stat <- payments_new[, list(lifetime = min(lifetime)), 
                                  by = list(user_pseudo_id, media_source)]

# считаем распределение пользователей по этому мин.лайфтайму
payments_new_stat <- payments_new_stat[, list(new_payer = uniqueN(user_pseudo_id)), 
                                       by = list(media_source, lifetime)]

# сортируем и делаем кумулятивную сумму
payments_new_stat <- payments_new_stat[order(media_source, lifetime)]
payments_new_stat[, new_payer_cum := cumsum(new_payer), by = media_source]

# считаем, сколько всего пользователей пришло
installs_stat <- installs[, list(total_users = uniqueN(user_pseudo_id)), by = media_source]

payments_new_stat <- merge(
  payments_new_stat,
  installs_stat,
  by = 'media_source', all.x = TRUE
)
payments_new_stat[, conversion := new_payer_cum / total_users]

# рисуем
plot_ly(payments_new_stat, x = ~lifetime, y = ~conversion, color = ~media_source,
        type = 'scatter', mode = 'lines') %>%
  layout(
    title = 'накопительная конверсия по источникам трафика',
    yaxis = list(rangemode = 'tozero')
  ) %>%
  config(displayModeBar = FALSE)  
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-67e3ecfa2ab8a225badf" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-67e3ecfa2ab8a225badf">{"x":{"visdat":{"3ff872c980b66":["function () ","plotlyVisDat"]},"cur_data":"3ff872c980b66","attrs":{"3ff872c980b66":{"x":{},"y":{},"mode":"lines","color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"накопительная конверсия по источникам трафика","yaxis":{"domain":[0,1],"automargin":true,"rangemode":"tozero","title":"conversion"},"xaxis":{"domain":[0,1],"automargin":true,"title":"lifetime"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[0,1,2,3,5,7,8,9,11,13,17,27,29],"y":[0.013878180416345412,0.022359290670778721,0.024672320740169621,0.030069390902081727,0.030840400925212029,0.031611410948342328,0.033924441017733231,0.035466461063993829,0.037008481110254433,0.038550501156515038,0.039321511179645337,0.040092521202775636,0.040863531225905934],"mode":"lines","type":"scatter","name":"Facebook Ads","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"line":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.0095605410397088374,0.01510130914226737,0.018143299473083817,0.020017382801890381,0.021266771687761421,0.022787766853169647,0.023792710087457224,0.024363083274485307,0.025069259601281983,0.025721114672171221,0.02637296974306046,0.026780379162366235,0.027133467325764573,0.027377912977348037,0.027785322396653812,0.027921125536422401,0.028192731815959583,0.028328534955728176,0.028545819979357921,0.028871747514802541,0.028980390026617415,0.029170514422293445,0.029333478190015753,0.029496441957738064,0.029659405725460372,0.029903851377043836,0.029958172632951273,0.030093975772719866,0.030229778912488455,0.030256939540442174,0.030419903308164485],"mode":"lines","type":"scatter","name":"applovin_int","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"line":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,20,21,24,25,26,30],"y":[0.0075894005659891944,0.011062516079238487,0.012863390789812194,0.014149729868793414,0.014664265500385902,0.015307435039876511,0.015693336763570879,0.016079238487265245,0.016722408026755852,0.017494211474144584,0.018008747105737074,0.018266014921533315,0.01839464882943144,0.018651916645227681,0.018909184461023926,0.019037818368922047,0.019295086184718292,0.019423720092616413,0.019552354000514537,0.019680987908412658,0.019809621816310779,0.020066889632107024,0.020581425263699511,0.020710059171597635],"mode":"lines","type":"scatter","name":"googleadwords_int","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"line":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.0051655399549315308,0.0085630091870341485,0.010105737562835847,0.011197781244583116,0.012185820766163979,0.012619171433524007,0.012965851967412029,0.013624544981799273,0.013884555382215289,0.014196567862714509,0.014491246316519327,0.014751256716935343,0.014907262957184954,0.015149939330906569,0.015305945571156179,0.015496619864794592,0.015704628185127405,0.015860634425377017,0.016016640665626625,0.016120644825793031,0.016276651066042643,0.016415323279597851,0.016553995493153058,0.016675333680013869,0.016814005893569076,0.016900676027041081,0.016970012133818685,0.017056682267290693,0.017143352400762698,0.017282024614317906,0.017334026694401108],"mode":"lines","type":"scatter","name":"organic","marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"line":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,19,20,21,22,23,24,25,26,27,28],"y":[0.0036335558886315118,0.0060407866648498889,0.0070854339828314485,0.0078121451605577506,0.008130081300813009,0.0089930508243629918,0.0093109869646182501,0.0094926647590498243,0.0096743425534814003,0.0099468592451287634,0.010173956488168233,0.010264795385384022,0.010446473179815598,0.010582731525639278,0.010673570422855067,0.010764409320070854,0.010900667665894537,0.010991506563110324,0.011127764908934006,0.011264023254757687,0.01130944270336558,0.011445701049189263,0.01153653994640505,0.011581959395012945,0.011718217740836626,0.011763637189444519,0.011854476086660308,0.011899895535268202],"mode":"lines","type":"scatter","name":"unityads_int","marker":{"color":"rgba(166,216,84,1)","line":{"color":"rgba(166,216,84,1)"}},"textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"line":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

### level 3 (HMP)

Посчитайте по каждой платформе конверсию в платящих в день инсталла. Когорта -- пришедшие в июне.

Делать аналогично динамике ретеншена первого дня.

```r
# аналогично предыдущему заданию
# пересобираем датасет, так как в прошлом задании мы в нем делали другие вычисления
# и он нужен заново
payments_new <- payments[user_pseudo_id %in% installs[, unique(user_pseudo_id)]]

payments_new <- merge(
  payments_new,
  installs[, list(user_pseudo_id, dt, media_source)],
  by = 'user_pseudo_id', all.x = TRUE
)
payments_new[, lifetime := pay_dt - dt]

payments_new <- payments_new[dt < '2022-07-25']
payments_new <- payments_new[lifetime <= 7]
payments_new <- payments_new[lifetime >= 0]

# отличие от предыдущего задания -- делаем группировку не по медиасорсам, а по дате инсталла
payments_new_stat <- payments_new[, list(lifetime = min(lifetime)), 
                                  by = list(user_pseudo_id, dt)]
payments_new_stat <- payments_new_stat[, list(new_payer = uniqueN(user_pseudo_id)), 
                                       by = list(dt, lifetime)]

installs_stat <- installs[, list(total_users = uniqueN(user_pseudo_id)), by = dt]

payments_new_stat <- merge(
  payments_new_stat,
  installs_stat,
  by = 'dt', all.x = TRUE
)

payments_new_stat <- payments_new_stat[order(dt, lifetime)]
payments_new_stat[, new_payer_cum := cumsum(new_payer), by = dt]
payments_new_stat[, conversion := new_payer_cum / total_users]

# оставляем только накопительную конверсию в платящих на определенные дни от инсталла
payments_new_stat <- payments_new_stat[lifetime %in% c(0, 1, 3, 7)]

plot_ly(payments_new_stat[dt < '2022-07-01'], 
        x = ~dt, y = ~conversion, color = ~as.character(lifetime),
        type = 'scatter', mode = 'lines') %>%
  layout(
    title = 'Динамика конверсии пользователей по дням',
    yaxis = list(rangemode = 'tozero')
  ) %>%
  config(displayModeBar = FALSE)  
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-47ec82e3ec849626212c" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-47ec82e3ec849626212c">{"x":{"visdat":{"3ff8753696b8a":["function () ","plotlyVisDat"]},"cur_data":"3ff8753696b8a","attrs":{"3ff8753696b8a":{"x":{},"y":{},"mode":"lines","color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Динамика конверсии пользователей по дням","yaxis":{"domain":[0,1],"automargin":true,"rangemode":"tozero","title":"conversion"},"xaxis":{"domain":[0,1],"automargin":true,"title":"dt"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":["2022-06-01","2022-06-02","2022-06-03","2022-06-04","2022-06-05","2022-06-06","2022-06-07","2022-06-08","2022-06-09","2022-06-10","2022-06-11","2022-06-12","2022-06-13","2022-06-14","2022-06-15","2022-06-16","2022-06-17","2022-06-18","2022-06-19","2022-06-20","2022-06-21","2022-06-22","2022-06-23","2022-06-24","2022-06-25","2022-06-26","2022-06-27","2022-06-28","2022-06-29","2022-06-30"],"y":[0.0070792418360356244,0.0093908629441624373,0.0075036075036075036,0.0075051171253127128,0.008271131732902965,0.010029633006610439,0.01038691249026227,0.0098353156450137237,0.0077948202162434,0.0069949636261891438,0.0066193853427895981,0.0089813800657174148,0.0082066135650494809,0.008634646519158122,0.0071960935492161402,0.0084459459459459464,0.0045871559633027525,0.0054024851431658562,0.0062310273206582518,0.0070169433510183128,0.0039318479685452159,0.0079392474974111157,0.010218463706835801,0.0040966816878328554,0.0075829383886255926,0.003552397868561279,0.0038204393505253103,0.0033090668431502318,0.002976190476190476,0.0096952908587257611],"mode":"lines","type":"scatter","name":"0","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"line":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-06-01","2022-06-02","2022-06-03","2022-06-04","2022-06-05","2022-06-06","2022-06-07","2022-06-08","2022-06-09","2022-06-10","2022-06-11","2022-06-12","2022-06-13","2022-06-14","2022-06-15","2022-06-16","2022-06-17","2022-06-18","2022-06-19","2022-06-20","2022-06-21","2022-06-22","2022-06-23","2022-06-24","2022-06-25","2022-06-26","2022-06-27","2022-06-28","2022-06-29","2022-06-30"],"y":[0.012788307832838547,0.013197969543147208,0.012987012987012988,0.013873095292244713,0.014121444422029453,0.015500341919307043,0.014541677486367177,0.01555352241537054,0.012572290671360321,0.012870733072188025,0.0094562647754137114,0.014238773274917854,0.012792662321988897,0.014301133297355639,0.012593163711128244,0.015202702702702704,0.0077981651376146793,0.0075634792004321992,0.0099057357405336315,0.010610987506417936,0.0058977719528178242,0.013117017604418364,0.014094432699083862,0.0094223678820155674,0.0099526066350710905,0.0066607460035523975,0.0062082139446036294,0.0052945069490403706,0.0066964285714285711,0.012465373961218837],"mode":"lines","type":"scatter","name":"1","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"line":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-06-01","2022-06-02","2022-06-03","2022-06-04","2022-06-05","2022-06-06","2022-06-07","2022-06-08","2022-06-09","2022-06-10","2022-06-11","2022-06-12","2022-06-13","2022-06-14","2022-06-15","2022-06-16","2022-06-17","2022-06-18","2022-06-19","2022-06-20","2022-06-21","2022-06-22","2022-06-23","2022-06-24","2022-06-25","2022-06-26","2022-06-27","2022-06-28"],"y":[0.01644211007079242,0.017766497461928935,0.01875901875901876,0.020468501250852856,0.016743998386120638,0.021198997036699339,0.019475460919241756,0.020814272644098811,0.015841086245914005,0.018186905428091774,0.015130023640661938,0.017524644030668127,0.017137340091720975,0.018618456556934702,0.016448213826779749,0.018339768339768341,0.009862385321100918,0.0093643075814874837,0.012302284710017574,0.013520451822693822,0.0085190039318479693,0.01794960303762513,0.015856236786469344,0.012699713232281851,0.011374407582938388,0.010657193605683837,0.0085959885386819486,0.0066181336863004635],"mode":"lines","type":"scatter","name":"3","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"line":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":["2022-06-01","2022-06-02","2022-06-03","2022-06-04","2022-06-05","2022-06-06","2022-06-07","2022-06-08","2022-06-09","2022-06-10","2022-06-11","2022-06-12","2022-06-13","2022-06-14","2022-06-15","2022-06-16","2022-06-17","2022-06-18","2022-06-19","2022-06-20","2022-06-21","2022-06-22","2022-06-23","2022-06-24","2022-06-25","2022-06-26","2022-06-29","2022-06-30"],"y":[0.01918246174925782,0.021065989847715735,0.021356421356421358,0.025699340459404141,0.022190841234617713,0.024618190107134716,0.023110880290833549,0.02584629460201281,0.02036711088760372,0.020145495243424735,0.017730496453900711,0.021905805038335158,0.020033791938209027,0.02239611440906638,0.019275250578257519,0.022924710424710424,0.012155963302752294,0.010985053124437242,0.014539063748202588,0.01574533629984597,0.011795543905635648,0.02105626510182948,0.017618040873854827,0.015157722244981565,0.014218009478672985,0.013321492007104795,0.0096726190476190479,0.013850415512465374],"mode":"lines","type":"scatter","name":"7","marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"line":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```



## Воронка платежей

Доля пользователей, которые сделали второй, третий и т.д. платеж.



```r
# опять пересобираем исходную табличку платежей
payments_new <- payments[user_pseudo_id %in% installs[, unique(user_pseudo_id)]]

payments_new <- merge(
  payments_new,
  installs[, list(user_pseudo_id, dt, media_source)],
  by = 'user_pseudo_id', all.x = TRUE
)

# отсортировать платежи пользователей по возрастанию 
payments_new <- payments_new[order(user_pseudo_id, ts)]

# альтернативные методы сортировки
# setkey(payments_new, user_pseudo_id, ts)
# setorder(payments_new, user_pseudo_id, ts)

# создаем номер платежа каждого пользователя. 1:.N -- и так в группе по пользователям
payments_new[, purchase_number := 1:.N, by = user_pseudo_id]
# payments_new[, purchase_number := seq(1, .N, by = 1), by = user_pseudo_id]

payments_new[, lifetime := pay_dt - dt]
payments_new <- payments_new[dt < '2022-07-01']
payments_new <- payments_new[lifetime <= 30]
payments_new <- payments_new[lifetime >= 0]

# считаем, сколько пользователей сделало платеж с этим номером
payments_funnel <- payments_new[, list(n_users = uniqueN(user_pseudo_id)), keyby = purchase_number]

# считаем посчитать долю от всего пользователей, сделавших платеж (purchase_number == 1)
payments_funnel[, total_payers := n_users[purchase_number == 1]]

# если у нас есть группировка, то надо отдельно считать и мерджить по ключу

# рисуем
payments_funnel[, share := n_users / total_payers]

plot_ly(payments_funnel[purchase_number <= 10], 
        x = ~purchase_number, y = ~share, type = 'bar') %>%
  layout(
    title = 'Воронка платежей'
  ) %>%
  config(displayModeBar = FALSE)  
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-f79c0e643003e8157407" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-f79c0e643003e8157407">{"x":{"visdat":{"3ff876dde19db":["function () ","plotlyVisDat"]},"cur_data":"3ff876dde19db","attrs":{"3ff876dde19db":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей","xaxis":{"domain":[0,1],"automargin":true,"title":"purchase_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"share"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[1,2,3,4,5,6,7,8,9,10],"y":[1,0.53423597678916823,0.36092843326885882,0.26847195357833653,0.20851063829787234,0.17485493230174082,0.14313346228239845,0.12340425531914893,0.10522243713733076,0.095164410058027074],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```


Воронки можно считать не от первого шага, а от предыдущего. В некоторых случаях это удобнее и информативнее.

```r
# если хотим считать от предыдущего шага
payments_funnel[, prev_users := shift(n_users, n = 1)]
payments_funnel[, prev_share := n_users / prev_users]

plot_ly(payments_funnel[purchase_number <= 10], 
        x = ~purchase_number, y = ~prev_share, type = 'bar') %>%
  layout(
    title = 'Воронка платежей, доля от предыдущего'
  ) %>%
  config(displayModeBar = FALSE)  
```

```
## Warning: Ignoring 1 observations
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-01587c693b730a04786b" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-01587c693b730a04786b">{"x":{"visdat":{"3ff875cc6ef9a":["function () ","plotlyVisDat"]},"cur_data":"3ff875cc6ef9a","attrs":{"3ff875cc6ef9a":{"x":{},"y":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей, доля от предыдущего","xaxis":{"domain":[0,1],"automargin":true,"title":"purchase_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"prev_share"},"hovermode":"closest","showlegend":false},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[2,3,4,5,6,7,8,9,10],"y":[0.53423597678916823,0.67559739319333811,0.7438370846730975,0.77665706051873196,0.83858998144712427,0.81858407079646023,0.86216216216216213,0.85266457680250785,0.90441176470588236],"type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

```r
plot_ly(payments_funnel[purchase_number <= 10], 
        x = ~purchase_number, y = ~share, type = 'bar', name = '% from payers') %>%
  add_trace(y = ~prev_share, name = '% from prev') %>%
  layout(
    title = 'Воронка платежей'
  ) %>%
  config(displayModeBar = FALSE)  
```

```
## Warning: Ignoring 1 observations
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-b10506cf26caac00f683" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-b10506cf26caac00f683">{"x":{"visdat":{"3ff874e2f72a4":["function () ","plotlyVisDat"]},"cur_data":"3ff874e2f72a4","attrs":{"3ff874e2f72a4":{"x":{},"y":{},"name":"% from payers","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar"},"3ff874e2f72a4.1":{"x":{},"y":{},"name":"% from prev","alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"bar","inherit":true}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"Воронка платежей","xaxis":{"domain":[0,1],"automargin":true,"title":"purchase_number"},"yaxis":{"domain":[0,1],"automargin":true,"title":"share"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[1,2,3,4,5,6,7,8,9,10],"y":[1,0.53423597678916823,0.36092843326885882,0.26847195357833653,0.20851063829787234,0.17485493230174082,0.14313346228239845,0.12340425531914893,0.10522243713733076,0.095164410058027074],"name":"% from payers","type":"bar","marker":{"color":"rgba(31,119,180,1)","line":{"color":"rgba(31,119,180,1)"}},"error_y":{"color":"rgba(31,119,180,1)"},"error_x":{"color":"rgba(31,119,180,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[2,3,4,5,6,7,8,9,10],"y":[0.53423597678916823,0.67559739319333811,0.7438370846730975,0.77665706051873196,0.83858998144712427,0.81858407079646023,0.86216216216216213,0.85266457680250785,0.90441176470588236],"name":"% from prev","type":"bar","marker":{"color":"rgba(255,127,14,1)","line":{"color":"rgba(255,127,14,1)"}},"error_y":{"color":"rgba(255,127,14,1)"},"error_x":{"color":"rgba(255,127,14,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```

