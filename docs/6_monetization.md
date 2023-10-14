# Метрики монетизации pt.3 {-}

## Запись занятия {-}

<iframe width="560" height="315" src="https://www.youtube.com/embed/_AeWA3nmHrI?si=kaA90fsn1qbHyFxN" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share" allowfullscreen></iframe>


## ARPU / ARPPU {-}
Averange revenue per user - сумма платежей за определенный период, деленная на общее количество пользователей когорты. Средний чек, наверное, одна из самых важных метрик для оперирования продуктом, так как изучение структуры ARPU позволяет понять, за что платят пользователи и как можно улучшить эту метрику и так далее.

Average revenue per paying user - сумма платежей за определенный период, деленная на количество платящих пользователей когорты.

Обе метрики считаются в определенном окне (количестве дней от инсталла) - обычно 7, 28 или 30 дней. Это необходимо для того, чтобы учесть ситуацию, когда пользователи одной когорты (месячной, например) могли прожить разное количество дней в приложении. Или когда необходимо сравнить разные каналы привлечения, рекламные кампании или группы аб-тестов.

### задача 1 {-}
Считаем статистики в окне 7 дней от инсталла:


```r
library(data.table)
library(plotly)
library(kableExtra)

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

# вычисляем лайфтайм
payments_new[, lifetime := pay_dt - dt]

# корректируем на окно лайфтайма
payments_new <- payments_new[dt < '2022-07-01']
payments_new <- payments_new[lifetime < 7]

# отсюда надо считать количество денег и платящих
# по сути, просто группировка по медиасорсу

payments_new_stat <- payments_new[, list(
  payers_7 = uniqueN(user_pseudo_id),
  gross_7 = sum(gross),
  n_purchases = .N
), by = media_source]


# считаем, сколько всего пользователей пришло
installs_stat <- installs[, list(total_users = uniqueN(user_pseudo_id)), 
                          by = media_source]

payments_new_stat <- merge(
  installs_stat,
  payments_new_stat,
  by = 'media_source', all.x = TRUE
)

payments_new_stat[, ARPU_7 := round(gross_7 / total_users, 2)]
payments_new_stat[, ARPPU_7 := round(gross_7 / payers_7, 2)]
payments_new_stat[, conv_7 := round(payers_7 / total_users, 3)]
payments_new_stat[, avg_purchase := round(gross_7 / n_purchases, 1)]
payments_new_stat[, purchases_per_user := round(n_purchases / payers_7, 1)]

payments_new_stat
```

```
##         media_source total_users payers_7  gross_7 n_purchases ARPU_7 ARPPU_7
## 1:      Facebook Ads        1297       40   612.47          64   0.47   15.31
## 2:      applovin_int       36818      878 17352.16        2161   0.47   19.76
## 3: googleadwords_int        7774      122  2092.53         247   0.27   17.15
## 4:           organic       57690      753 16699.53        1968   0.29   22.18
## 5:      unityads_int       22017      206  2990.22         477   0.14   14.52
##    conv_7 avg_purchase purchases_per_user
## 1:  0.031          9.6                1.6
## 2:  0.024          8.0                2.5
## 3:  0.016          8.5                2.0
## 4:  0.013          8.5                2.6
## 5:  0.009          6.3                2.3
```

```r
# kable_classic(kbl(payments_new_stat))
# kable_material(kbl(payments_new_stat))
```

<br>

## LTV {-}
Lifetime value - общая сумма платежей, которые сделает пользователь за всю свою жизнь в приложении. Так как для каждого пользователя обычно сложно предсказать, сколько он проживет, то считается как кумулятивный средний чек когорты - общая накопленная сумма платежей, сделанная к определенному дню от инсталла, деленная на количество пользователей когорты. Кривая LTV/cumARPU сходится к общему значению ARPU по всей выборке за все время жизни когорты. 

LTV, фактически, одна из ключевых метрик, так как график LTV/cumARPU позволяет оценить динамику платежей когорты, сделать какие-то предсказания. Отношение LTV и CPI позволяют оценить эффективность рекламных кампаний. Например, за 90 дней от инсталла платежами пользователей возвращается 40-60% затраченных на привлечение денег. На 270-360 - все затраченные, и дальнейшие платежи составляют чистую прибыль (абстрактный пример, в реальности периоды сильно зависят от продукта). Соответственно, если даже в перспективе значение LTV когорты (сколько заплатит каждый привлеченный пользователь) не превысит CPI (сколько стоило привлечение каждого пользователя) когорты, то такая рекламная кампания убыточна и может быть полезна только для увеличение объема пользователей.

### задача 2 {-}
Посчитать кумулятивное ARPU  30  дня по июньской когорте

Алгоритм
 - посчитать сумму гросса по дням лайфтайма
 - посчитать кумулятивную сумму этого гросса c помощью `cumsum()`
 - поделить кумулятивную сумму на общее количество пользователей когорты
 - нарисовать график
 

```r
# пересоздаем датасет, так как раньше отрезали только 7 дней
# берем платежи только новых пользователей
payments_new <- payments[user_pseudo_id %in% installs[, unique(user_pseudo_id)]]

# прикрепляем к ним медиасорсы
payments_new <- merge(
  payments_new,
  installs[, list(user_pseudo_id, dt, media_source)],
  by = 'user_pseudo_id', all.x = TRUE
)

# вычисляем лайфтайм
payments_new[, lifetime := pay_dt - dt]

# корректируем на окно лайфтайма
payments_new <- payments_new[dt < '2022-07-01']
payments_new <- payments_new[lifetime <= 30]
payments_new <- payments_new[lifetime >= 0]


# считаем платежи
payments_new_stat <- payments_new[, list(gross = sum(gross)), 
                                  by = list(media_source, lifetime)]

# сортируем и делаем кумулятивную сумму
payments_new_stat <- payments_new_stat[order(media_source, lifetime)]
payments_new_stat[, gross_cum := cumsum(gross), by = media_source]

# считаем, сколько всего пользователей пришло
installs_stat <- installs[, list(total_users = uniqueN(user_pseudo_id)), by = media_source]

payments_new_stat <- merge(
  payments_new_stat,
  installs_stat,
  by = 'media_source', all.x = TRUE
)
payments_new_stat[, cumARPU := gross_cum / total_users]

# рисуем
plot_ly(payments_new_stat, x = ~lifetime, y = ~cumARPU, color = ~media_source,
        type = 'scatter', mode = 'lines') %>%
  layout(
    title = 'cARPU по источникам трафика',
    yaxis = list(rangemode = 'tozero')
  ) %>%
  config(displayModeBar = FALSE)  
```

```{=html}
<div class="plotly html-widget html-fill-item-overflow-hidden html-fill-item" id="htmlwidget-e406d00953bbd95428d9" style="width:672px;height:480px;"></div>
<script type="application/json" data-for="htmlwidget-e406d00953bbd95428d9">{"x":{"visdat":{"3ffc5a18660a":["function () ","plotlyVisDat"]},"cur_data":"3ffc5a18660a","attrs":{"3ffc5a18660a":{"x":{},"y":{},"mode":"lines","color":{},"alpha_stroke":1,"sizes":[10,100],"spans":[1,20],"type":"scatter"}},"layout":{"margin":{"b":40,"l":60,"t":25,"r":10},"title":"cARPU по источникам трафика","yaxis":{"domain":[0,1],"automargin":true,"rangemode":"tozero","title":"cumARPU"},"xaxis":{"domain":[0,1],"automargin":true,"title":"lifetime"},"hovermode":"closest","showlegend":true},"source":"A","config":{"modeBarButtonsToAdd":["hoverclosest","hovercompare"],"showSendToCloud":false,"displayModeBar":false},"data":[{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,23,25,27,28,29,30],"y":[0.16789514263685434,0.28882806476484202,0.32498843484965306,0.34110254433307641,0.35650732459521978,0.41440246723207402,0.47222050886661526,0.47992289899768698,0.48846569005397078,0.50316114109483423,0.52626831148804942,0.52933693138010796,0.54628373168851196,0.5602158828064765,0.58100231303006944,0.58484965304548964,0.5925520431765614,0.60178874325366238,0.60949113338473404,0.61179645335389365,0.61410177332305316,0.63721665381649961,0.68190439475713194,0.70501927525057828],"mode":"lines","type":"scatter","name":"Facebook Ads","marker":{"color":"rgba(252,141,98,1)","line":{"color":"rgba(252,141,98,1)"}},"textfont":{"color":"rgba(252,141,98,1)"},"error_y":{"color":"rgba(252,141,98,1)"},"error_x":{"color":"rgba(252,141,98,1)"},"line":{"color":"rgba(252,141,98,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.12762480308544524,0.21906350154815268,0.28913982291270229,0.33457031886576877,0.37864088217719261,0.43238904883480578,0.46575968276386226,0.49682953989895923,0.52955076321364225,0.56602042479221792,0.59303900266173837,0.62182057689173464,0.64553886685859863,0.6683513498832061,0.69170786028572673,0.72171953935574684,0.74138003150632537,0.76853549894073248,0.79496387636481858,0.81607338801672791,0.83471264055624672,0.85771742082676661,0.88037074257156533,0.89873540116247186,0.91764435873757111,0.93721033190287084,0.96345184420663521,0.98130751262968918,0.99945705904720228,1.0301808897821689,1.0471668748981449],"mode":"lines","type":"scatter","name":"applovin_int","marker":{"color":"rgba(102,194,165,1)","line":{"color":"rgba(102,194,165,1)"}},"textfont":{"color":"rgba(102,194,165,1)"},"error_y":{"color":"rgba(102,194,165,1)"},"error_x":{"color":"rgba(102,194,165,1)"},"line":{"color":"rgba(102,194,165,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.072965011577051792,0.12073449961409839,0.15979933110367908,0.18485593002315423,0.21094803190120931,0.25118214561358387,0.26917031129405727,0.28306663236429142,0.30698096218163123,0.31984178029328547,0.33657962438898908,0.35136223308464126,0.36922433753537448,0.38374581939799346,0.39710445073321343,0.40121044507332149,0.41341394391561631,0.41958708515564719,0.4372047851813739,0.44285824543349639,0.44915230254695149,0.46110110625160805,0.46816568047337287,0.47844481605351186,0.48706457422176497,0.49502958579881667,0.52098662207357871,0.52830975045021877,0.54038847440185256,0.55349884229482915,0.565078466683818],"mode":"lines","type":"scatter","name":"googleadwords_int","marker":{"color":"rgba(141,160,203,1)","line":{"color":"rgba(141,160,203,1)"}},"textfont":{"color":"rgba(141,160,203,1)"},"error_y":{"color":"rgba(141,160,203,1)"},"error_x":{"color":"rgba(141,160,203,1)"},"line":{"color":"rgba(141,160,203,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.072439417576702192,0.13732639972265373,0.18392754376841525,0.21663910556422042,0.24450251343386858,0.26299722655572677,0.28521147512566958,0.312451898075921,0.3375597157219602,0.35589478245796297,0.37631097243889555,0.39045449817992517,0.40818911423123388,0.42361587796845007,0.441885595423815,0.45804333506673406,0.47828982492632843,0.49073721615531096,0.50120956838273345,0.51660703761483595,0.53734078696481002,0.55237268157392772,0.56770116137978666,0.58595406482925794,0.60171346853873975,0.61555070202807927,0.62836470792164834,0.64473097590570105,0.65610799098630423,0.66947547235222571,0.68000208008320151],"mode":"lines","type":"scatter","name":"organic","marker":{"color":"rgba(231,138,195,1)","line":{"color":"rgba(231,138,195,1)"}},"textfont":{"color":"rgba(231,138,195,1)"},"error_y":{"color":"rgba(231,138,195,1)"},"error_x":{"color":"rgba(231,138,195,1)"},"line":{"color":"rgba(231,138,195,1)"},"xaxis":"x","yaxis":"y","frame":null},{"x":[0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30],"y":[0.03833719398646504,0.061050097651814565,0.076745696507244471,0.096247899350501973,0.10577689966843812,0.11957441976654412,0.13027660444202216,0.1362647045464869,0.14112458554753154,0.15351274015533461,0.15786574010991516,0.1640305218694646,0.17106099831948052,0.17559522187400656,0.18439206068038344,0.19024163146659409,0.19627333424172241,0.1997224871690059,0.209251487486942,0.21605986283326531,0.22794249897806251,0.23206749330063142,0.24091156833356053,0.24753236135713325,0.25084253077167656,0.25537857110414691,0.25855021120043614,0.26544488349911444,0.26812099741109158,0.27147885724667314,0.27483580869328256],"mode":"lines","type":"scatter","name":"unityads_int","marker":{"color":"rgba(166,216,84,1)","line":{"color":"rgba(166,216,84,1)"}},"textfont":{"color":"rgba(166,216,84,1)"},"error_y":{"color":"rgba(166,216,84,1)"},"error_x":{"color":"rgba(166,216,84,1)"},"line":{"color":"rgba(166,216,84,1)"},"xaxis":"x","yaxis":"y","frame":null}],"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.20000000000000001,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script>
```
 
 
## Полезные материалы {-}

[What Is a Business Model? 30 Successful Types of Business Models You Need to Know](https://fourweekmba.com/what-is-a-business-model/) Коротко, что такое бизнес-модели, рассматриваются 30 разных моделей. Полезно для понимания, как вообще могут зарабатывать разные продукты.

[Основные метрики мобильных приложений](https://apptractor.ru/measure/user-analytics/osnovnyie-metriki-mobilnyih-prilozheniy.html) Очень обзорный материал от devtodev. Есть неплохой блок по метрикам монетизации.
