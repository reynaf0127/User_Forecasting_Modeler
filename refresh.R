observeEvent(input$refresh, {
source("/opt/ki_r/ki_db.R")
vertica <- ki_initVertica("vertica")

data_current<-dbGetQuery(vertica,"
  with revenue as(
select *
from(
   SELECT
      DATE(
         (CASE WHEN ASF.revenue_source_id IN (2,17,40,42,44,56,59) 
          THEN ASF.purchase_timestamp
          ELSE (ASF.purchase_timestamp + INTERVAL '1 HOUR') 
          END))                                             AS trans_date,
      SUM(rev_total)                                        AS rev_total,
      SUM(CASE WHEN (revenue_source_id NOT IN (2, 56) AND ASF.item NOT IN ('Crowns', 'Crowns Blended', 'gift_coupon', 'In-game Item')) THEN rev_total ELSE 0 END) AS rev_sub_ppt,
      SUM(CASE WHEN (revenue_source_id NOT IN (2, 56) AND ASF.item IN ('Crowns', 'Crowns Blended', 'gift_coupon')) THEN rev_total ELSE 0 END)                     AS rev_crown,
      SUM(CASE WHEN (revenue_source_id NOT IN (2, 56) AND ASF.item IN ('In-game Item')) THEN rev_total ELSE 0 END)                                                AS rev_igi,
      SUM(CASE WHEN (revenue_source_id NOT IN (2, 56)) THEN 0 ELSE ASF.rev_total END)                                                                             AS rev_ppc
   FROM
      Core_DB.SAS_Sales_PFACT AS ASF
   LEFT JOIN Core_DB.CS_ItemCategory_DIM AS CID ON
      CID.item_id = (CASE WHEN (ASF.sales_ki_sku IS NULL) THEN ASF.ki_sku ELSE ASF.sales_ki_sku END)
   WHERE
      ASF.payment_status IN ('Paid','Refunded','Unpaid') AND
      (ASF.game_id=1 OR (ASF.game_id = 0 AND ASF.site_id = 1)) AND
      is_combo_card_sale=0 and
      revenue_source_id not in (44,59)
   GROUP BY 1
) as a
where year(trans_date)>=2021 and year(trans_date)<=2021
order by 1
),

ga AS(
SELECT play_date, account_id, 
       case when LAG(play_date) OVER (PARTITION BY account_id ORDER BY play_date) is not null then LAG(play_date) OVER (PARTITION BY account_id ORDER BY play_date)
            else min(play_date) over (partition by account_id) end AS previous_login,
       case when LEAD(play_date) OVER (PARTITION BY account_id ORDER BY play_date) is not null then LEAD(play_date) OVER (PARTITION BY account_id ORDER BY play_date)
            else date(getdate()) end AS next_login
FROM WZ_Core_DB.MQT_GCS_GameActivityDaily AS ga
where agg_play_sessions>0
GROUP BY 1,2
),

attribution as (
select a.account_id,
       date(d.account_creation_date) as account_creation_date
from Core_DB.CS_AccountCore_DIM as a --Grab country and campaign info
join Core_DB.MKT_Campaign_DIM as b on a.url_campaign_id=b.mkt_campaign_id --Grab campaign definition
join Core_DB.MKT_CampaignGroup_DIM as c on b.mkt_group_id=c.mkt_group_id --Grab campaign group definition
join WZ_Core_DB.GCS_AccountGame_DIM as d on a.account_id=d.account_id --Grab wizard account_id and account_creation_date
where d.account_creation_date is not null and a.account_id != 0 and a.ki_employee_flag=0 and b.game_id=1 and d.account_id!=0
      and a.account_id not in (select account_id from eramirez.V_W101SyntheticAccounts)
      and group_name !='Steam'
group by 1,2
),

user_type as (
select play_date,dau_category,total_players,retention_rate
from(
select play_date,dau_category,if_retain,number_players,
       sum(number_players) over(partition by play_date,dau_category) as total_players,
       number_players/(sum(number_players) over(partition by play_date,dau_category)) as retention_rate
from(
select play_date,dau_category,if_retain,count(distinct account_id) as number_players
from(
SELECT play_date, next_login, previous_login, 
        a.*,
        case when date(play_date)-date(account_creation_date)=0 then 'New players'
             when date(play_date)-date(account_creation_date)!=0 and date(play_date)-date(previous_login)<=7 then 'Current players'
             else 'Returning players' end as dau_category,
        case when date(next_login)-date(play_date)<=7 then 'retain players' else 'churn players' end as if_retain 
FROM ga
JOIN attribution AS a ON a.account_id = ga.account_id
WHERE ga.play_date >= '2021-01-01' and ga.play_date <= '2021-12-31'
) as a
group by 1,2,3
) as b
) as c
where if_retain='retain players'
group by 1,2,3,4
),

year_date as(
select distinct play_date
from WZ_Core_DB.MQT_GCS_GameActivityDaily
where year(play_date)=2021 and play_date<=getdate()-1
order by play_date
)

select a.play_date as date, 
       u1.total_players as current,
       u2.total_players as new,
       u3.total_players as returning,

       u1.retention_rate as current_r,
       u2.retention_rate as new_r,
       u3.retention_rate as returning_r,
       
              r1.rev_crown as rev_crowns,
       r1.rev_crown/(u1.total_players+u2.total_players+u3.total_players) as arpu_crowns,
              r1.rev_igi as rev_ingame,
       r1.rev_igi/(u1.total_players+u2.total_players+u3.total_players) as arpu_ingame,
              r1.rev_ppc as rev_ppdcard,
       r1.rev_ppc/(u1.total_players+u2.total_players+u3.total_players) as arpu_ppdcard,
              0.0 as rev_monthly,0.0 as arpu_monthly,
              0.0 as rev_prepaid,0.0 as arpu_prepaid,
              0.0 as rev_semi,0.0 as arpu_semi,
              0.0 as rev_annual,0.0 as arpu_annual,

              r1.rev_sub_ppt as rev_sub_ppt,
       r1.rev_sub_ppt/(u1.total_players+u2.total_players+u3.total_players) as arpu_sub_ppt,
              r1.rev_total as rev_total,
       r1.rev_total/(u1.total_players+u2.total_players+u3.total_players) as arpu_total,
       u1.total_players+u2.total_players+u3.total_players as dau
       
from year_date as a
left join revenue as r1 on a.play_date=r1.trans_date

left join user_type as u1 on a.play_date=u1.play_date and u1.dau_category='Current players'
left join user_type as u2 on a.play_date=u2.play_date and u2.dau_category='New players'
left join user_type as u3 on a.play_date=u3.play_date and u3.dau_category='Returning players'

group by 1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26
order by 1

")

data_current$date<-as.Date(data_current$date)
data_preview<-data %>% filter(date<min(data_current$date)|date>max(data_current$date))
data_preview<-rbind(data_preview,data_current) %>% arrange(date)
output$preview<-renderTable({tail(data_preview %>% filter (!is.na(current)),n=20)})
data<-data_preview
})
