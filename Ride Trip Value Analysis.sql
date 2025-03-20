WITH oms AS (
  SELECT Airport_Tag,
  Business_Type,
  order_booking_at_MYT AS Booking_Date,
  order_id,
  order_status,
  order_item_booking_status,
  Order_MYR_GMV,
  unique_user,
  unique_booking,
  order_item_pickup_longitude AS longitude,
  order_item_pickup_latitude AS latitude,
  CAST(order_item_ride_distance AS INT64)/1000 AS order_item_ride_distance
  FROM `xxx-oms-prd.oms_master.oms_MasterTransactionData_Ride`
  WHERE Country = 'Malaysia'
    AND order_item_business_type = 'e-hailing'
    AND order_booking_at_MYT BETWEEN '2024-01-01' AND CURRENT_DATE()
),

final as (
  SELECT oms.*, name AS ZoneName,
  CASE WHEN order_item_ride_distance BETWEEN 0 AND 5 THEN 'a.0-5km'
    WHEN order_item_ride_distance BETWEEN 6 AND 10 THEN 'b.6-10km'
    WHEN order_item_ride_distance BETWEEN 11 AND 15 THEN 'c.11-15km'
    WHEN order_item_ride_distance BETWEEN 16 AND 20 THEN 'd.16-20km'
    WHEN order_item_ride_distance BETWEEN 21 AND 25 THEN 'e.21-25km'
    WHEN order_item_ride_distance BETWEEN 26 AND 30 THEN 'f.26-30km'
    WHEN order_item_ride_distance BETWEEN 31 AND 35 THEN 'g.31-35km'
    WHEN order_item_ride_distance BETWEEN 36 AND 40 THEN 'h.36-40km'
    WHEN order_item_ride_distance BETWEEN 41 AND 45 THEN 'i.41-45km'
    WHEN order_item_ride_distance BETWEEN 46 AND 50 THEN 'j.46-50km'
    WHEN order_item_ride_distance >50 THEN 'k. > 50km'
    ELSE NULL END AS Distance_Range,
  CASE WHEN order_myr_gmv BETWEEN 0 AND 5 THEN 'a.MYR 0-5'
	  WHEN order_myr_gmv BETWEEN 6 AND 10 THEN 'b.MYR 6-10'
    WHEN order_myr_gmv BETWEEN 11 AND 15 THEN 'c.MYR 11-15'
    WHEN order_myr_gmv BETWEEN 16 AND 20 THEN 'd.MYR 16-20'
    WHEN order_myr_gmv BETWEEN 21 AND 25 THEN 'e.MYR 21-25'
    WHEN order_myr_gmv BETWEEN 26 AND 30 THEN 'f.MYR 26-30'
    WHEN order_myr_gmv BETWEEN 31 AND 35 THEN 'g.MYR 31-35'
    WHEN order_myr_gmv BETWEEN 36 AND 40 THEN 'h.MYR 36-40'
    WHEN order_myr_gmv BETWEEN 41 AND 45 THEN 'i.MYR 41-45'
    WHEN order_myr_gmv BETWEEN 46 AND 50 THEN 'j.MYR 46-50'
    WHEN order_myr_gmv > 50 THEN 'k. > MYR 50'
    ELSE NULL END AS GMV_Range
  FROM oms
  INNER JOIN `xxx-ecomdata-dev.GEOSPATIAL_COMMON.my_autosurge_zones`
  ON ST_CONTAINS(geometry, ST_GEOGPOINT(longitude, latitude))
  WHERE zonegroup = 'Hotspot'
)

SELECT *
FROM final
ORDER BY Booking_Date
