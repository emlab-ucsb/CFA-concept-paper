################################################################################
#
# This query produces a table of gridded fishing effort (hours) and their
# distance to LSMPA borders.
# 
# The query is divided into 2 sub-queries:
# - First, we identify a list of MMSIs (ssvid) that:
#             - have had activity
#             - are purse seiners, longliners, or trawlers
# - We then create a gridded version of fishing effort (0.1 degree res)
#
# Once these two subqueries are done, we call the distance to lsmpa table and
# left join by lat / long.
################################################################################
########
# Identify vessels we want
########
WITH
  vessels_i_want AS (
  SELECT
    ssvid,
    best.best_vessel_class,
    best.best_engine_power_kw
  FROM
    `world-fishing-827.gfw_research.vi_ssvid_v20190430`  
  WHERE
    best.best_vessel_class IN ("tuna_purse_seines",
      "drifting_longlines",
      "trawlers")
    AND on_fishing_list_best
    AND CAST(ssvid AS int64) NOT IN (SELECT ssvid FROM `world-fishing-827.gfw_research.bad_mmsi` CROSS JOIN UNNEST(ssvid) AS ssvid)
    AND activity.overlap_hours_multinames = 0
    AND activity.overlap_hours < 24*3
    AND activity.active_hours > 24
    AND activity.offsetting IS FALSE),
  #
  #
  #
  #
  ########
  # Get gridded effort by year and gear
  ########
  gridded_effort_by_gear_and_year AS (
  SELECT
    EXTRACT(year
    FROM
      timestamp) AS year,
    best_vessel_class,
    CAST(FLOOR(lat / 0.1) * 0.1 AS NUMERIC) + 0.05 AS lat,
    CAST(FLOOR(lon / 0.1) * 0.1 AS NUMERIC) + 0.05 AS lon,
    SUM(hours) AS fishing_hours
  FROM
    `world-fishing-827.gfw_research.pipe_v20190502_fishing`  
  LEFT JOIN
    vessels_i_want
  USING
    (ssvid)
  WHERE
    ssvid IN (
    SELECT
      ssvid
    FROM
      `vessels_i_want`)
    AND nnet_score > 0.5
    AND distance_from_shore_m > 1000
    AND timestamp >= TIMESTAMP("2016-01-01")
    AND timestamp <= TIMESTAMP("2019-12-31")
  GROUP BY
    year,
    lat,
    lon,
    best_engine_power_kw,
    best_vessel_class)
    #
    #
    #
    #
    ########
    # Left join LSMPA distance and identity
    ########
SELECT
  year,
  best_vessel_class,
  lat,
  lon,
  distance,
  wdpaid,
  iucn_cat,
  fishing_hours
FROM
  gridded_effort_by_gear_and_year
LEFT JOIN `emlab-gcp.ocean_halos_v2.dist_to_lsmpa` USING(lat, lon)