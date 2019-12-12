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
    AND activity.active_hours > 0),
  #
  #
  #
  #
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
    `world-fishing-827.gfw_research.pipe_production_v20190502_fishing`
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
    AND DATE(_PARTITIONTIME) > "2015-12-31"
  GROUP BY
    year,
    lat,
    lon,
    best_engine_power_kw,
    best_vessel_class)
SELECT
  year,
  best_vessel_class,
  lat,
  lon,
  distance,
  wdpa_pid,
  fishing_hours
FROM
  gridded_effort_by_gear_and_year
LEFT JOIN `emlab-gcp.ocean_halos_v2.dist_to_lsmpa` USING(lat, lon)
WHERE distance IS NOT NULL