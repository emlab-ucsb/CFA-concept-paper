################################################################################
#
# This query produces a table of gridded radiance (nW/cm2/sr) and their
# distance to LSMPA borders.
# 
# We first bin lat and long (0.1 degree res) and aggregate by lat, lon and year
# summing and averaging across all cells.
#
# We then call the distance to lsmpa table and left join by lat / long.
################################################################################
WITH
  radiance AS (
  SELECT
    EXTRACT(YEAR
    FROM
      Date_Mscan) AS year,
    CAST(FLOOR(Lat_DNB / 0.1) * 0.1 AS NUMERIC) + 0.05 AS lat,
    CAST(FLOOR(Lon_DNB/ 0.1) * 0.1 AS NUMERIC) + 0.05 AS lon,
    SUM(Rad_DNB) AS sum_rad,
    AVG(Rad_DNB) AS avg_rad
  FROM
    `world-fishing-827.pipe_viirs_production_v20180723.raw_vbd_redacted`
  WHERE
    DATE(_PARTITIONTIME) > "2015-12-31"
  GROUP BY
    year,
    lat,
    lon)
  #
  #
  #
  #
  ########
SELECT
  year,
  lat,
  lon,
  sum_rad,
  avg_rad,
  distance,
  wdpa_pid_num
FROM
  radiance
LEFT JOIN
  `emlab-gcp.ocean_halos_v2.dist_to_lsmpa`
USING
  (lat,
    lon)
WHERE distance IS NOT NULL
ORDER BY year, lat, lon