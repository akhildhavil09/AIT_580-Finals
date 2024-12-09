use final_project;
SELECT 
    COUNT(*) AS total_arrests,
    COUNT(DISTINCT ARREST_BORO) AS unique_boroughs,
    COUNT(DISTINCT PERP_RACE) AS unique_races,
    COUNT(DISTINCT AGE_GROUP) AS unique_age_groups
FROM ArrestData;
-- 1. Arrests by Borough
SELECT 
    ARREST_BORO, 
    COUNT(*) AS arrest_count,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM ArrestData), 2) AS percentage
FROM ArrestData
GROUP BY ARREST_BORO
ORDER BY arrest_count DESC;

-- 2. Top 10 Offenses
SELECT 
    OFNS_DESC, 
    COUNT(*) AS offense_count,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM ArrestData), 2) AS percentage
FROM ArrestData
GROUP BY OFNS_DESC
ORDER BY offense_count DESC
LIMIT 10;

-- 3. Arrests by Race, Sex, and Age Group
SELECT 
    PERP_RACE, 
    PERP_SEX,
    AGE_GROUP,
    COUNT(*) AS arrest_count,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM ArrestData), 2) AS percentage
FROM ArrestData
GROUP BY PERP_RACE, PERP_SEX, AGE_GROUP
ORDER BY arrest_count DESC;

-- 4. Monthly Arrests
SELECT 
    SUBSTR(ARREST_DATE, 4, 2) AS month,
    COUNT(*) AS monthly_arrests,
    ROUND(COUNT(*) * 100.0 / (SELECT COUNT(*) FROM ArrestData), 2) AS percentage
FROM ArrestData
GROUP BY month
ORDER BY monthly_arrests DESC;

-- 5. Arrests by Precinct
SELECT 
    ARREST_PRECINCT,
    COUNT(*) AS precinct_arrests,
    AVG(Latitude) AS avg_latitude,
    AVG(Longitude) AS avg_longitude
FROM ArrestData
GROUP BY ARREST_PRECINCT
ORDER BY precinct_arrests DESC
LIMIT 10;


