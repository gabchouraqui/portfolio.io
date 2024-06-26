-- Question 1

SELECT operating_airline,
	COUNT(*) AS total_flights
FROM "opt_gabriel"
WHERE operating_airline IS NOT NULL
GROUP BY operating_airline
ORDER BY total_flights DESC;



-- Question 2

WITH average_delay AS (
    SELECT
        CASE 
            WHEN deptime BETWEEN 0000 AND 0600 THEN 'Night'
            WHEN deptime BETWEEN 0601 AND 1200 THEN 'Morning'
            WHEN deptime BETWEEN 1201 AND 1800 THEN 'Afternoon'
            WHEN deptime BETWEEN 1801 AND 2400 THEN 'Evening'
            ELSE 'Unknown'
        END AS time_slot, dayofweek,
        AVG(depdelay) AS Avg_delay
    FROM "opt_gabriel" WHERE deptime IS NOT NULL
    GROUP BY 
        CASE 
            WHEN deptime BETWEEN 0000 AND 0600 THEN 'Night'
            WHEN deptime BETWEEN 0601 AND 1200 THEN 'Morning'
            WHEN deptime BETWEEN 1201 AND 1800 THEN 'Afternoon'
            WHEN deptime BETWEEN 1801 AND 2400 THEN 'Evening'
            ELSE 'Unknown'
        END, dayofweek
)
SELECT dayofweek, time_slot, ROUND(AVG(Avg_delay),2) AS Overall_Avg_delay
FROM average_delay
GROUP BY dayofweek, time_slot
ORDER BY dayofweek, time_slot;


-- Question 4

WITH flight_route AS (
  SELECT
    CASE
      WHEN CONCAT(origin, ' - ', dest) IS NOT NULL THEN CONCAT(origin, ' - ', dest)
    END AS flightroute,
    CASE
      WHEN dayofmonth BETWEEN 1 AND 7 THEN '1'
      WHEN dayofmonth BETWEEN 8 AND 14 THEN '2'
      WHEN dayofmonth BETWEEN 15 AND 21 THEN '3'
      WHEN dayofmonth BETWEEN 22 AND 28 THEN '4'
      WHEN dayofmonth BETWEEN 29 AND 31 THEN '5'
    END AS week_category,
    dayofmonth
  FROM
    "opt_gabriel"
)
SELECT
  flightroute,
  week_category,
  ROUND(AVG(total_flights),0) AS average_flights
FROM (
  SELECT
    flightroute,
    week_category,
    COUNT(*) AS total_flights
  FROM
    flight_route
  WHERE
    week_category IS NOT NULL
  GROUP BY
    flightroute,
    week_category,
    dayofmonth
) grouped
GROUP BY
  flightroute,
  week_category
ORDER BY
  flightroute,
  week_category;



-- Question 5

WITH weather_delay_stats AS (
  SELECT
    Origin AS Airport,
    COUNT(CASE WHEN WeatherDelay > 0 THEN 1 END) AS Flights_Delayed_By_Weather,
    COUNT(*) AS Total_Flights
  FROM "opt_gabriel"
  GROUP BY Origin
)
SELECT
  Airport,
  Flights_Delayed_By_Weather,
  Total_Flights,
  ROUND((Flights_Delayed_By_Weather * 100.0 / NULLIF(Total_Flights, 0)),2) AS Percentage_Delayed_By_Weather
FROM weather_delay_stats
ORDER BY Flights_Delayed_By_Weather DESC;
