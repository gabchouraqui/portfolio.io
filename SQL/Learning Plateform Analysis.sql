Code for the mban_a33 - Gabriel Chouraqui - DD-MBAN

--------------------------------------------

-- Q1: Which courses are the most watched by students, and how are they rated?

WITH Course_ratings AS (
    SELECT
        ci.course_id,
        ci.course_title,
        AVG(cr.course_rating) AS avg_rating
    FROM
        course_ratings AS cr
    JOIN
        course_info AS ci USING (course_id)
    GROUP BY
        ci.course_id, ci.course_title
    ORDER BY
        avg_rating DESC
)

SELECT
    sl.course_id,
    cr.course_title AS course_title,
    ROUND(SUM(sl.minutes_watched), 2) AS minutes_watched,
    cr.avg_rating
FROM
    student_learning AS sl
JOIN
    Course_ratings AS cr USING (course_id)
GROUP BY
    cr.course_id, cr.course_title, cr.avg_rating
ORDER BY
    minutes_watched DESC;

--------------------------------------------

-- Q2: How many students register each month, and what fraction are also onboarded?

SELECT
    MONTH(si.date_registered) AS registration_month,
    COUNT(DISTINCT si.student_id) AS total_students_registered,
    COUNT(DISTINCT sl.student_id) AS onboarded_students,
    COUNT(DISTINCT sl.student_id) / COUNT(DISTINCT si.student_id) AS onboarded_fraction
FROM
    student_info si
LEFT JOIN
    student_learning sl ON si.student_id = sl.student_id
GROUP BY
    registration_month
ORDER BY
    registration_month;

--------------------------------------------

-- Q3: How do students engage with the online platform (minutes and average minutes watched) based on type (free-plan or paying)?

SELECT
    CASE
        WHEN sp.purchase_type IS NULL THEN 'Free-plan'
        ELSE 'Paying'
    END AS purchase_category,
    ROUND(COUNT(DISTINCT si.student_id),2) AS total_students,
    ROUND(SUM(sl.minutes_watched),2) AS total_minutes_watched,
    ROUND(AVG(sl.minutes_watched),2) AS average_minutes_watched
FROM
    student_info si
LEFT JOIN
    student_learning sl ON si.student_id = sl.student_id
LEFT JOIN
    student_purchases sp ON si.student_id = sp.student_id
GROUP BY
    purchase_category
ORDER BY
    average_minutes_watched DESC;

--------------------------------------------

-- Q4: Do students watch more content with time, and does it vary seasonally?

WITH PeriodWatched AS (
    SELECT
        date_watched as watch_date,
        ROUND(SUM(sl.minutes_watched),2) AS total_minutes_watched
    FROM student_learning sl
    GROUP BY watch_date
)
SELECT
    pw.watch_date,
    pw.total_minutes_watched
FROM PeriodWatched pw
ORDER BY pw.watch_date;

--------------------------------------------

-- Q5: Which countries have the most students registered, and does this number scale proportionally with the number of minutes watched per country?

WITH CountryRegistrations AS (
    SELECT
        si.student_country,
        COUNT(si.student_id) AS total_students_registered
    FROM student_info si
    GROUP BY si.student_country
),
CountryWatchedMinutes AS (
    SELECT
        si.student_country,
        ROUND(SUM(sl.minutes_watched),1) AS total_minutes_watched
    FROM student_info si
    JOIN student_learning sl ON si.student_id = sl.student_id
    GROUP BY si.student_country
)
SELECT
    cr.student_country,
    cr.total_students_registered,
    COALESCE(cwm.total_minutes_watched, 0) AS total_minutes_watched
FROM CountryRegistrations cr
LEFT JOIN CountryWatchedMinutes cwm ON cr.student_country = cwm.student_country
ORDER BY cr.total_students_registered DESC;

--------------------------------------------

-- Q5: Which countries have the most students registered, and does this number scale proportionally with the number of minutes watched per country? - TOP 5 COUNTRIES

WITH CountryRegistrations AS (
    SELECT
        si.student_country,
        COUNT(si.student_id) AS total_students_registered
    FROM student_info si
    GROUP BY si.student_country
),
CountryWatchedMinutes AS (
    SELECT
        si.student_country,
        ROUND(SUM(sl.minutes_watched),1) AS total_minutes_watched
    FROM student_info si
    JOIN student_learning sl ON si.student_id = sl.student_id
    GROUP BY si.student_country
)
SELECT
    cr.student_country,
    cr.total_students_registered,
    COALESCE(cwm.total_minutes_watched, 0) AS total_minutes_watched
FROM CountryRegistrations cr
LEFT JOIN CountryWatchedMinutes cwm ON cr.student_country = cwm.student_country
ORDER BY total_minutes_watched DESC
LIMIT 5;

