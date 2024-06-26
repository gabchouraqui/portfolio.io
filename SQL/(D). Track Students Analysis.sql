Code for the sql_and_tableau analysis - Gabriel Chouraqui DD-MBAN

-------------------------------------------------------------------------------------------------------------------

/* Main Table */

SELECT 
    ROW_NUMBER() OVER (ORDER BY student_id, track_name DESC) AS `student_track_id`,
    student_id AS `student_id`,
    track_name AS `track_name`,
    date_enrolled AS `date_enrolled`,
    CASE
        WHEN date_completed IS NOT NULL THEN 'YES'
        ELSE 'NO'
    END AS `track_completed`,
    DATEDIFF(IFNULL(date_completed, CURDATE()), date_enrolled) AS `days_for_completion`,
    CASE
        WHEN DATEDIFF(IFNULL(date_completed, CURDATE()), date_enrolled) = 0 THEN 'Same day'
        WHEN DATEDIFF(IFNULL(date_completed, CURDATE()), date_enrolled) BETWEEN 1 AND 7 THEN '1 to 7 days'
        WHEN DATEDIFF(IFNULL(date_completed, CURDATE()), date_enrolled) BETWEEN 8 AND 30 THEN '8 to 30 days'
        WHEN DATEDIFF(IFNULL(date_completed, CURDATE()), date_enrolled) BETWEEN 31 AND 60 THEN '31 to 60 days'
        WHEN DATEDIFF(IFNULL(date_completed, CURDATE()), date_enrolled) BETWEEN 61 AND 90 THEN '61 to 90 days'
        WHEN DATEDIFF(IFNULL(date_completed, CURDATE()), date_enrolled) BETWEEN 91 AND 365 THEN '91 to 365 days'
        ELSE '366+ days'
    END AS `completion_bucket`
FROM 
    career_track_student_enrollments cts
JOIN 
    career_track_info cti ON cts.track_id = cti.track_id;

-------------------------------------------------------------------------------------------------------------------

/* What is the number of enrolled students monthly? 
Which is the month with the most enrollments? Speculate about the reason for the increased numbers. */

SELECT
    YEAR(date_enrolled) AS `enrollment_year`,
    MONTH(date_enrolled) AS `enrollment_month`,
    COUNT(DISTINCT student_id) AS `enrolled_students`
FROM
    career_track_student_enrollments
GROUP BY
    `enrollment_year`, `enrollment_month`
ORDER BY
    `enrollment_month` ASC;

-------------------------------------------------------------------------------------------------------------------

/* Which career track do students enroll most in? */
SELECT
    cti.track_name AS `track_name`,
    COUNT(cts.student_id) AS `enroll_count`
FROM
    career_track_student_enrollments cts
INNER JOIN
    career_track_info cti ON cts.track_id = cti.track_id
GROUP BY
    `track_name`
ORDER BY
    `enroll_count` DESC
LIMIT 1;

-------------------------------------------------------------------------------------------------------------------

/* What is the career track completion rate? 
Can you say if it’s increasing, decreasing, or staying constant with time? */

SELECT
    YEAR(date_enrolled) AS `completion_year`,
    MONTH(date_enrolled) AS `completion_month`,
    SUM(CASE WHEN date_completed IS NOT NULL THEN 1 ELSE 0 END) AS `completed_tracks`,
    COUNT(*) AS `total_tracks_enrolled`,
    SUM(CASE WHEN date_completed IS NOT NULL THEN 1 ELSE 0 END) / COUNT(*) * 100 AS `completion_rate`
FROM
    career_track_student_enrollments
GROUP BY
    `completion_year`, `completion_month`
ORDER BY
    `completion_month` ASC;

-------------------------------------------------------------------------------------------------------------------

/* How long does it typically take students to complete a career track? 
What type of subscription is most suitable for students who aim to complete a career track: monthly, quarterly, or annual? */ 

SELECT
    track_name,
    AVG(days_for_completion) AS avg_days_for_completion
FROM
    (SELECT 
        cti.track_name,
        DATEDIFF(IFNULL(cts.date_completed, CURDATE()), cts.date_enrolled) AS days_for_completion
    FROM 
        career_track_student_enrollments cts
    JOIN 
        career_track_info cti ON cts.track_id = cti.track_id) AS subquery
GROUP BY
    track_name;
