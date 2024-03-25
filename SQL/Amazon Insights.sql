Code for the mban_a32_aws_services - Gabriel Chouraqui - DD-MBAN

--------------------------------------------

-- Number of each type of action

SELECT
    SUM(CASE WHEN action_name LIKE '%fail%' THEN 1 ELSE 0 END) AS total_fail_users,
    SUM(CASE WHEN action_name LIKE '%success%' THEN 1 ELSE 0 END) AS total_success_users,
    SUM(CASE WHEN action_name LIKE '%landing%' THEN 1 ELSE 0 END) AS total_landing_users
FROM checkout_actions;

--------------------------------------------

-- Monthly data

SELECT 
    CONCAT(MONTHNAME(action_date), ' ', YEAR(action_date)) AS month_sequence_year,
    COUNT(*) AS total_actions,
    SUM(CASE WHEN action_name LIKE '%success%' THEN 1 ELSE 0 END) AS successful_purchases,
    SUM(CASE WHEN action_name LIKE '%land%' THEN 1 ELSE 0 END) AS landing_page,
    SUM(CASE WHEN error_message IS NOT NULL THEN 1 ELSE 0 END) AS total_errors
FROM checkout_actions
GROUP BY MONTH(action_date), YEAR(action_date)
ORDER BY YEAR(action_date), MONTH(action_date);

--------------------------------------------

-- Which error message appears the most

  SELECT 
    ca.error_message, 
    COUNT(error_message) AS total_message
FROM checkout_actions AS ca
WHERE ca.action_name LIKE '%fail%'
  AND ca.error_message IS NOT NULL
GROUP BY error_message
ORDER BY total_message DESC;

--------------------------------------------

-- Total number of error gotten per device

  SELECT 
    ca.device, 
    COUNT(device) AS total_device
FROM checkout_actions AS ca
  WHERE ca.error_message IS NOT NULL
GROUP BY ca.device
ORDER BY total_device DESC;

--------------------------------------------

-- Monthly analysis of January 2023 (last month provided)

SELECT 
    DAY(action_date) AS day_of_month,
    COUNT(*) AS total_actions,
    SUM(CASE WHEN action_name LIKE '%success%' THEN 1 ELSE 0 END) AS successful_purchases,
    SUM(CASE WHEN action_name LIKE '%land%' THEN 1 ELSE 0 END) AS landing_page,
    SUM(CASE WHEN error_message IS NOT NULL THEN 1 ELSE 0 END) AS total_errors
FROM checkout_actions
WHERE action_date >= '2023-01-01' AND action_date < '2023-02-01'
GROUP BY DAY(action_date)
ORDER BY DAY(action_date);

