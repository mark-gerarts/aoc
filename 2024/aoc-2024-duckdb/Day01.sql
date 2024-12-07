WITH input AS (
    SELECT string_split(line, '   ') as split
    FROM read_csv('input/01.txt', columns={'line': 'VARCHAR'})
),
l AS (
    SELECT
        split[1]::INTEGER AS l,
        ROW_NUMBER() OVER (ORDER BY split[1]) AS n,
    FROM input
),
r AS (
    SELECT
        split[2]::INTEGER AS r,
        ROW_NUMBER() OVER (ORDER BY split[2]) AS n,
    FROM input
),
lists AS (
    SELECT l.l, r.r
    FROM l JOIN r ON l.n = r.n
),
part_1 AS (
    SELECT SUM(ABS(l - r)) AS solution
    FROM lists
),
part_2 AS (
    SELECT SUM((SELECT COUNT(r) * l FROM r WHERE r = l)) AS solution
    FROM l
)
SELECT 'Part 1' AS part, solution FROM part_1
UNION ALL
SELECT 'Part 2' AS part, solution FROM part_2
