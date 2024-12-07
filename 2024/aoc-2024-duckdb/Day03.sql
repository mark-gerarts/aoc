CREATE MACRO sum_mults(mult_string) AS (
    WITH mults AS (
        SELECT
            unnest(regexp_extract_all(mult_string, 'mul\((\d{1,3}),(\d{1,3})\)', 1)::INTEGER[]) AS l,
            unnest(regexp_extract_all(mult_string, 'mul\((\d{1,3}),(\d{1,3})\)', 2)::INTEGER[]) AS r,
            l * r AS mult
    ) SELECT SUM(mult) FROM mults
);

WITH input AS (
    SELECT 'do()' || string_agg(line, '') AS line
    FROM read_csv('input/03.txt', columns={'line': 'VARCHAR'})
),
part_1 AS (
    SELECT sum_mults(line) AS solution FROM input
),
do_parts AS (
    SELECT regexp_split_to_table(line, 'do\(\)') AS do_string
    FROM input
),
drop_donts AS (
    SELECT regexp_split_to_array(do_string, E'don\'t\\(\\)')[1] AS do_string
    FROM do_parts
),
part_2 AS (
    SELECT SUM(sum_mults(do_string)) AS solution FROM drop_donts
)
SELECT 'Part 1' AS part, solution FROM part_1
UNION ALL
SELECT 'Part 2' AS part, solution FROM part_2
