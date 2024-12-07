CREATE MACRO list_all(list) AS list_reduce(list, (x, c) -> x AND c);

CREATE MACRO is_safe(report_input) AS (
    WITH safety_tests AS (
        SELECT
            array_pop_back(list_zip(report_, array_pop_front(report_))) AS pairs,
            list_all(list_transform(pairs, x -> x[1] < x[2])) AS all_ascending,
            list_all(list_transform(pairs, x -> x[1] > x[2])) AS all_descending,
            list_transform(pairs, x -> ABS(x[1] - x[2])) AS differences,
            list_max(differences) <= 3 AND list_min(differences) >= 1 AS close_enough,
            (all_ascending OR all_descending) AND close_enough AS is_safe
        -- Since this is a macro, we need to avoid name conflicts...
        FROM (SELECT report_input AS report_)
    )
    SELECT is_safe FROM safety_tests
);

WITH input AS (
    SELECT string_split(line, ' ')::INTEGER[] as report
    FROM read_csv('input/02.txt', columns={'line': 'VARCHAR'})
),
part_1 AS (
    SELECT COUNT(report) AS solution
    FROM input
    WHERE is_safe(report)
),
part_2_unnested AS (
    SELECT
        report,
        unnest([report] || list_transform(report, (x, i) -> report[0:i - 1] || report[i + 1:])) AS possible_report,
    FROM input
),
part_2 AS (
    SELECT COUNT(1) AS solution
    FROM (
        SELECT 1
        FROM part_2_unnested
        GROUP BY report
        HAVING BOOL_OR(is_safe(possible_report))
    )
)
SELECT 'Part 1' AS part, solution FROM part_1
UNION ALL
SELECT 'Part 2' AS part, solution FROM part_2
