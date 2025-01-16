WITH input AS (
    SELECT ROW_NUMBER() OVER() as row_number, line
    FROM read_csv('input/05.txt', columns={'line': 'VARCHAR'})
),
orderings AS (
    SELECT
        string_split(line, '|')[1] AS before,
        string_split(line, '|')[2] AS after,
        before || ',.*' || after AS regex
    FROM input
    WHERE row_number < (SELECT row_number FROM input WHERE line IS NULL)
),
updates AS (
    SELECT
        line,
        string_split(line, ',') AS pages,
        row_number
    FROM input
    WHERE row_number > (SELECT row_number FROM input WHERE line IS NULL)
),
updates_unnested AS (
    SELECT
        unnest(pages) AS page,
        line,
        row_number,
    FROM updates
),
checks AS (
    SELECT
        line,
        row_number,
        (NOT contains(line, after)) OR regexp_matches(line, regex) AS ordering_correct
    FROM updates_unnested u
    JOIN orderings o ON u.page = o.before
    ORDER BY row_number
),
correct_lines AS (
    SELECT line
    FROM checks
    GROUP BY line, row_number
    HAVING bool_and(ordering_correct)
    ORDER BY row_number
),
middles AS (
    SELECT
        string_split(line, ',') AS pages,
        pages[(len(pages) // 2) + 1]::INT AS middle
    FROM correct_lines
)
SELECT SUM(middle) FROM middles;
