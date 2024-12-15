CREATE MACRO pad_left(list_, n_) AS [0 FOR x IN range(0, n_)] || list_;

CREATE MACRO corners(pos_) AS
    [(pos_[1]::INTEGER - 1, pos_[2]::INTEGER - 1),
     (pos_[1]::INTEGER + 1, pos_[2]::INTEGER + 1),
     (pos_[1]::INTEGER - 1, pos_[2]::INTEGER + 1),
     (pos_[1]::INTEGER + 1, pos_[2]::INTEGER - 1)];

WITH input AS (
    SELECT array_agg(string_split(line, '')) AS grid
    FROM read_csv('input/04.txt', columns={'line': 'VARCHAR'})
),
grid AS (
    SELECT
        unnest(
            flatten(
                list_transform(
                    grid,
                    (r, y) -> list_transform(
                        r,
                        (c, x) -> {'x': x, 'y': y, 'c': c}
                    )
                )
            )
        ) AS cell,
        cell['x'] AS x,
        cell['y'] AS y,
        cell['c'] AS c
    FROM input
),
indices AS (
    SELECT
        MAX(y) AS w,
        MAX(x) AS h,
        list_transform(range(1, w + 1), x -> range(x, w + 1))
        || list_transform(range(1, w), x -> pad_left(range(1, w + 1), x)[0:w]) AS diagonal_indices,
        list_transform(
                diagonal_indices,
                (idxs) -> list_transform(
                    idxs,
                    (idx, row_num) -> (row_num, idx))) AS diagonals_ltr,
        list_transform(
                diagonal_indices,
                (idxs) -> list_transform(
                    idxs,
                    (idx, row_num) -> (w - row_num + 1, idx))) AS diagonals_rtl,
        list_transform(
            range(1, h + 1),
            (x) -> list_transform(
                range(1, w + 1),
                (y) -> (x, y)
            )
        ) AS verticals,
        list_transform(
            range(1, h + 1),
            (x) -> list_transform(
                range(1, w + 1),
                (y) -> (y, x)
            )
        ) AS horizontals,
        diagonals_ltr || diagonals_rtl || verticals || horizontals AS all_indices
    FROM grid
),
string_positions AS (
    SELECT
        unnest(all_indices) AS string_positions,
        generate_subscripts(all_indices, 1) AS index
    FROM indices
),
string_positions_unnested AS (
    SELECT
        unnest(string_positions) AS pos,
        generate_subscripts(string_positions, 1) AS sort_order,
        index
    FROM string_positions
),
strings AS (
    SELECT string_agg(c, '' ORDER BY sort_order) AS str
    FROM string_positions_unnested
    JOIN grid ON pos[1] = x AND pos[2] = y
    GROUP BY index
),
part_1 AS (
    SELECT
        SUM(len(regexp_extract_all(str, 'XMAS')))
        + SUM(len(regexp_extract_all(str, 'SAMX'))) AS solution
    FROM strings
),
as_with_corners AS (
    SELECT
        (x, y) AS a_position,
        unnest(corners(a_position)) AS corner,
        corner[1] AS corner_x,
        corner[2] AS corner_y
    FROM grid
    WHERE c = 'A'
),
corner_chars AS (
    SELECT
        a_position,
        corner_x,
        corner_y,
        c,
    FROM as_with_corners
    JOIN grid ON corner_x = grid.x AND corner_y = grid.y
),
x_masses AS (
    SELECT
        a_position,
        array_agg(c ORDER BY corner_x, corner_y) AS corners,
        ((corners[1] = 'M' AND corners[4] = 'S') OR (corners[1] = 'S' AND corners[4] = 'M'))
            AND
        ((corners[2] = 'M' AND corners[3] = 'S') OR (corners[2] = 'S' AND corners[3] = 'M')) AS is_x_mas
    FROM corner_chars
    GROUP BY a_position
),
part_2 AS (
    SELECT count(*) AS solution
    FROM x_masses
    WHERE is_x_mas
)
SELECT 'Part 1' AS part, solution FROM part_1
UNION ALL
SELECT 'Part 2' AS part, solution FROM part_2
