function parse_input(filename)
    raw_ranges = read(filename, String) |> x -> split(x, ",")
    to_id_range(raw_range) = split(raw_range, "-") .|> r -> parse(Int, r)
    unpack_range(r) = string.(r[1]:r[2])

    Iterators.flatmap(unpack_range, to_id_range.(raw_ranges))
end

function is_invalid_part1(id::String)
    iseven(length(id)) || return false

    halfway = length(id) ÷ 2
    id[1:halfway] == id[(halfway+1):end]
end

function is_invalid_part2(id::String)
    for num_chunks in (length(id)÷2):-1:1
        chunks = collect(Iterators.partition(collect(id), num_chunks))
        length(unique(chunks)) == 1 && return true
    end

    return false
end

function solve(ids, is_invalid)
    sum(parse(Int, id) for id in ids if is_invalid(id))
end

"""
Just to get an idea of Julia's threading, let's try to parallelize the workload.
"""
function solve_multithreaded(ids, is_invalid)
    ids = collect(ids)
    results = [0 for _ in 1:Threads.maxthreadid()]

    Threads.@threads for i in eachindex(ids)
        id = ids[i]
        if is_invalid(id)
            results[Threads.threadid()] += parse(Int, id)
        end
    end

    return sum(results)
end

all_ids = parse_input("input/02.txt")

println("Part 1: $(solve(all_ids, is_invalid_part1))")
println("Part 2: $(solve(all_ids, is_invalid_part2))")
