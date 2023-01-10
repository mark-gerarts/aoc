<?php

function parse_input(string $input): array {
    return array_map('intval', preg_split('/\s+/', $input));
}

function redistribute(array $memoryBank): array {
    $length = count($memoryBank);
    $highestValue = max($memoryBank);
    $startingPosition = array_search($highestValue, $memoryBank);
    $memoryBank[$startingPosition] = 0;
    $distributionValue = floor($highestValue / ($length - 1)) ?: 1;

    for ($i = $startingPosition + 1; $highestValue > 0; $i++) {
        $i %= $length;
        $memoryBank[$i] += min([$distributionValue, $highestValue]);
        $highestValue -= $distributionValue;
    }

    return $memoryBank;
}

function solve(string $input) {
    $memoryBank = parse_input($input);
    $previousStates = [];
    $steps = 0;
    $loopDetected = false;
    $continue = true;

    while ($continue) {
        $previousStates[] = $memoryBank;
        $memoryBank = redistribute($memoryBank);
        $steps++;

        $continue = !in_array($memoryBank, $previousStates);
        if (!$loopDetected && !$continue) {
            $loopDetected = true;
            $continue = true;
            $previousStates = [];
            $steps = 0;
        }
    }

    return $steps;
}
