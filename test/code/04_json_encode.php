<?php

echo "Array sequential" . PHP_EOL;
$sequential = ["foo", "bar", "baz", "blong"];
var_dump(
    $sequential,
    json_encode($sequential)
);

echo PHP_EOL . "Array no secuential" . PHP_EOL;
$nonsequential = [1 => "foo", 2 => "bar", 3 => "baz", 4 => "blong"];
var_dump(
    $nonsequential,
    json_encode($nonsequential)
);

echo PHP_EOL . "Array secuential with destroyed key" . PHP_EOL;
unset($sequential[1]);
var_dump(
    $sequential,
    json_encode($sequential)
);
