<?php
function fibonacci_recursive($n) {
    if ($n <= 1) {
        return 1;
    }
    return fibonacci_recursive($n - 1) + fibonacci_recursive($n - 2);
}

echo fibonacci_recursive(20);
