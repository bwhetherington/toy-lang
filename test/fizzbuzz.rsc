fn test(n) {
  if n % 15 == 0 {
    println("fizzbuzz");
  } else if n % 5 == 0 {
    println("buzz");
  } else if n % 3 == 0 {
    println("fizz");
  } else {
    println(n);
  }
}

fn fizzbuzz(to) {
  iter.Range.new(0, to).for_each(test);
}

fizzbuzz(100);